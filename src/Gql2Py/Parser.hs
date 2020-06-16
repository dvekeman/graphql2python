{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Gql2Py.Parser
  ( schemaDocument
  , parseSchemaDoc

  , value
  , parseValueConst
  , nameParser

  , graphQLType
  , parseGraphQLType

  , Parser
  , runParser
  ) where

import           Protolude                     hiding (option)

import           Control.Monad.Fail            (fail)
import           Data.Aeson.Parser             (jstring)
import qualified Data.Attoparsec.ByteString    as A
import           Data.Attoparsec.Text          (Parser, anyChar, char, many1,
                                                match, option, scan, scientific,
                                                sepBy1, (<?>))
import qualified Data.Attoparsec.Text          as AT
import           Data.Char                     (isAsciiLower, isAsciiUpper,
                                                isDigit)
import           Data.Scientific               (Scientific)
import           Data.Text                     (find)

import qualified Gql2Py.Syntax as AST

runParser :: AT.Parser a -> Text -> Either Text a
runParser parser t =
  either (Left . toS) return $ AT.parseOnly (parser <* AT.endOfInput) t

schemaDocument :: Parser AST.SchemaDocument
schemaDocument =
  whiteSpace *> (AST.SchemaDocument <$> many1 typeDefinition)
  <?> "type document error"

parseSchemaDoc :: Text -> Either Text AST.SchemaDocument
parseSchemaDoc = runParser schemaDocument

graphQLType :: Parser AST.GType
graphQLType =
    (flip AST.TypeList <$> listType <*> nullability)
    <|> (flip AST.TypeNamed <$> namedType <*> nullability)
    <?> "graphQLType error!"

parseGraphQLType :: Text -> Either Text AST.GType
parseGraphQLType = runParser graphQLType

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> nameParser

listType :: Parser AST.ListType
listType = AST.ListType <$> brackets graphQLType

nullability :: Parser AST.Nullability
nullability =
  (tok "!" $> AST.Nullability False)
  <|> pure (AST.Nullability True)

typeDefinition :: Parser AST.TypeDefinition
typeDefinition =
      AST.DefinitionSchema            <$> schemaDefinition
  <|> AST.TypeDefinitionObject        <$> objectTypeDefinition
  <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition
  <|> AST.TypeDefinitionUnion         <$> unionTypeDefinition
  <|> AST.TypeDefinitionScalar        <$> scalarTypeDefinition
  <|> AST.TypeDefinitionEnum          <$> enumTypeDefinition
  <|> AST.TypeDefinitionInputObject   <$> inputObjectTypeDefinition
  <?> "typeDefinition error!"

optDesc :: Parser (Maybe AST.Description)
optDesc = optional (AST.Description . AST.unStringValue <$> stringValue)

stringValue :: Parser AST.StringValue
stringValue = do
  parsed <- char '"' *> jstring_
  case unescapeText parsed of
    Left err      -> fail err
    Right escaped -> pure (AST.StringValue escaped)
  where
    jstring_ :: Parser Text
    jstring_ = scan startState go <* anyChar

    startState = False
    go a c
      | a = Just False
      | c == '"' = Nothing
      | otherwise = let a' = c == backslash
                    in Just a'
      where backslash = '\\'

    unescapeText str = A.parseOnly jstring ("\"" <> encodeUtf8 str <> "\"")

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition = AST.ObjectTypeDefinition
  <$> optDesc
  <*  tok "type"
  <*> nameParser
  <*> optempty interfaces
  <*> optempty directives
  <*> fieldDefinitions

schemaDefinition :: Parser AST.SchemaDefinition
schemaDefinition = AST.SchemaDefinition
  <$> optDesc
  <*  tok "schema"
  <*> optempty directives
  <*> fieldDefinitions

directives :: Parser [AST.Directive]
directives = many1 directive

directive :: Parser AST.Directive
directive = AST.Directive
  <$  tok "@"
  <*> nameParser
  <*> optempty arguments

arguments :: Parser [AST.Argument]
arguments = parens $ many1 argument

argument :: Parser AST.Argument
argument = AST.Argument <$> nameParser <* tok ":" <*> value

number :: Parser (Either Scientific Integer)
number = do
  (numText, num) <- match (tok scientific)
  pure $ case Data.Text.find (\c -> c == '.' || c == 'e' || c == 'E') numText of
    Just _ -> Left num
    Nothing -> Right (floor num)

value :: Parser AST.Value
value = tok (
      AST.VVariable <$> (variable <?> "variable")
  <|> (fmap (either AST.VFloat AST.VInt) number <?> "number")
  <|> AST.VNull     <$  literal "null"
  <|> AST.VBoolean  <$> (booleanValue <?> "booleanValue")
  <|> AST.VString   <$> (stringValue <?> "stringValue")
  <|> AST.VEnum     <$> (fmap AST.EnumValue nameParser <?> "name")
  <|> AST.VList     <$> (listValue <?> "listValue")
  <|> AST.VObject   <$> (objectValue <?> "objectValue")
  <?> "value error!"
  )

booleanValue :: Parser Bool
booleanValue
  =   True  <$ literal "true"
  <|> False <$ literal "false"

listValueC :: Parser AST.ListValueC
listValueC = listValueG valueConst

listValueG :: Parser a -> Parser (AST.ListValueG a)
listValueG val = AST.ListValueG <$> brackets (many val)

listValue :: Parser AST.ListValue
listValue = listValueG value

objectValueC :: Parser AST.ObjectValueC
objectValueC = objectValueG valueConst

objectValueG :: Parser a -> Parser (AST.ObjectValueG a)
objectValueG p = AST.ObjectValueG <$> braces (many (objectFieldG p <?> "objectField"))

objectValue :: Parser AST.ObjectValue
objectValue = objectValueG value

objectFieldG :: Parser a -> Parser (AST.ObjectFieldG a)
objectFieldG p = AST.ObjectFieldG <$> nameParser <* tok ":" <*> p

variable :: Parser AST.Variable
variable = AST.Variable <$ tok "$" <*> nameParser



defaultValue :: Parser AST.DefaultValue
defaultValue = tok "=" *> valueConst

parseValueConst :: Text -> Either Text AST.ValueConst
parseValueConst = runParser valueConst

valueConst :: Parser AST.ValueConst
valueConst = tok (
      (fmap (either AST.VCFloat AST.VCInt) number <?> "number")
  <|> AST.VCNull     <$  literal "null"
  <|> AST.VCBoolean  <$> (booleanValue <?> "booleanValue")
  <|> AST.VCString   <$> (stringValue <?> "stringValue")
  <|> AST.VCEnum     <$> (fmap AST.EnumValue nameParser <?> "name")
  <|> AST.VCList     <$> (listValueC <?> "listValue")
  <|> AST.VCObject   <$> (objectValueC <?> "objectValue")
  <?> "value (const) error!"
  )

interfaces :: Parser [AST.NamedType]
interfaces = tok "implements" *> many1 namedType

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
  <$> optDesc
  <*> nameParser
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> graphQLType
  <*> optempty directives

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$> optDesc
  <*  tok "interface"
  <*> nameParser
  <*> optempty directives
  <*> fieldDefinitions

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$> optDesc
  <*  tok "union"
  <*> nameParser
  <*> optempty directives
  <*  tok "="
  <*> unionMembers

unionMembers :: Parser [AST.NamedType]
unionMembers = namedType `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
  <$> optDesc
  <*  tok "scalar"
  <*> nameParser
  <*> optempty directives

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$> optDesc
  <*  tok "enum"
  <*> nameParser
  <*> optempty directives
  <*> enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition
  <$> optDesc
  <*> enumValue
  <*> optempty directives

enumValue :: Parser AST.EnumValue
enumValue = AST.EnumValue <$> nameParser

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$> optDesc
  <*  tok "input"
  <*> nameParser
  <*> optempty directives
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> optDesc
  <*> nameParser
  <*  tok ":"
  <*> graphQLType
  <*> optional defaultValue

tok :: AT.Parser a -> AT.Parser a
tok p = p <* whiteSpace
{-# INLINE tok #-}

literal :: AT.Parser a -> AT.Parser a
literal p = p <* ends <* whiteSpace
{-# INLINE literal #-}

ends :: AT.Parser ()
ends = do
  mc <- AT.peekChar
  case mc of
    Nothing -> pure ()
    Just c  ->
      if isNonFirstChar c
         then mzero
         else pure ()

comment :: Parser ()
comment =
  AT.char '#' *>
  AT.skipWhile (\c -> c /= '\n' && c /= '\r' )
{-# INLINE comment #-}

isSpaceLike :: Char -> Bool
isSpaceLike c =
  c == '\t' || c == ' ' || c == '\n' || c == '\r' || c == ','
{-# INLINE isSpaceLike #-}

whiteSpace :: AT.Parser ()
whiteSpace = do
  AT.skipWhile isSpaceLike
  (comment *> whiteSpace) <|> pure ()

nameParser :: AT.Parser AST.Name
nameParser =
  AST.Name <$> tok ((<>) <$> AT.takeWhile1 isFirstChar
                         <*> AT.takeWhile isNonFirstChar)
{-# INLINE nameParser #-}

isFirstChar :: Char -> Bool
isFirstChar x = isAsciiLower x || isAsciiUpper x || x == '_'
{-# INLINE isFirstChar #-}

isNonFirstChar :: Char -> Bool
isNonFirstChar x = isFirstChar x || isDigit x
{-# INLINE isNonFirstChar #-}

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty
