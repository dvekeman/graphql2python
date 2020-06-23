{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Gql2Py.Printer where

import qualified Data.Text as Txt
import           Prelude                       (String)
import           Protolude
import           Data.Scientific               (Scientific)
import           Gql2Py.Syntax

import           Data.Text.Lazy                (Text)
import           Data.Text.Lazy.Builder

class (Monoid a, IsString a) => Printer a where
  stringP :: String -> a
  textP   :: Protolude.Text -> a
  charP   :: Char -> a
  intP    :: Integer -> a
  floatP  :: Scientific -> a

  {-# MINIMAL stringP, textP, charP, intP, floatP #-}

  nameP    :: Name -> a
  nameP    = textP . unName

--  nodeP :: TypedOperationDefinition -> a
--  nodeP = node

--  selectionSetP :: SelectionSet -> a
--  selectionSetP = selectionSet


-- | the pretty printer implementation

schemaDocument :: (Printer a) => SchemaDocument -> a
schemaDocument ed =
  stringP "from __future__ import annotations\n"
  <> stringP "import datetime\n"
  <> stringP "import decimal\n"
  <> stringP "from dataclasses import dataclass\n"
  <> stringP "from typing import Optional, Dict, List\n"
  <> stringP "from enum import Enum, unique\n"
  <> stringP "from uuid import UUID\n\n"
  <> (mconcat $ intersperse (stringP "") $ map typeDefinition $
  getTypeDefinitions ed)

typeDefinition :: (Printer a) => TypeDefinition -> a
typeDefinition = \case
  DefinitionSchema d -> schemaDefinition d
  TypeDefinitionObject d -> typeDefinitionObject d
  TypeDefinitionInterface  d -> typeDefinitionInterface d
  TypeDefinitionUnion  d -> typeDefinitionUnion d
  TypeDefinitionScalar  d -> typeDefinitionScalar d
  TypeDefinitionEnum  d -> typeDefinitionEnum d
  TypeDefinitionInputObject  d -> typeDefinitionInputObject d

typeDefinitionObject :: (Printer a) => ObjectTypeDefinition -> a
typeDefinitionObject (ObjectTypeDefinition _ name _ _ fields) =
  stringP "@dataclass(frozen=True)\nclass " <> nameP name <> stringP ":\n"
  <> optempty fields_f (sort fields)
  <> "    " <> stringP "\n\n"
  <> stringP "    def to_json(self):\n        res = {}\n"
  <> optempty tojson_fields_f fields
  <> stringP "\n        return res\n"
  <> stringP "        pass\n\n"
  <> stringP "    @classmethod\n"
  <> stringP "    def from_json(cls, d):\n"
  <> stringP "        return cls(\n"
  <> optempty fromjson_fields_f fields
  <> stringP "\n        )\n"
  <> stringP "        pass\n\n"

fields_quicksort :: Ord a => [a] -> [a]
fields_quicksort []     = []
fields_quicksort (p:xs) = (fields_quicksort lesser) ++ [p] ++ (fields_quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

tojson_fields_f :: (Printer a) => [FieldDefinition] -> a
tojson_fields_f = mconcat . intersperse (charP '\n') . map tojson_field_f

tojson_field_f :: (Printer a) =>  FieldDefinition -> a
tojson_field_f (FieldDefinition _ name _ gtype _) =
  stringP "        if self." <> nameP name <> stringP " is not None:\n" <> stringP "            res[\"" <> nameP name <> stringP "\"] = self." <> nameP name

fromjson_fields_f :: (Printer a) => [FieldDefinition] -> a
fromjson_fields_f = mconcat . intersperse (charP '\n') . map fromjson_field_f

fromjson_field_f :: (Printer a) =>  FieldDefinition -> a
fromjson_field_f (FieldDefinition _ name _ gtype _) =
  stringP "            " <> nameP name <>" = (\"" <> nameP name <> "\" in d) if d[\"" <> nameP name <> stringP "\"] else None,"


fields_f :: (Printer a) => [FieldDefinition] -> a
fields_f = mconcat . intersperse (charP '\n') . map field_f

field_f :: (Printer a) =>  FieldDefinition -> a
field_f (FieldDefinition _ name _ gtype _) =
  stringP "    " <> nameP name <> stringP ": " <> gtype_f gtype

gtype_f :: (Printer a) =>  GType -> a
gtype_f = \case
  TypeNamed n d -> nonNull_start n <> type_f d <> nonNull_end n
  TypeList n d -> nonNull_start n <> list_type_f d <> nonNull_end n

gtype_f1 :: (Printer a) =>  GType -> a
gtype_f1 = \case
  TypeNamed _ d -> type_f d
  TypeList _ d -> list_type_f d

nonNull_start :: (Printer a) => Nullability -> a
nonNull_start n = bool (stringP "Optional[") mempty $ not (unNullability n)

nonNull_end :: (Printer a) => Nullability -> a
nonNull_end n = bool (stringP "] = None") mempty $ not (unNullability n)

type_f :: (Printer a) => NamedType -> a
type_f (NamedType name) = name_f name

name_f :: (Printer a) => Name -> a
name_f (Name n) = case Txt.unpack n of
  "timestamptz" -> stringP "datetime.datetime"
  "String" -> stringP "str"
  "Int" -> stringP "int"
  "jsonb" -> stringP "Dict"
  "date" -> stringP "datetime.datetime"
  "Float" -> "float"
  "uuid" -> "UUID"
  "numeric" -> "decimal.Decimal"
  "Boolean" -> "bool"
  "bigint" -> "int"
  _ -> textP n

list_type_f :: (Printer a) => ListType -> a
list_type_f (ListType t) =
  stringP "List[" <> gtype_f1 t <> charP ']'


schemaDefinition :: (Printer a) => SchemaDefinition -> a
schemaDefinition _ = stringP ""

typeDefinitionInterface :: (Printer a) => InterfaceTypeDefinition -> a
typeDefinitionInterface _ = stringP ""

typeDefinitionUnion :: (Printer a) => UnionTypeDefinition -> a
typeDefinitionUnion _ = stringP ""

typeDefinitionScalar :: (Printer a) => ScalarTypeDefinition -> a
typeDefinitionScalar _ = stringP ""

typeDefinitionEnum :: (Printer a) => EnumTypeDefinition -> a
typeDefinitionEnum (EnumTypeDefinition _ name _ values) =
  stringP "@unique\n"
  <> stringP "class " <> nameP name <> stringP "(Enum):\n"
  <> enum_values_f values
  <> stringP "\n\n"

enum_values_f :: (Printer a) => [EnumValueDefinition] -> a
enum_values_f = mconcat . intersperse (charP '\n') . map enum_value_f

enum_value_f :: (Printer a) => EnumValueDefinition -> a
enum_value_f (EnumValueDefinition _ name _) =
  enum_value_name_f name

enum_value_name_f :: (Printer a) => EnumValue -> a
enum_value_name_f (EnumValue name) =
  stringP "    " <> nname_f name <> "= \"" <> nname_f name <> stringP "\""

nname_f :: (Printer a) => Name -> a
nname_f (Name n) =
  textP n


typeDefinitionInputObject :: (Printer a) => InputObjectTypeDefinition -> a
typeDefinitionInputObject _ = stringP ""

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

instance Printer Builder where
  stringP = fromString
  {-# INLINE stringP #-}

  textP   = fromText
  {-# INLINE textP #-}

  charP   = singleton
  {-# INLINE charP #-}

  intP    = fromString . show
  {-# INLINE intP #-}

  floatP  = fromString . show
  {-# INLINE floatP #-}

renderSchemaDoc :: SchemaDocument -> Data.Text.Lazy.Text
renderSchemaDoc = render schemaDocument

render :: (a -> Builder) -> a -> Data.Text.Lazy.Text
render f = toLazyText . f
