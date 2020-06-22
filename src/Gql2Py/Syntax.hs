{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Description: The GraphQL AST
module Gql2Py.Syntax
  ( Name(..)
  , isValidName
  , SchemaDocument(..)
  , Variable(..)
  , Argument(..)
  , ValueConst(..)
  , Value(..)
  , StringValue(..)
  , ListValueG(..)
  , ListValue
  , ListValueC
  , ObjectValueG(..)
  , ObjectValue
  , ObjectValueC
  , ObjectFieldG(..)
  , DefaultValue
  , Directive(..)
  , GType(..)
  , Nullability(..)
  , isNullable
  , isNotNull
  , isListType
  , showNT
  , NamedType(..)
  , ListType(..)
  , Description(..)
  , TypeDefinition(..)
  , SchemaDefinition(..)
  , ObjectTypeDefinition(..)
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , EnumValue(..)
  , InputObjectTypeDefinition(..)
  , DirectiveDefinition(..)
  , DirectiveLocation(..)
  , ExecutableDirectiveLocation(..)
  , TypeSystemDirectiveLocation(..)
  ) where

import           Control.Monad.Fail         (fail)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift(..), Lit(RationalL))
import           Language.Haskell.TH.Lib    (litE)
import           Protolude                  hiding (lift)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Text.Regex.TDFA            as TDFA
import qualified Data.Scientific            as S

newtype Name
  = Name { unName :: Text }
  deriving ( Eq, Ord, Show, Hashable, IsString, Lift, Semigroup
           , Monoid, J.ToJSONKey, J.ToJSON)

isValidName :: Name -> Bool
isValidName (Name text) =
  TDFA.match compiledRegex $ T.unpack text
  where
    compiledRegex = TDFA.makeRegex ("^[_a-zA-Z][_a-zA-Z0-9]*$" :: BL.ByteString) :: TDFA.Regex

parseName :: Text -> J.Parser Name
parseName text =
  bool (fail $ T.unpack errorMessage) (pure name) $ isValidName name
  where
    name = Name text
    errorMessage = text <> " is not valid GraphQL name"

instance J.FromJSON Name where
  parseJSON = J.withText "Text" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

newtype SchemaDocument
  = SchemaDocument {getTypeDefinitions :: [TypeDefinition]}
  deriving (Ord, Show, Eq, Lift, Hashable)

newtype Variable
  = Variable { unVariable :: Name }
  deriving ( Eq, Ord, Show, Hashable, Lift, J.ToJSONKey, J.FromJSONKey
           , J.ToJSON, J.FromJSON)

data Argument
  = Argument
  { _aName  :: !Name
  , _aValue :: !Value
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Argument

data ValueConst
  = VCInt !Integer
  | VCFloat !S.Scientific
  | VCString !StringValue
  | VCBoolean !Bool
  | VCNull
  | VCEnum !EnumValue
  | VCList !ListValueC
  | VCObject !ObjectValueC
  deriving (Ord, Show, Eq, Generic)

instance Lift ValueConst where
  lift (VCInt i) = [e|VCInt $(lift i)|]
  lift (VCFloat sc) = [e|VCFloat $(litE (RationalL (toRational sc)))|]
  lift (VCString s) = [e|VCString $(lift s)|]
  lift (VCBoolean b) = [e|VCBoolean $(lift b)|]
  lift (VCNull) = [e|VCNull|]
  lift (VCEnum ev) = [e|VCEnum $(lift ev)|]
  lift (VCList xs) = [e|VCList $(lift xs)|]
  lift (VCObject o) = [e|VCObject $(lift o)|]

instance Hashable ValueConst

data Value
  = VVariable !Variable
  | VInt !Integer
  | VFloat !S.Scientific
  | VString !StringValue
  | VBoolean !Bool
  | VNull
  | VEnum !EnumValue
  | VList !ListValue
  | VObject !ObjectValue
  deriving (Ord, Show, Eq, Generic)

instance Lift Value where
  lift (VVariable v) = [e|VVariable $(lift v)|]
  lift (VInt i) = [e|VInt $(lift i)|]
  lift (VFloat sc) = [e|VFloat $(litE (RationalL (toRational sc)))|]
  lift (VString s) = [e|VString $(lift s)|]
  lift (VBoolean b) = [e|VBoolean $(lift b)|]
  lift (VNull) = [e|VNull|]
  lift (VEnum ev) = [e|VEnum $(lift ev)|]
  lift (VList xs) = [e|VList $(lift xs)|]
  lift (VObject o) = [e|VObject $(lift o)|]

instance Hashable Value

newtype StringValue
  = StringValue { unStringValue :: Text }
  deriving (Ord, Show, Eq, Lift, Hashable)

newtype ListValueG a
  = ListValueG {unListValue :: [a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ListValue = ListValueG Value

type ListValueC = ListValueG ValueConst

newtype ObjectValueG a
  = ObjectValueG {unObjectValue :: [ObjectFieldG a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ObjectValue = ObjectValueG Value

type ObjectValueC = ObjectValueG ValueConst

data ObjectFieldG a
  = ObjectFieldG
  { _ofName  :: Name
  , _ofValue :: a
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (ObjectFieldG a)

type DefaultValue = ValueConst

data Directive
  = Directive
  { _dName      :: !Name
  , _dArguments :: ![Argument]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Directive

newtype Nullability
  = Nullability { unNullability :: Bool }
  deriving (Show, Ord, Eq, Lift, Generic, Hashable)

data GType
  = TypeNamed !Nullability !NamedType
  | TypeList !Nullability !ListType
  deriving (Eq, Show, Lift, Generic)

instance Ord GType where
  compare x y =  flip compare (isNullable x) (isNullable y)



class ToGType a where
  toGT :: a -> GType

instance ToGType GType where
  toGT t = t

instance J.ToJSON GType where
  toJSON = J.toJSON . showGT

instance Hashable GType

showGT :: GType -> Text
showGT = \case
  TypeNamed nullability nt -> showNT nt <> showNullable nullability
  TypeList nullability lt  -> showLT lt <> showNullable nullability

showNullable :: Nullability -> Text
showNullable = bool "!" "" . unNullability

showNT :: NamedType -> Text
showNT = unName . unNamedType

showLT :: ListType -> Text
showLT lt = "[" <> showGT (unListType lt) <> "]"

isNullable :: GType -> Bool
isNullable = \case
  (TypeNamed nullability _) -> unNullability nullability
  (TypeList nullability _)  -> unNullability nullability

isListType :: GType -> Bool
isListType = \case
  (TypeList _ _)  -> True
  (TypeNamed _ _) -> False


isNotNull :: GType -> Bool
isNotNull = not . isNullable

newtype NamedType
  = NamedType { unNamedType :: Name }
  deriving (Eq, Ord, Show, Hashable, Lift, J.ToJSON,
            J.ToJSONKey, J.FromJSON, J.FromJSONKey)

instance ToGType NamedType where
  toGT = TypeNamed (Nullability True)

newtype ListType
  = ListType {unListType :: GType }
  deriving (Eq, Ord, Show, Lift, Hashable)

instance ToGType ListType where
  toGT = TypeList (Nullability True)

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | DefinitionSchema SchemaDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeDefinition

newtype Description
  = Description { unDescription :: Text }
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid, Hashable,
            J.ToJSON, J.FromJSON)

data SchemaDefinition
  = SchemaDefinition
  { _sdDescription          :: !(Maybe Description)
  , _sdDirectives           :: ![Directive]
  , _sdFieldsDefinition     :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable SchemaDefinition

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdDescription          :: !(Maybe Description)
  , _otdName                 :: !Name
  , _otdImplementsInterfaces :: ![NamedType]
  , _otdDirectives           :: ![Directive]
  , _otdFieldsDefinition     :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ObjectTypeDefinition

data FieldDefinition
  = FieldDefinition
  { _fldDescription         :: !(Maybe Description)
  , _fldName                :: !Name
  , _fldArgumentsDefinition :: !ArgumentsDefinition
  , _fldType                :: !GType
  , _fldDirectives          :: ![Directive]
  }
  deriving (Show, Eq, Lift, Generic)

instance Hashable FieldDefinition

instance Ord FieldDefinition where
    compare x y = compare (_fldType x) (_fldType y)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition
  = InputValueDefinition
  { _ivdDescription  :: !(Maybe Description)
  , _ivdName         :: !Name
  , _ivdType         :: !GType
  , _ivdDefaultValue :: !(Maybe DefaultValue)
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputValueDefinition

data InterfaceTypeDefinition
  = InterfaceTypeDefinition
  { _itdDescription      :: !(Maybe Description)
  , _itdName             :: !Name
  , _itdDirectives       :: ![Directive]
  , _itdFieldsDefinition :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InterfaceTypeDefinition

data UnionTypeDefinition
  = UnionTypeDefinition
  { _utdDescription :: !(Maybe Description)
  , _utdName        :: !Name
  , _utdDirectives  :: ![Directive]
  , _utdMemberTypes :: ![NamedType]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable UnionTypeDefinition

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdDescription :: !(Maybe Description)
  , _stdName        :: !Name
  , _stdDirectives  :: ![Directive]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ScalarTypeDefinition

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdDescription      :: !(Maybe Description)
  , _etdName             :: !Name
  , _etdDirectives       :: ![Directive]
  , _etdValueDefinitions :: ![EnumValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumTypeDefinition

data EnumValueDefinition
  = EnumValueDefinition
  { _evdDescription :: !(Maybe Description)
  , _evdName        :: !EnumValue
  , _evdDirectives  :: ![Directive]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumValueDefinition

newtype EnumValue
  = EnumValue { unEnumValue :: Name }
  deriving (Show, Eq, Lift, Hashable, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdDescription      :: !(Maybe Description)
  , _iotdName             :: !Name
  , _iotdDirectives       :: ![Directive]
  , _iotdValueDefinitions :: ![InputValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputObjectTypeDefinition

data DirectiveDefinition
  = DirectiveDefinition
  { _ddDescription :: !(Maybe Description)
  , _ddName        :: !Name
  , _ddArguments   :: !ArgumentsDefinition
  , _ddLocations   :: ![DirectiveLocation]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveDefinition

data DirectiveLocation
  = DLExecutable !ExecutableDirectiveLocation
  | DLTypeSystem !TypeSystemDirectiveLocation
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveLocation

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDirectiveLocation

data TypeSystemDirectiveLocation
  = TSDLSCHEMA
  | TSDLSCALAR
  | TSDLOBJECT
  | TSDLFIELD_DEFINITION
  | TSDLARGUMENT_DEFINITION
  | TSDLINTERFACE
  | TSDLUNION
  | TSDLENUM
  | TSDLENUM_VALUE
  | TSDLINPUT_OBJECT
  | TSDLINPUT_FIELD_DEFINITION
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDirectiveLocation
