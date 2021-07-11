{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TypeFamilies #-}

module Schema where

import Data.Morpheus.Kind (ENUM, SCALAR)
import Data.Morpheus.Types (KIND, GQLType (..), liftEither, EncodeScalar(..), DecodeScalar(..), ScalarValue(..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- model
data Patient = Patient 
  { id         :: PatientId
  , person     :: Person
  , insurances :: [Insurance]
  } deriving (Generic, GQLType)

data Person = Person
  { firstName    :: Text
  , lastName     :: Text
  , phoneNumber  :: Maybe PhoneNumber
  , emailAddress :: Maybe EmailAddress
  } deriving (Generic, GQLType)

newtype PhoneNumber = PhoneNumber Text
  deriving (Generic)

newtype EmailAddress = EmailAddress Text
  deriving (Generic)

data Insurance = Insurance 
  { code      :: InsuranceCode
  , plan      :: InsurancePlan
  , isExpired :: Bool
  } deriving (Generic, GQLType)

data InsuranceCode = InsuranceCode Text Text Text
  deriving (Eq, Generic)

newtype PatientId = PatientId Text
  deriving (Eq, Generic)

data InsurancePlan = Primary | Secondary | Tertiary
  deriving (Generic, GQLType)

-- scalars
instance GQLType PatientId where
  type KIND PatientId = SCALAR

instance GQLType InsuranceCode where
  type KIND InsuranceCode = SCALAR

instance GQLType PhoneNumber where
  type KIND PhoneNumber = SCALAR

instance GQLType EmailAddress where
  type KIND EmailAddress = SCALAR

-- decoders
instance DecodeScalar InsuranceCode where
  decodeScalar _ = Left "invalid insurance code"

instance DecodeScalar PatientId where
  decodeScalar (String x) = pure $ PatientId x
  decodeScalar _          = Left "invalid patient id"

instance DecodeScalar PhoneNumber where
  decodeScalar (String x) = pure $ PhoneNumber x
  decodeScalar _          = Left "invalid phone number"

instance DecodeScalar EmailAddress where
  decodeScalar (String x) = pure $ EmailAddress x
  decodeScalar _          = Left "invalid email address"

-- encoders
instance EncodeScalar PatientId where
  encodeScalar (PatientId value) = String value

instance EncodeScalar InsuranceCode where
  encodeScalar (InsuranceCode c p s) = String (c <> "-" <> p <> "-" <> s)

instance EncodeScalar PhoneNumber where
  encodeScalar (PhoneNumber value) = String value

instance EncodeScalar EmailAddress where
  encodeScalar (EmailAddress value) = String value