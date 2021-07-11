{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveAnyClass #-}

module Projections (
  -- re-export
  Connection,
  release,
  -- module
  DecodeProblem(..), 
  ConnectProblem (..), 
  connect,
  fetchPatients, 
  fetchPatient) where 

import Control.Exception (Exception, throwIO)
import Data.Aeson
import Data.Text (Text, splitOn, pack)
import Data.ByteString (ByteString)
import Data.Vector (Vector, toList)
import Hasql.Session (Session, statement, run)
import Hasql.Statement (Statement(..))
import Hasql.Connection (Connection, ConnectionError, acquire, release)
import Hasql.TH (vectorStatement, maybeStatement)

import qualified Schema as S

-- module functions
fetchPatients :: Connection -> IO [S.Patient]
fetchPatients connection = 
  run (statement () selectPatients) connection
    >>= either throwIO return
    >>= sequence . fmap decodePatient . toList

fetchPatient :: Connection -> S.PatientId -> IO (Maybe S.Patient)
fetchPatient connection (S.PatientId pid) = 
  run (statement (pid) selectPatient) connection
    >>= either throwIO return
    >>= sequence . fmap decodePatient

-- SQL
type PatientRow = (Value)

selectPatients :: Statement () (Vector PatientRow)
selectPatients =
  [vectorStatement|
    select "data" :: jsonb
    from "projection_patients"
    order by "last_name", "first_name"
  |]

selectPatient :: Statement (Text) (Maybe PatientRow)
selectPatient =
  [maybeStatement|
    select "data" :: jsonb
    from "projection_patients"
    where "id" = $1 :: text
  |]

-- JSON decoders
newtype DecodeProblem = DecodeProblem Text
  deriving (Show, Exception)

decodePatient :: PatientRow -> IO S.Patient
decodePatient row = case fromJSON row of 
  Success patient -> return patient
  Error problem   -> throwIO $ DecodeProblem (pack problem)

instance FromJSON S.Patient where
  parseJSON = withObject "Patient" $ \v -> S.Patient
    <$> v .: "id"
    <*> v .: "person"
    <*> v .: "insurances"

instance FromJSON S.PatientId where
  parseJSON = withText "PatientId" (return <$> S.PatientId)

instance FromJSON S.Person where
  parseJSON = withObject "Person" $ \v -> S.Person
    <$> v .:  "firstName"
    <*> v .:  "lastName"
    <*> v .:? "phoneNumber"
    <*> v .:? "emailAddress"

instance FromJSON S.EmailAddress where
  parseJSON = withText "EmailAddress" (return <$> S.EmailAddress)

instance FromJSON S.PhoneNumber where
  parseJSON = withText "PhoneNumber" (return <$> S.PhoneNumber)

instance FromJSON S.Insurance where
  parseJSON = withObject "Insurance" $ \v -> S.Insurance
    <$> v .: "code"
    <*> v .: "plan"
    <*> v .: "isExpired"

instance FromJSON S.InsuranceCode where 
  parseJSON = withText "InsuranceCode" $ \v -> case splitOn "-" v of
    c : p : s : [] -> return $ S.InsuranceCode c p s
    _              -> fail "invalid insurance code format"

instance FromJSON S.InsurancePlan where 
  parseJSON = withText "InsurancePlan" $ \v -> case v of
    "Primary"   -> return S.Primary
    "Secondary" -> return S.Secondary
    "Tertiary"  -> return S.Tertiary
    _           -> fail "invalid insurance plan format"

-- misc 
data ConnectProblem = ConnectProblem ConnectionError
  deriving (Show, Exception)

connect :: ByteString -> IO Connection
connect connectionString = acquire connectionString 
  >>= either (throwIO . ConnectProblem) return