{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeFamilies #-}

module Resolvers (ResolverContext, newContext, resolve) where

import Prelude hiding (id)
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types (RootResolver (..), Undefined (..), GQLType (..), ResolverQ, ResolverM, liftEither)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import qualified Schema as S
import qualified Projections as P
import qualified Commands as C
import qualified Flows as F

data Query m = Query
  { patient  :: PatientArguments -> m (Maybe S.Patient)
  , patients :: m [S.Patient]
  } deriving (Generic, GQLType)

newtype PatientArguments = PatientArguments
  { id :: S.PatientId
  } deriving (Generic, GQLType)

newtype Mutation m = Mutation
  { newPatient :: NewPatientArguments -> m S.NewPatientResult
  } deriving (Generic, GQLType)

newtype NewPatientArguments = NewPatientArguments
  { patient :: S.NewPatient
  } deriving (Show, Generic, GQLType)

-- resolvers
newtype ResolverContext = ResolverContext P.Connection

newContext :: P.Connection -> ResolverContext
newContext = ResolverContext

resolvePatients :: ResolverContext -> ResolverQ e IO [S.Patient]
resolvePatients (ResolverContext db) = liftIO $ P.fetchPatients db

resolvePatient :: ResolverContext -> PatientArguments -> ResolverQ e IO (Maybe S.Patient)
resolvePatient (ResolverContext db) (PatientArguments pid) = liftIO $ P.fetchPatient db pid

resolveNewPatient :: NewPatientArguments -> ResolverM () IO S.NewPatientResult
resolveNewPatient (NewPatientArguments arguments) = do
  command <- liftEither . pure $ C.parseNewPatient arguments
  result  <- liftIO $ F.handle (F.Environment False) command
  return $ S.NewPatientResult result

resolve :: ResolverContext -> ByteString -> IO ByteString
resolve context = interpreter root
  where
    root :: RootResolver IO () Query Mutation Undefined
    root = RootResolver
      { queryResolver = Query
        { patient  = resolvePatient context
        , patients = resolvePatients context
        }
      , mutationResolver = Mutation
        { newPatient = resolveNewPatient
        }
      , subscriptionResolver = Undefined
      }
