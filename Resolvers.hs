{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TypeFamilies #-}

module Resolvers (ResolverContext, newContext, resolve) where

import Prelude hiding (id)
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types (RootResolver (..), Undefined (..), GQLType (..), ResolverQ)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import qualified Schema as S
import qualified Projections as P

data Query m = Query
  { patient  :: PatientArguments -> m (Maybe S.Patient)
  , patients :: m [S.Patient]
  } deriving (Generic, GQLType)

data PatientArguments = PatientArguments
  { id :: S.PatientId 
  } deriving (Generic, GQLType)

-- resolvers
data ResolverContext = ResolverContext P.Connection

newContext :: P.Connection -> ResolverContext
newContext connection = ResolverContext connection

resolvePatients :: ResolverContext -> ResolverQ e IO [S.Patient]
resolvePatients (ResolverContext db) = liftIO $ P.fetchPatients db

resolvePatient ::  ResolverContext -> PatientArguments -> ResolverQ e IO (Maybe S.Patient)
resolvePatient (ResolverContext db) (PatientArguments pid) = liftIO $ P.fetchPatient db pid

resolve :: ResolverContext -> ByteString -> IO ByteString
resolve context = interpreter root 
  where
    root :: RootResolver IO () Query Undefined Undefined
    root = RootResolver
      { queryResolver = Query 
        { patient  = resolvePatient context
        , patients = resolvePatients context
        }
      , mutationResolver     = Undefined
      , subscriptionResolver = Undefined
      }
