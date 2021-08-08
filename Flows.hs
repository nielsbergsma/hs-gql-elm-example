{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies #-}

module Flows where

import Commands

data Environment = Environment Bool

class Flow c r | c -> r where
  handle :: Environment -> c -> IO r

instance (Flow NewPatient Bool) where
  handle env (NewPatient {..}) = return True
