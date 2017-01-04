{-# LANGUAGE DeriveDataTypeable #-}
module Errors where

import Control.Exception
import Data.Typeable

data F24FileError = ParserFailure
                  | MissingData
    deriving (Show, Typeable)

instance Exception F24FileError
