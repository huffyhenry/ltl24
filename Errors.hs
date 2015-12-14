-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

{-# LANGUAGE DeriveDataTypeable #-}
module Errors where

import Control.Exception
import Data.Typeable

data F24FileError = ParserFailure
                  | MissingData
    deriving (Show, Typeable)

instance Exception F24FileError
