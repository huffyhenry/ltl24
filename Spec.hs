-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module Spec where

import LTL24

data Spec = Spec {
    specName :: String,
    specFormula :: LTL24
}

instance Eq Spec where
    s1 == s2 = (specName s1) == (specName s2)
