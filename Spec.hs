-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module Spec where

import LTL24

data Spec = Spec {
    sname :: String,
    formula :: LTL24
}
