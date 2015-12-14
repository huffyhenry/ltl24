-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

{-# LANGUAGE MultiParamTypeClasses #-}
module Defs where


-- Formula is something normalizable
class Formula a where
    nf :: a -> a

-- A property can be evaluated to a boolean on a suitable model object.
class Formula a => Property a b where
    sat :: a -> b -> Bool
