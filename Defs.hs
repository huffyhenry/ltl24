{-# LANGUAGE MultiParamTypeClasses #-}
module Defs where


-- Formula is something normalizable
class Formula a where
    nf :: a -> a

-- A property can be evaluated to a boolean on a suitable model object.
class Formula a => Property a b where
    sat :: a -> b -> Bool
