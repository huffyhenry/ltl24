-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

{-# LANGUAGE MultiParamTypeClasses #-}
module EventProp where

import Data.Maybe (isNothing)

import F24
import Defs
import Literal

data EventProp = Equal Literal Literal
               | Greater Literal Literal
               | IsSet Literal
               | HasQ Int
               | Successful

instance Show EventProp where
    show (Equal l l') = (show l) ++ "=" ++ (show l')
    show (Greater l l') = (show l) ++ ">" ++ (show l')
    show (IsSet l) = (show l) ++ "!"
    show (HasQ i) = "Q" ++ (show i) ++ "!"
    show Successful = "is successful"

instance Formula EventProp where
    nf ep = ep

instance Property EventProp Event where
    sat (Equal l l') e = let v = liteval l e
                             v' = liteval l' e
                         in v == v' && not (isNothing v) && not (isNothing v')
    sat (Greater l l') e = case liteval l e of
                               Nothing -> False
                               Just v -> case liteval l' e of
                                             Nothing -> False
                                             Just v' -> let vv = read v :: Double
                                                            vv' = read v' :: Double
                                                        in vv > vv'
    sat (IsSet l) e = not $ isNothing $ (liteval l e)
    sat (HasQ i) e = hasq i e
    sat Successful e = maybe False (\_ -> True) (outcome e)
