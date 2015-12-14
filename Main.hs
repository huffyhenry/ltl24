-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module Main where

import Defs
import F24
import F24File
import Parsers
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    let req = "always (type_id=15 implies next type_id=10)"
    game <- loadGameFromFile "f24sample.xml"
    case parse ltl24 "" req of
        Left err -> putStrLn $ show err
        Right phi -> do putStrLn $ "parsed: " ++ (show phi)
                        putStrLn $ "normed: " ++ (show $ nf phi)
                        putStrLn $ "checked: " ++ (show $ sat phi (events game))
