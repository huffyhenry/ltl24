-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module Main where

import UI

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict

welcome = "Welcome to LTL24 v0.0.1. Copyright (c) Marek Kwiatkowski 2015."

main :: IO ()
main = do putStrLn welcome
          evalStateT (runInputT defaultSettings loop) defaultEnv
