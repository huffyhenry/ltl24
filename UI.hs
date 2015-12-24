-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module UI where

import Data.List (intersperse)
import Data.Char (toLower)
import System.Console.Haskeline

prompt = "LTL24$ "

type Environment = InputT IO

-- A command consists of:
-- * function executing the command based on parameters
-- * command name
-- * help text
type Command = ([String] -> Environment (), (String, String))

runCmd :: Command -> [String] -> Environment ()
runCmd cmd params = (fst cmd) params


commands = [cmdQuit, cmdHelp, cmdAbout]

-- The main program loop
loop :: Environment ()
loop = do input <- getInputLine prompt
          case input of
              Nothing -> runCmd cmdQuit []  -- Ctrl+D
              Just "" -> loop
              Just sth -> let cmd = head (words sth)
                              params = tail (words sth)
                          in case cmd of
                              "quit" -> runCmd cmdQuit params
                              "help" -> runCmd cmdHelp params >> loop
                              "about" -> runCmd cmdAbout params >> loop
                              otherwise -> do outputStrLn ("Unknown command " ++ show cmd ++ ". " ++
                                                           "Say \"help\" for the list of available commands.")
                                              loop

cmdQuit :: Command
cmdQuit = (\_ -> do input <- getInputLine "Really quit [y/n]? "
                    case maybe Nothing (Just . (map toLower)) input of
                        Just "y" -> outputStrLn "Bye." >> return ()
                        Nothing -> runCmd cmdQuit []
                        otherwise -> loop,
           ("quit",
            "quit -- exit the program.\n" ++
            "USAGE: quit")
          )

cmdHelp :: Command
cmdHelp = (\params -> let names = map (fst . snd) commands :: [String]
                          descs = map (snd . snd) commands :: [String]
                      in case params of
                          [] -> outputStrLn ("Available commands: " ++
                                concat (intersperse ", " names) ++ ".\n" ++
                                "Say \"help <command>\" for more information.")
                          c:_ -> maybe (outputStrLn ("Unknown command " ++ show c ++ ".") >> (fst cmdHelp) []) outputStrLn (lookup c (zip names descs)),
           ("help",
            "help -- display information on available commands.\n" ++
            "USAGE: help <command>")
          )

cmdAbout :: Command
cmdAbout = (\_ -> do outputStrLn "LTL24 data querying and verification system, version 0.0.1."
                     outputStrLn "Copyright (c) Marek Kwiatkowski <marek@mareklab.org>, 2015."
                     outputStrLn "Unauthorized use and distribution prohibited.",
            ("about",
             "about -- display information about the program.\n" ++
             "USAGE: about")
           )

