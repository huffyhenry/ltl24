-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module UI where

import Data.List (intersperse)
import Data.Char (toLower)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Console.Haskeline
import Text.Parsec.Prim (parse)

import F24
import F24File
import Spec
import Parsers

prompt = "LTL24$ "

data Configuration = Configuration {
    cores :: Int
}

data Environment = Environment {
    config :: Configuration,
    games :: [Game],
    specs :: [Spec]
}

addGame :: Game -> Environment -> Environment
addGame game env = Environment{
    config = config env,
    games = (games env) ++ [game],
    specs = specs env
}

addSpec :: Spec -> Environment -> Environment
addSpec spec env = Environment{
    config = config env,
    games = (games env),
    specs = specs env ++ [spec]
}

defaultEnv :: Environment
defaultEnv = Environment{
    config = Configuration{
        cores = 1
    },
    games = [],
    specs = []
}

type ProgramState = InputT (StateT Environment IO) ()

-- A command consists of:
-- * function executing the command based on parameters
-- * command name
-- * help text
type Command = ([String] -> ProgramState, (String, String))

runCmd :: Command -> [String] -> ProgramState
runCmd cmd params = (fst cmd) params


commands = [cmdQuit, cmdHelp, cmdAbout, cmdLoad, cmdStatus]

-- The main program loop
loop :: ProgramState
loop = do input <- getInputLine prompt
          case input of
              Nothing -> runCmd cmdQuit []  -- Ctrl+D
              Just "" -> loop
              Just sth -> let cmd = head (words sth)
                              params = tail (words sth)
                              msg = "Unknown command " ++ show cmd ++ ". " ++
                                    "Say \"help\" for the list of " ++
                                    "available commands."
                          in case cmd of
                              "quit" -> runCmd cmdQuit params
                              "help" -> runCmd cmdHelp params >> loop
                              "about" -> runCmd cmdAbout params >> loop
                              "load" -> runCmd cmdLoad params >> loop
                              "status" -> runCmd cmdStatus params >> loop
                              otherwise -> outputStrLn msg >> loop

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
cmdHelp = (\params -> let names = map (fst . snd) commands
                          descs = map (snd . snd) commands
                          find = \x -> lookup x (zip names descs)
                          msg = \x -> "Unknown command " ++ show x ++ "."
                          unkn = \x -> (outputStrLn (msg x) >> (fst cmdHelp) [])
                      in case params of
                          [] -> outputStrLn ("Available commands: " ++
                                concat (intersperse ", " names) ++ ".\n" ++
                                "Say \"help <command>\" for more information.")
                          c:_ -> maybe (unkn c) outputStrLn (find c),
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

cmdLoad :: Command
cmdLoad = (\params -> case head params of
                          "game" -> do let filename = head $ tail params
                                       game <- lift $ lift (loadGameFromFile filename)
                                       lift $ modify (addGame game)
                          "spec" -> do case tail params of
                                           "inline":rest -> do case parse spec "" (unwords rest) of
                                                                   Left _ -> outputStrLn "Syntax error."
                                                                   Right sp -> lift $ modify (addSpec sp),
           ("load",
            "load -- add games or specs to the active environment.\n" ++
            "USAGE: load [game | spec] <filename>\n" ++
            "       load spec inline <name>:<formula>")
          )

cmdStatus :: Command
cmdStatus = (\params -> do env <- lift get
                           let gms = games env
                           let sps = specs env
                           let info :: (a -> String) -> String -> [a] -> String
                               info fid name items = case length items of
                                                         0 -> "0 " ++ name
                                                         n | n < 5 -> (show n) ++ " " ++ name ++ ": " ++ concat (intersperse ", " (map fid items))
                                                         n | n >= 5 -> info fid name (take 4 items) ++ ", ..."
                           outputStrLn ((info (show . gid) "game(s)" gms) ++ ".")
                           outputStrLn ((info sname "spec(s)" sps) ++ "."),

             ("status",
              "status -- display the state of the active environment.\n" ++
              "USAGE: status")
            )
