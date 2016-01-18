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
import Defs
import LTL24()
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
addGame game env = env {games = games env ++ [game]}

addSpec :: Spec -> Environment -> Environment
addSpec spec env = env {specs = specs env ++ [spec]}

defaultEnv :: Environment
defaultEnv = Environment{
    config = Configuration{
        cores = 1
    },
    games = [],
    specs = []
}

type ProgramState = InputT (StateT Environment IO) ()

data Command = Command {
    commandName :: String,
    helpText :: String,
    runCommand :: [String] -> ProgramState
}

failCmd :: Command -> ProgramState
failCmd cmd = outputStrLn ("Invalid command syntax. " ++
                           "Say \"help " ++ commandName cmd ++ "\" for help.")


commands :: [Command]
commands = [cmdQuit, cmdHelp, cmdAbout, cmdLoad, cmdStatus, cmdVerify]

getCommand :: String -> Maybe Command
getCommand name = lookup name (zip (map commandName commands) commands)

-- The main program loop
loop :: ProgramState
loop = do input <- getInputLine prompt
          case input of
              Nothing -> runCommand cmdQuit []  -- Ctrl+D
              Just "" -> loop                   -- Return
              Just sth -> let name = head (words sth)
                              params = tail (words sth)
                              msg = "Unknown command \"" ++ name ++ "\". " ++
                                    "Say \"help\" for the list of " ++
                                    "available commands."
                          in if name == "quit" then runCommand cmdQuit params
                             else case getCommand name of
                                 Nothing -> outputStrLn msg >> loop
                                 Just cmd -> runCommand cmd params >> loop

cmdQuit :: Command
cmdQuit = Command {
    commandName = "quit",
    helpText = "quit -- exit the program.\n" ++
               "USAGE: quit",
    runCommand = runQuit
}

runQuit :: [String] -> ProgramState
runQuit _ = do input <- getInputLine "Really quit [y/n]? "
               case maybe Nothing (Just . (map toLower)) input of
                   Just "y" -> outputStrLn "Bye." >> return ()
                   Nothing -> runQuit []
                   otherwise -> loop

cmdHelp :: Command
cmdHelp = Command {
    commandName = "help",
    helpText = "help -- display information on available commands.\n" ++
               "USAGE: help\n" ++
               "       help <command name>",
    runCommand = runHelp
}

runHelp :: [String] -> ProgramState
runHelp [] = let cmdList = concat $ intersperse ", " (map commandName commands)
             in outputStrLn ("Available commands: " ++ cmdList ++ ".")
runHelp (sth:_) = case getCommand sth of
                      Nothing -> outputStrLn ("Unknown command " ++ show sth ++ ".")
                      Just cmd -> outputStrLn $ helpText cmd

cmdAbout :: Command
cmdAbout = Command {
    commandName = "about",
    helpText = "about -- display information about the program.\n" ++
               "USAGE: about",
    runCommand = runAbout
}

runAbout :: [String] -> ProgramState
runAbout _ = do outputStrLn "LTL24 data querying and verification system, version 0.0.1."
                outputStrLn "Copyright (c) Marek Kwiatkowski <marek@mareklab.org>, 2015."
                outputStrLn "Unauthorized use and distribution prohibited."

cmdLoad :: Command
cmdLoad = Command {
    commandName = "load",
    helpText = "load -- add games or specs to the active environment.\n" ++
               "USAGE: load [game | spec] <filename>\n" ++
               "       load spec inline <name>:<formula>",
    runCommand = runLoad
}

-- FIXME: Check if game or spec being added already exists
runLoad :: [String] -> ProgramState
runLoad ("game":rest) = do game <- lift (lift (loadGameFromFile (head rest)))
                           lift (modify (addGame game))
                           runCommand cmdStatus []
runLoad ("spec":("inline":rest)) = do case parse spec "" (unwords rest) of
                                          Left _ -> outputStrLn "Syntax error."
                                          Right sp -> do lift $ modify (addSpec sp)
                                                         runCommand cmdStatus []
runLoad _ = failCmd cmdLoad


cmdStatus :: Command
cmdStatus = Command {
    commandName = "status",
    helpText = "status -- display the state of the active environment.\n" ++
                "USAGE: status",
    runCommand = runStatus
}

runStatus :: [String] -> ProgramState
runStatus [] = do env <- lift get
                  -- FIXME: Define instance Show Environment instead
                  let gms = games env
                  let sps = specs env
                  let info :: (a -> String) -> String -> [a] -> String
                      info fid name items = case length items of
                                                0 -> "0 " ++ name
                                                n | n < 5 -> (show n) ++ " " ++ name ++ ": " ++ concat (intersperse ", " (map fid items))
                                                n | n >= 5 -> info fid name (take 4 items) ++ ", ..."
                  outputStrLn ((info (show . gid) "game(s)" gms) ++ ".")
                  outputStrLn ((info specName "spec(s)" sps) ++ ".")

cmdVerify :: Command
cmdVerify = Command {
    commandName = "verify",
    helpText = "verify -- check if the games satisfy the specs.\n" ++
               "USAGE: verify",
    runCommand = runVerify
}

runVerify :: [String] -> ProgramState
runVerify _ = do env <- lift get
                 let cg :: Game -> [Spec] -> ProgramState
                     cg g [] = return ()
                     cg g (s:ss) = let passed = sat (specFormula s) (events g)
                                       msg = if passed then "passed" else "failed"
                                   in outputStrLn ((specName s) ++ ": " ++ msg) >> cg g ss
                 let cgs :: [Game] -> [Spec] -> ProgramState
                     cgs [] _ = return ()
                     cgs (g:gs) ss = do outputStrLn ("Verifying game " ++ (show $ gid g) ++ ".")
                                        cg g ss
                                        cgs gs ss

                 cgs (games env) (specs env)

--cmdDrop = Command {
--   commandName = "drop",
--   helpText = "drop -- remove a game or a spec from the active environment.\n" ++
--              "USAGE: drop [game | spec] <identifier>",
--   runCommand = runDrop
--}
--
--runDrop :: [String] -> ProgramState
--runDrop ("game":ids) = return ()
--runDrop ("spec":ids) = return ()
--runDrop _ = failCmd cmdDrop
