module SpecFile where

import Text.Parsec.Prim (parse)

import Spec
import Parsers

loadSpecsFromFile :: String -> IO [Spec]
loadSpecsFromFile filepath = do code <- readFile filepath
                                case parse specs "" code of
                                    Left err -> putStrLn (show err) >> return []
                                    Right ss -> return ss
