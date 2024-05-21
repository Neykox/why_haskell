{-# LANGUAGE DeriveGeneric #-}

import System.IO
import System.Environment (getArgs)
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Set (Set)
import qualified Data.Set as Set

data MyStates = MyStates {
    read :: Char,
    to_state :: String,
    write :: Char,
    action :: String
} deriving (Show, Generic)

data MyData = MyData {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: [tname, [MyStates]]
} deriving (Show, Generic)

instance FromJSON MyData

recur :: String -> String -> IO ()
recur to_state head:tape = do
    let matchingTransitions = filter (\(tn, states) -> tn == to_state && any (\state -> read state == head) states) (transitions myData)
    print matchingTransitions

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Missing args"
        [_] -> putStrLn "Only one argument provided"
        (filename:secondArg:_) -> do
            contents <- B.readFile filename
            let jsonData = eitherDecode contents :: Either String MyData
            case jsonData of
                Left err -> putStrLn $ "Error parsing JSON: " ++ err
                Right myData -> do
                    print myData
                    putStrLn $ "Second argument: " ++ secondArg

                    let set1 = Set.fromList (concat (alphabet myData))
                    let excludedChar = blank myData
                    let checkChar = \c -> Set.member c set1 && c /= excludedChar
                    if all checkChar secondArg
                        then putStrLn "All characters are in the alphabet and not equal to the blank character"
                        else putStrLn "Some characters are not in the alphabet or are equal to the blank character"

                    recur (initial myData) secondArg