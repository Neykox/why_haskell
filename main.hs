{-# LANGUAGE DeriveGeneric #-}

import System.IO
import System.Environment (getArgs)
import Data.Aeson (parseJSON, eitherDecode, FromJSON, genericParseJSON, defaultOptions, withObject, (.:))
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (find)

data MyStates = MyStates {
    read :: Char,
    to_state :: String,
    write :: Char,
    action :: String
} deriving (Show, Generic)

instance FromJSON MyStates

data MyData = MyData {
    name :: String,
    alphabet :: [String],
    blank :: Char,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map String [MyStates]
} deriving (Show, Generic)

instance FromJSON MyData

findMatchingState :: String -> Char -> MyData -> Maybe MyStates
findMatchingState to_state head myData = do
    let matchingTransitions = Map.filter (\states -> to_state == to_state) (transitions myData)
    states <- Map.lookup to_state matchingTransitions
    find (\state -> Main.read state == head) states

handle_output :: String -> Char -> String -> String -> MyStates -> IO ()
handle_output beg head tape curr_state state = do
    let str = "[" ++ beg ++ "<" ++ [head] ++ ">" ++ tape ++ "] (" ++ curr_state ++ ", " ++ [head] ++ ") -> (" ++ (to_state state) ++ ", " ++ [(Main.write state)] ++ ", " ++ (action state) ++ ")"
    putStrLn str

recur :: String -> String -> String -> MyData -> IO ()
recur curr_state beg (head:tape) myData = do
    let matchingState = findMatchingState curr_state head myData
    case matchingState of
        Nothing -> putStrLn "No state found where head == read"
        Just state -> do
            handle_output beg head tape curr_state state
            let newHead = (write state)
            if (action state) == "RIGHT" then recur (to_state state) (beg ++ [newHead]) tape myData
            else do
                let lastCharOfBeg = last beg
                let newTape = [lastCharOfBeg] ++ [newHead] ++ tape
                let newBeg = init beg --remove last char of beg -> going to be the next head
                recur (to_state state) newBeg newTape myData

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Missing args"
        [_] -> putStrLn "Only one argument provided"
        (filename:user_input:_) -> do
            contents <- B.readFile filename
            let jsonData = eitherDecode contents :: Either String MyData
            case jsonData of
                Left err -> putStrLn $ "Error parsing JSON: " ++ err
                Right myData -> do
                    let set1 = Set.fromList (concat (alphabet myData))
                    let excludedChar = blank myData
                    let checkChar = \c -> Set.member c set1 && c /= excludedChar
                    if all checkChar user_input
                        then putStrLn "All characters are in the alphabet and not equal to the blank character"
                        else putStrLn "Some characters are not in the alphabet or are equal to the blank character"

                    recur (initial myData) "" (user_input ++ "..........") myData