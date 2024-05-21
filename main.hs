{-# LANGUAGE DeriveGeneric #-}

import System.IO
import System.Environment (getArgs)
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data MyData = MyData {
    name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial :: String,
    finals :: [String],
    --transitions :: [(String, Char, String, Char, String)]
} deriving (Show, Generic)

instance FromJSON MyData

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
                Right data -> print data
            putStrLn $ "Second argument: " ++ secondArg

            let set1 = Set.fromList data.alphabet
            let excludedChar = data.blank
            let checkChar = \c -> Set.member c set1 && c /= excludedChar
            if all checkChar secondArg
                then putStrLn "All characters are in set1 and not equal to the excluded character"
                else putStrLn "Some characters are not in set1 or are equal to the excluded character"