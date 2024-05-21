import System.IO
import System.Environment (getArgs)

printLines :: [String] -> Int -> IO ()
printLines  [] _ = return ()
printLines (line:lines) i = do
    putStrLn $ show i ++ " " ++ line
    printLines lines (i+1)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Missing args"
        [_] -> putStrLn "Only one argument provided"
        (filename:secondArg:_) -> do
            contents <- readFile filename
            let linesOfFile = lines contents
            printLines linesOfFile 0
            putStrLn $ "Second argument: " ++ secondArg
