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
        xs -> printLines xs 0
