module Main where

import Data.List (isPrefixOf)

-- The canonical machine: a binary incrementer.  Reading the tape from the
-- right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
-- stops; if the carry runs off the tape a new leading 1 is added.
increment :: String -> String
increment bits = reverse (go (reverse bits))
  where
    go [] = "1"
    go ('1' : rest) = '0' : go rest
    go (_ : rest) = '1' : rest

main :: IO ()
main = do
    input <- getContents
    let line = stripCarriageReturn (takeWhile (/= '\n') input)
    if not (all isBit line)
        then putStrLn "Invalid input: expected a binary string."
        else do
            putStrLn ("Initial tape: " ++ line)
            putStrLn ("Final tape: " ++ increment line)

isBit :: Char -> Bool
isBit character = character == '0' || character == '1'

stripCarriageReturn :: String -> String
stripCarriageReturn text
    | "\r" `isPrefixOf` reverse text = reverse (drop 1 text)
    | otherwise = text
