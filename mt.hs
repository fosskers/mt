-- mt - Prints a multiplication table.
-- Written by Colin Woodbury <colingw@gmail.com>
-- Usage: mt <number>

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Char (isDigit)

main :: IO ()
main = do
  args  <- getArgs
  let lim = processArgs args
  putStrLn $ makeTable lim

-- A default value of `10` is given for improper input.
processArgs :: [String] -> Int
processArgs []    = 10
processArgs (n:_) | and $ map isDigit n = read n
                  | otherwise = 10

makeTable :: Int -> String
makeTable lim = topRow ++ "\n" ++ border ++ "\n" ++ table
  where topRow = renderRow "" 1 lim
        border = replicate (read pad + 2 + (lim * (read pad + 1))) '-'
        table  = concat $ map (\m -> renderRow (show m) m lim ++ "\n") [1..lim]
        pad    = getPad lim

renderRow :: String -> Int -> Int -> String
renderRow label mult lim = printf ("%" ++ pad ++ "s |%s") label renderedRow
  where renderedRow = concat $ map renderNum [1..lim]
        renderNum n = printf (" %" ++ pad ++ "d") (n * mult) :: String
        pad         = getPad lim

getPad :: Int -> String
getPad lim = show . length . show $ lim * lim
