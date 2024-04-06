module Main (main) where
import System.Exit (exitFailure)
import Control.Monad (when)
import Pred (falla)

main :: IO ()
main = do
    let x = V.fromList [1, 2, 3]
        y = V.fromList [4, 5, 6]
        result = sumdiv2 x y
    print result