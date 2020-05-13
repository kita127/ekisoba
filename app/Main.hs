module Main where

import qualified Language.C                    as C
import qualified Language.C.System.GCC         as GCC
import qualified Language.C.Pretty             as Pretty

main :: IO ()
main = do
    res <- C.parseCFile (GCC.newGCC "gcc")
                        Nothing
                        ["-I/usr/include/gtk-2.0"]
                        "./examples/sample0.c"
    putStrLn ""
    case res of
        Right a -> print a
        _       -> putStrLn "parse error"
