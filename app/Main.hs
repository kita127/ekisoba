module Main where

import qualified Language.C            as C
import qualified Language.C.Ekisoba    as Eki
import qualified Language.C.Pretty     as Pretty
import qualified Language.C.System.GCC as GCC

main :: IO ()
main = do
    parseRes <- C.parseCFile (GCC.newGCC "gcc")
                        Nothing
                        ["-I/usr/include/gtk-2.0"]
                        "./examples/sample0.c"
    putStrLn ""
    let transRes = case parseRes of
            Right a -> Eki.translate a
            _       -> error "parse error"

    print transRes
