module Main where

import qualified Data.Aeson                 as Aes
import qualified Data.Aeson.Encode.Pretty   as AesP
import qualified Data.Aeson.TH              as TH
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Language.C                 as C
import qualified Language.C.Ekisoba         as Eki
import qualified Language.C.Pretty          as Pretty
import qualified Language.C.System.GCC      as GCC

main :: IO ()
main = do
    cRes <- C.parseCFile (GCC.newGCC "gcc")
                        Nothing
                        ["-I/usr/include/gtk-2.0"]
                        "./examples/sample0.c"
    putStrLn ""
    print cRes
    putStrLn ""

    ekiRes <- Eki.parseCFile Eki.GCC
                        ["-I/usr/include/gtk-2.0"]
                        "./examples/sample0.c"

    case ekiRes of
        Right r -> B.putStrLn $ AesP.encodePretty r
        Left r  -> error $ show r




