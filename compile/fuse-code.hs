#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON (toJSONFilter)

fuseCodeBlock :: [Block] -> [Block]
fuseCodeBlock [] = []
fuseCodeBlock (CodeBlock a1 s1 : CodeBlock _ s2 : xs) =
    CodeBlock a1 (s1 ++ "\n\n" ++ s2) : fuseCodeBlock xs
fuseCodeBlock (x:xs) = x : fuseCodeBlock xs


main :: IO ()
main = toJSONFilter fuse
    where
        fuse (Pandoc m blocks) = Pandoc m (fuseCodeBlock blocks)
