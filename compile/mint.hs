#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON (toJSONFilter)

import Text.Pandoc.Walk
{-import Text.Pandoc.Shared-} -- Exports Element

latexEnv :: String -> String -> String
latexEnv env s = beginEnv ++ "\n" ++ s ++ "\n" ++ endEnv
    where
        beginEnv = "\\begin{" ++ env ++ "}"
        endEnv = "\\end{" ++ env ++ "}"

mintCodeBlock :: Block -> Block
mintCodeBlock (CodeBlock _ s) =
    RawBlock (Format "latex") (latexEnv "haskellcode" s)
mintCodeBlock x = x

-- This should work starting on commit 75cfa7b46 (13 May 2015)
fragileHeader :: Block -> Block
fragileHeader (Header 2 a s) = Header 2 e s
    where
        (b, c, d) = a
        e = (b, c ++ ["fragile"], d)
fragileHeader x = x


main :: IO ()
main = toJSONFilter mintCodeBlock
