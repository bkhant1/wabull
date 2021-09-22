module Main where

import qualified Lexer
import qualified Parser
import qualified Compiler
import qualified Render


main :: IO ()
main = do
    file <- readFile "test/wabulls/justABox.wab"
    print $ Render.toJs =<< Compiler.compileWabull file
