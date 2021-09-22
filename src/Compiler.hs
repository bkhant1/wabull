{-# LANGUAGE TupleSections #-}

module Compiler 
    ( compileWabull
    )
    where


import qualified Parser as P
import qualified Lexer as L
import qualified Runner as R


compileWabull :: String -> Either String R.Layout
compileWabull program = do
    lexed <- L.lexWabull program
    parsed <- P.parseWabull lexed
    R.runWabull parsed
