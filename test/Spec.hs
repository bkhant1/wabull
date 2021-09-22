{-# LANGUAGE TupleSections #-}

import Test.HUnit 
import Lexer
    ( Token(..)
    , Kw(..)
    , Sp(..)
    , Unit(..)
    , lexWabull
    , tokensToCode
    )
import Parser 
    ( parseWabull
    , Declaration(..)
    , Expression(..)
    , Symbol
    , UnannotatedExpression(..)
    , UnannotatedDeclaration(..)
    )
import Runner 
    ( runWabull 
    , Layout(..)
    , Value (..)
    , Box (..)
    )
import Data.String (String)
import Data.Map ( fromList, toList ) 
import qualified Data.Set as Map
import Data.Ratio ((%))
import Compiler (compileWabull)


exampleDeclaration :: String
exampleDeclaration = 
    "a over b with ratio=0.5:\n" ++
    "    [a with ratio=ratio, b with ratio=(1 - ratio)]"


exampleDeclarationLexed :: [(Token, Int)]
exampleDeclarationLexed =  
    map (, 1)
        [ Name "a"
        , Name "over"
        , Name "b"
        , Keyword With
        , Name "ratio"
        , Separator Equal
        , SizeLiteral (1 / 2) Percentage
        , Separator Column
        , Separator NewLine
        ]
    ++
    map (, 2)
        [ Tabulation 
        , Separator LeftSquareBracket
        , Name "a"
        , Keyword With
        , Name "ratio"
        , Separator Equal
        , Name "ratio"
        , Separator Comma
        , Name "b"
        , Keyword With
        , Name "ratio"
        , Separator Equal
        , Separator LeftBracket
        , SizeLiteral 1 Percentage
        , Name "-"
        , Name "ratio"
        , Separator RightBracket
        , Separator RightSquareBracket
        ]

exampleDeclarationParsed =
    fromList 
        [ ( "over"
            , ( ArgFunctionDeclaration 
                    "a" 
                    "b" 
                    [ ( "ratio", 
                        ( Literal (1 % 2) Percentage
                        ,1
                        )
                    ) ] 
                    ( List 
                        [ ( KwargFunctionCall 
                                "a" 
                                [ ("ratio", ( Var "ratio",2 ) ) ]
                            , 2
                          )
                        , ( KwargFunctionCall 
                                "b"
                                [ ( "ratio" , 
                                    ( ArgedFunctionCall 
                                        ( Literal (1 % 1) Percentage, 2 ) 
                                        "-" 
                                        ( Var "ratio", 2 ) 
                                        []
                                    , 2
                                    )
                                ) ]
                          , 2 
                          )
                        ]
                    , 2
                    )
              , 1
              )
        ) ]


exampleProgram :: String
exampleProgram = 
    "layout:\n" ++
    "   box over box with ratio=0.1\n" ++
    "\n" ++
    "a over b with ratio=0.5:\n" ++
    "   [a with hRatio=r, b with hRatio=1-r]\n"

-- >>> compileWabull "layout:\n box over box with ratio=0.1\n a over b with ratio=0.5:\n [a with hRatio=r, b with hRatio=1-r]\n"

exampleProgramRan = Right
    Container 
        [ Box { widthRatio=1, heightRatio=0.1 }
        , Box { widthRatio=1, heightRatio=0.9 }
        ]


-- exampleDeclarationParsed =
--     fromList 
--         [ ( "over"
--             , ArgFunctionDeclaration 
--             "a" 
--             "b"
--             [ ( "ratio", Literal (1/2) Percentage ) ]
--             ( List 
--                 [ KwargFunctionCall 
--                     "a"
--                     [("ratio", Var "ratio")]
--                 , KwargFunctionCall 
--                     "b"
--                     [ ( "ratio"
--                         , ArgedFunctionCall
--                             (Literal 1 Percentage) 
--                             "-" 
--                             (Var "ratio")
--                             []
--                         )
--                     ]
--                 ]
--             )
--         ) ]
    
{-  
>>> (lexWabull exampleDeclaration) == (Right exampleDeclarationLexed)
True

>>> parseWabull exampleDeclarationLexed
Left "Can't parse \n (line 1)"

-}

tests :: Test
tests = TestList
    [ TestCase
        ( assertEqual 
            "Lexing exampleDeclaration"
            (lexWabull exampleDeclaration)
            (Right exampleDeclarationLexed)
        )
    , TestCase 
        ( assertEqual 
            "Incorrect character fails at lexing"
            (lexWabull "box:\n { 1 }")
            (Left "Found unexpected character: '{' (at line 2)")
        )
    , TestCase
        ( assertEqual 
            "Parsing simple program"
            (parseWabull exampleDeclarationLexed)
            (Right exampleDeclarationParsed)
        )
    -- , TestCase 
    --     ( assertEqual
    --         "Preprocessed implicit layout brackets around indented block"
    --         ( tokensToCode . preprocessedImplicitLayout <$> lexWabull 
    --             ( "b headedBy a:\n" ++
    --               "   a over b with ratio=0.1"
    --             )
    --         )
    --         ( Right "b headedBy a: (a over b with ratio=0.1)")
    --     )
    -- , TestCase 
    --     ( assertEqual 
    --         "Preprocessed implicit layout with two layers"
    --         ( tokensToCode . preprocessedImplicitLayout <$> lexWabull 
    --             ( "b headedBy a: \n" ++
    --               "   if wideScreen then\n" ++
    --               "       a over b with ratio=0.1\n" ++
    --               "   else\n" ++
    --               "       a over b with ratio=0.2"
    --             )
    --         )
    --         (Right "b headedBy a: (if wideScreen then (a over b with ratio=0.1) else (a over b with ratio=0.2))")
    --     )
    -- TODO: preprocessedImplicitLayout should not add brackets if the indentation is in a list
    -- TODO: other cases? probs should add brackets only if in a specific block (after a with, after a then, after ...)
    -- , TestCase 
    --     ( assertEqual 
    --         "Parsing exampleDeclaration"
    --         (parseWabull exampleDeclarationLexed)
    --         (Right exampleDeclarationParsed)
    --     )
    ]


main :: IO Counts 
main = runTestTT tests
