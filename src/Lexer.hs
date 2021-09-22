module Lexer
    ( lexWabull
    , Kw(..)
    , Sp(..)
    , Unit(..)
    , Token(..)
    , tokensToCode
    , tokenToCode
    ) where

import Numeric

import Data.Ratio

import Data.Char 
    ( isLetter
    , isDigit 
    )
import Data.Functor 
    ( (<&>) )
import Text.Read 
    ( readMaybe 
    )
import Data.List.Split 
    ( splitOn
    )
import Data.Ratio ( (%) )
import qualified Debug.Trace as D
import GHC.OldList (intercalate)


data Kw = 
    If
    | Then
    | Else
    | With
    deriving ( Show, Eq )


data Sp =
    Column
    | Equal
    | NewLine
    | Comma
    | RightSquareBracket
    | LeftSquareBracket
    | RightBracket
    | LeftBracket
    deriving ( Show, Eq )


data Unit =
    Percentage
    | CentiMeters
    deriving ( Show, Eq )


data Token =
    Name String
    | Separator Sp
    | Tabulation
    | Keyword Kw 
    | SizeLiteral Rational Unit
    deriving ( Show, Eq )


readFloatToRational :: String -> Either String Rational
readFloatToRational s = 
    let 
        parse intPart fracPart = 
            let
                numberOfTrailingZeroInFrac = length $ dropWhile (/='0') $ reverse fracPart
                numberOfFracFigures = length fracPart - numberOfTrailingZeroInFrac 
                ratioParsed = do
                    ipp <- (readMaybe intPart :: Maybe Integer)
                    fpp <- (readMaybe fracPart :: Maybe Integer) 
                    return $ (ipp*(10^numberOfFracFigures) + fpp) % (10^numberOfFracFigures)
            in
            case ratioParsed of 
                Just v -> 
                    return v
                Nothing ->
                    Left $ "Couldn't parse " ++ s
    in
    case splitOn "." s of
        [intPart, fracPart] ->
            parse intPart fracPart
        [intPart] ->
            parse intPart "0"
        _ ->
            Left $ "Couldn't parse number in " ++ s


parseName :: String -> Either String (Token, String)
parseName program =
    let
        parseNameRec :: String -> String -> Either String (String, String)
        parseNameRec (c:rest) acc
            | isLetter c =
                parseNameRec rest (acc ++ [c]) 
            | otherwise =
                return (acc, c:rest)
        parseNameRec [] [] =
            Left "Tried to parse a name but program terminates" 
        parseNameRec [] acc =
            return (acc, [])
    in 
    do
        (name, rest) <- parseNameRec program ""
        return (Name name, rest)


parseSizeLiteral :: String -> Either String (Token, String)
parseSizeLiteral program =
    let 
        parseSizeLiteralRec :: String -> (String, String) -> (String, String, String)
        parseSizeLiteralRec (c:rest) (accValue, accUnit)
            | isLetter c || c == '%' =
                parseSizeLiteralRec rest (accValue, c:accUnit)
            | isSizeLiteral c =
                parseSizeLiteralRec rest (c:accValue, accUnit)
            | otherwise =
                (accValue, accUnit, c:rest)
        parseSizeLiteralRec [] (accValue, accUnit) =
            (accValue, accUnit, [])

        parseUnit :: String -> Either String Unit
        parseUnit "cm" = return CentiMeters 
        parseUnit "%" = return Percentage
        parseUnit "" = return Percentage
        parseUnit text = Left $ "Coulnd't parse unit: " ++ text

        (value, unit, rest) = parseSizeLiteralRec program ("", "")
    in
    do 
        valueParsed <- readFloatToRational $ reverse value
        unitParsed <- parseUnit $ reverse unit
        return (SizeLiteral valueParsed unitParsed, rest)


isSizeLiteral :: Char -> Bool
isSizeLiteral = or . (<*>) [isDigit, (=='-'), (=='.')] . pure


isOperator :: Char -> Bool
isOperator = or . (<*>) [(=='+'), (=='-'), (=='/'), (=='*') ] . pure


tokenToCode :: Token -> String
tokenToCode token =
    case token of 
        Separator NewLine -> "\n"
        Separator Column -> ":"
        Separator Equal -> "="
        Separator Comma -> ","
        Separator RightSquareBracket -> "]"
        Separator LeftSquareBracket -> "["
        Separator RightBracket -> ")"
        Separator LeftBracket -> "("
        Keyword If -> "if"
        Keyword Else -> "else"
        Keyword Then -> "then"
        Keyword With -> "with"
        SizeLiteral r u -> 
            show r ++ case u of 
                Percentage -> "%" 
                CentiMeters -> "cm"
        Tabulation -> "    "
        Name s -> s


tokensToCode :: [(Token, Int)] -> String
tokensToCode =
    intercalate "" . map (tokenToCode . fst) 


lexWabull :: String -> Either String [(Token, Int)]
lexWabull layout =
    let
        lexWabullRec :: String -> [(Token, Int)] -> Int -> Either String [(Token, Int)]
        lexWabullRec layout acc lineno =
            let 
                recurseAddingLineno :: String -> Token -> [(Token, Int)] -> Either String [(Token, Int)]
                recurseAddingLineno rest token acc = 
                    lexWabullRec rest ((token, lineno):acc) lineno 
            in
            case layout of
                ':':rest ->
                    recurseAddingLineno rest (Separator Column) acc
                '=':rest -> 
                    recurseAddingLineno rest (Separator Equal) acc 
                ',':rest ->
                    recurseAddingLineno rest (Separator Comma) acc 
                '[':rest ->
                    recurseAddingLineno rest (Separator LeftSquareBracket) acc 
                ']':rest ->
                    recurseAddingLineno rest (Separator RightSquareBracket) acc 
                '(':rest ->
                    recurseAddingLineno rest (Separator LeftBracket) acc 
                ')':rest ->
                    recurseAddingLineno rest (Separator RightBracket) acc 
                ' ':' ':' ':' ':rest -> do
                    recurseAddingLineno rest Tabulation acc 
                ' ':rest ->
                    lexWabullRec rest acc lineno
                '\n':rest ->
                    lexWabullRec rest ((Separator NewLine, lineno):acc) (lineno + 1)
                'w':'i':'t':'h':rest ->
                    recurseAddingLineno rest (Keyword With) acc 
                'i':'f':rest ->
                    recurseAddingLineno rest (Keyword If) acc 
                'e':'l':'s':'e':rest ->
                    recurseAddingLineno rest (Keyword Else) acc 
                't':'h':'e':'n':rest ->
                    recurseAddingLineno rest (Keyword Else) acc 
                c:rest
                    | isLetter c ->
                        do
                            (token, newRest) <- parseName (c:rest)
                            recurseAddingLineno newRest token acc 
                    | isOperator c ->
                        recurseAddingLineno rest (Name [c]) acc 
                    | isSizeLiteral c ->
                        do 
                            (token, newRest) <- parseSizeLiteral (c:rest)
                            recurseAddingLineno newRest token acc 
                    | otherwise ->
                        Left ("Found unexpected character: '" ++ [c] ++ "' (at line " ++ show lineno ++ ")")
                [] ->
                    return acc
    in 
    lexWabullRec layout [] 1 <&> reverse
