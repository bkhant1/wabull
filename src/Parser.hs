module Parser 
    ( parseWabull
    , Declaration(..)
    , Expression(..)
    , UnannotatedExpression(..)
    , UnannotatedDeclaration(..)
    , Symbol
    ) where


import qualified Lexer as L
import qualified Data.Map as M


import qualified Debug.Trace as DT
import Data.List ( groupBy )
import Data.List.Split ( splitWhen )
import Data.IntSet (fold)
import GHC.OldList (groupBy)
import Lexer (tokenToCode)


type Symbol = String


type Expression = (UnannotatedExpression, Int)


data UnannotatedExpression =
    List [Expression]
    | Literal Rational L.Unit
    | ArgedFunctionCall Expression Symbol Expression [(Symbol, Expression)]
    | Var Symbol
    | KwargFunctionCall Symbol [(Symbol, Expression)]
    | IfElse Expression Expression Expression
    deriving ( Show, Eq )


type Declaration = (UnannotatedDeclaration, Int)


data UnannotatedDeclaration = 
    Variable Expression 
    | ArgFunctionDeclaration Symbol Symbol [(Symbol, Expression)] Expression
    | KwargFunctionDeclaration [(Symbol, Expression)] Expression
    deriving ( Show, Eq )


goToMatchingX :: L.Token -> L.Token -> [(L.Token, Int)] -> Either String ([(L.Token, Int)], [(L.Token, Int)])
goToMatchingX open close tokens =
    let 
        gtmxRec [] ((token, l) : rest) left
            | token == close = 
                return (left, rest)
            | token == open = 
                Left "Unbalanced square brackets"
            | otherwise = 
                gtmxRec [] rest ((token,l):left)
        gtmxRec _ [] _ = 
            Left "Reached end of file when trying to match brackets"
        gtmxRec stack ((token,l) : rest) left 
            | token == close =
                gtmxRec (tail stack) rest ((close,l):left)
            | token == open =
                gtmxRec (open:stack) rest ((open,l):left)
            | otherwise =
                gtmxRec stack rest ((token,l):left)
    in
    gtmxRec [] tokens [] >>= (\(x,y) -> return (reverse x, y))


goToMatchingSquareBracket :: [(L.Token, Int)] -> Either String ([(L.Token,Int)], [(L.Token, Int)])
goToMatchingSquareBracket = 
    goToMatchingX (L.Separator L.LeftSquareBracket) (L.Separator L.RightSquareBracket)


goToMatchingBracket :: [(L.Token, Int)] -> Either String ([(L.Token, Int)], [(L.Token, Int)])
goToMatchingBracket = 
    goToMatchingX (L.Separator L.LeftBracket) (L.Separator L.RightBracket)


parseList :: [(L.Token, Int)] -> Either String [Expression]
parseList tokens = 
    let 
        parseExprAndRest exprAndRest acc = do
            (expression, moreRest) <- parseExpression exprAndRest
            parseListRec moreRest (acc ++ [expression])
        parseListRec [] acc = 
            return acc
        parseListRec ( (L.Separator L.Comma, l1) : rest) acc = 
            parseExprAndRest rest acc
        parseListRec x acc = 
            parseExprAndRest x acc
    in
    parseListRec tokens []


parseAnyExpressionButArgFunctionCall :: [(L.Token, Int)] -> Either String (Expression, [(L.Token, Int)])
parseAnyExpressionButArgFunctionCall tokens =
    case tokens of

        -- Kwarg function call
        (L.Name s, l1) : (L.Keyword L.With, l2) : rest ->
            do 
                (kwargs, rest) <- parseKwargs rest
                return ( (KwargFunctionCall s kwargs, l1), rest)

        -- Variable
        (L.Name s, l) : rest ->
            return ( (Var s, l), rest)

        -- List
        (L.Separator L.LeftSquareBracket, l) : rest ->
            do
                (list, rest) <- goToMatchingSquareBracket rest
                listParsed <- parseList list
                return ((List listParsed, l), rest)
        
        -- Literal
        (L.SizeLiteral v u, l) : rest ->
            return ((Literal v u, l), rest)

        -- IfThenElse
        (L.Keyword L.If, lIf) : rest -> do 
            (condition, rest1) <- parseExpression rest
            case rest1 of
                ( (L.Keyword L.Then, lThen) : rest2) -> do
                    (expr1, rest3) <- parseExpression rest2 
                    case rest3 of 
                        ( (L.Keyword L.Else, lElse) : rest4) -> do
                            (expr2, rest5) <- parseExpression rest4
                            return ((IfElse condition expr1 expr2, lIf), rest5)
                        _ ->
                            Left $ "No else found in if/then/else expression line " ++ show lIf
                _ ->
                    Left $ "No then found in if/then/else expression line " ++ show lIf
        
        -- Expression in brackets
        (L.Separator L.LeftBracket, l) : rest -> do
            (expr, rest) <- goToMatchingBracket rest
            (exprParsed, _) <- parseExpression expr
            return (exprParsed, rest)
        
        -- Error
        (anyOther, l) : _ -> 
            Left $ "Can't parse " ++ tokenToCode anyOther ++ " (line " ++ show l ++ ")"

        [] ->
            Left "Parsing unexpectedly interrupted"


parseExpression :: [(L.Token, Int)] -> Either String (Expression, [(L.Token, Int)])
parseExpression tokens =
    do
        (expression, rest) <- parseAnyExpressionButArgFunctionCall tokens
        DT.traceShow (expression, rest) $ case rest of
            -- Arged function call, the only case where an expression is followed by a name
            ( (L.Name s, l1) : arg2AndRest) -> 
                do 
                    (arg2, maybeWithAndRest) <- parseExpression arg2AndRest
                    case maybeWithAndRest of 
                        ( (L.Keyword L.With, lWith) : kwargsAndRest) ->
                            do
                                (kwargs, actualRest) <- parseKwargs kwargsAndRest
                                return ((ArgedFunctionCall expression s arg2 kwargs, l1), actualRest)
                        actualRest ->
                            return ((ArgedFunctionCall expression s arg2 [], l1), actualRest)

            -- Anything else, we're done parsing
            _ -> 
                return (expression, rest)


parseKwargs :: [(L.Token, Int)] -> Either String ([(Symbol, Expression)], [(L.Token, Int)])
parseKwargs tokens =
    let         
        parseValueAndContinue key valueAndRest acc =
            do 
                (value, rest) <- parseExpression valueAndRest
                parseKwargsRec rest ((key, value):acc)

        parseKwargsRec ( (L.Separator L.Comma, l1) : (L.Name key, l2) : (L.Separator L.Equal, l3) : valueAndRest) acc =
            parseValueAndContinue key valueAndRest acc
        parseKwargsRec ( (L.Name key, l1) : (L.Separator L.Equal, l2) : valueAndRest) acc =
            parseValueAndContinue key valueAndRest acc
        parseKwargsRec any acc =
            return (acc, any)
    in
    parseKwargsRec tokens []


parseTopLevel :: [(L.Token, Int)] -> M.Map Symbol Declaration -> Either String (M.Map Symbol Declaration)
parseTopLevel tokens acc = 
    case tokens of 

        -- variable declaration
        ( (L.Name name, l1) : (L.Separator L.Column, l2) : expression) ->
            do
                (variableValue, rest) <- parseExpression expression
                parseTopLevel rest (M.insert name (Variable variableValue, l1) acc)

        -- arged function declaration 
        ( (L.Name arg1, l1) : (L.Name functionName, l2) : (L.Name arg2, l3) : (withOrColumn, l4) : rest) ->
            let 
                parseBodyThenRest bodyAndRest kwargs = do 
                    (expression, moreRest) <- parseExpression (drop 1 bodyAndRest)
                    let function = ArgFunctionDeclaration arg1 arg2 kwargs expression
                    parseTopLevel moreRest (M.insert functionName (function, l1) acc)
            in
            if withOrColumn == L.Separator L.Column then
                parseBodyThenRest rest []
            else if withOrColumn == L.Keyword L.With then
                do 
                    (kwargs, moreRest) <- parseKwargs rest
                    parseBodyThenRest moreRest kwargs
            else 
                Left $ "Expected ':' or 'with' but found " ++ show (take 1 rest) ++ " (line " ++ show l4 ++ ")"

        -- kwarged function declaration
        ( (L.Name functionName, l1) : (L.Keyword L.With, l2) : rest) ->
            do 
                (kwargs, bodyAndRest) <- parseKwargs rest
                (expression, moreRest) <- parseExpression (drop 1 bodyAndRest)
                parseTopLevel moreRest (M.insert functionName (KwargFunctionDeclaration kwargs expression, l1) acc)

        ( (L.Separator L.NewLine, l): rest) ->
            parseTopLevel rest acc

        [] ->
            return acc

        s:_ ->
            Left $ "Invalid top level symbol" ++ show s



isNewLine :: (L.Token, b) -> Bool
isNewLine = (==L.Separator L.NewLine) . fst


isTabulation :: (L.Token, b) -> Bool
isTabulation = (==L.Tabulation) . fst


isVoidToken :: (L.Token, b) -> Bool
isVoidToken s = (fst s == L.Separator L.NewLine) || (fst s == L.Tabulation)


{- 
Replaces tabs + new lines by brackets, e.g :

`biggerBox with ratio: 
    box with (ratio * 2)`

becomes:

`biggerBox with ratio: (box with (ratio * 2))`
-}
makeCodeBlocksForOneLayer :: [[(L.Token, Int)]] -> [[[(L.Token, Int)]]]
makeCodeBlocksForOneLayer =
    let
        startsByTab [] = False
        startsByTab (h:t) = isTabulation h
        groupByIndentBlock = groupBy
            ( \x y -> (startsByTab x && startsByTab y) 
                      || ((not .startsByTab) x && (not . startsByTab) y)
            )
        addBrackets l = ((L.Separator L.LeftBracket, 0):l) ++ [(L.Separator L.RightBracket, 0)]
    in
    groupByIndentBlock 


chompTabs :: [[(L.Token, b)]] -> [[(L.Token, b)]]
chompTabs [] = []
chompTabs (line:lines) =
    case line of 
        (L.Tabulation,_):restOfLine ->
            restOfLine:chompTabs lines
        any ->
            any:chompTabs lines


tabsToBrackets :: [[(L.Token, Int)]] -> [[(L.Token, Int)]]
tabsToBrackets tokens =
    let 
        bracketTabulatedBlocks :: [[[(L.Token, Int)]]] -> [[[(L.Token, Int)]]] -> [[[(L.Token, Int)]]]
        bracketTabulatedBlocks [] acc = reverse acc  
        bracketTabulatedBlocks (block:blocks) acc = 
            case block of 
                [] -> 
                    bracketTabulatedBlocks blocks acc
                (line1:_) -> 
                    case line1 of
                        [] -> 
                            bracketTabulatedBlocks blocks (block:acc)
                        (L.Tabulation,_):restOfLine ->
                            let
                                chompedBlock = chompTabs block
                                blocksInsideBracketed = tabsToBrackets chompedBlock
                                bracketedBlock = 
                                    [(L.Separator L.LeftBracket,0)]:blocksInsideBracketed ++ [[(L.Separator L.RightBracket,0)]]
                            in
                            bracketTabulatedBlocks
                                blocks
                                (bracketedBlock:acc)
                        _ ->
                            bracketTabulatedBlocks blocks (block:acc)
    in
    concat $ bracketTabulatedBlocks (makeCodeBlocksForOneLayer tokens) []


preprocessedImplicitLayout :: [(L.Token, Int)] -> [(L.Token, Int)]
preprocessedImplicitLayout = 
    concat . tabsToBrackets . splitWhen isNewLine 


parseWabull :: [(L.Token, Int)] -> Either String (M.Map Symbol Declaration)
parseWabull tokens = 
    parseTopLevel 
        (filter (not . isVoidToken) tokens)
        M.empty
