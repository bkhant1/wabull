{-# LANGUAGE LambdaCase #-}

module Runner 
    ( runWabull
    , Layout(..)
    , Box(..)
    , Value(..)
    )
    where


import qualified Parser as P
import qualified Lexer as L
import qualified Data.Map as M
import Control.Monad ( (>=>) ) 

import Debug.Trace 


data Box = Box
    { widthRatio :: Rational
    , heightRatio :: Rational
    }
    deriving ( Show )


data Value =
    Visual Layout
    | Numeric Rational
    | Boolean Bool
    | Thunk ([(P.Symbol, P.Expression)] -> Value)


instance Show Value where
    show a = ""


data Layout = 
    El Box
    | Container [Layout]
    deriving ( Show )


mergeKwargs :: [(P.Symbol, P.Expression)] -> [(P.Symbol, P.Expression)] -> Either String [(P.Symbol, P.Expression)]
mergeKwargs defaults fromCall =
    let 
        defaultsAsMap = M.fromList defaults
        fromCallAsMap = M.fromList fromCall
    in
    if M.isSubmapOfBy (\_ _ -> True) fromCallAsMap defaultsAsMap then
        return $ M.toList $ M.union
            fromCallAsMap
            defaultsAsMap
    else
        Left $ "Unexpected kwarg arguments"


evalExpressionWithArgsAndKwargs ::
    M.Map P.Symbol P.Declaration -> 
    [(P.Symbol, P.Expression)] -> 
    [(P.Symbol, P.Expression)] -> 
    Maybe ((P.Symbol, P.Expression), (P.Symbol, P.Expression)) ->
    P.Expression ->
    Either String Value
evalExpressionWithArgsAndKwargs decls declKwargs kwargs args expr =
    let 
        toVarDecl (s, expr) = (s, (P.Variable expr, 0))
        argsAsList = maybe [] (\x -> [toVarDecl $ fst x, toVarDecl $ snd x]) args
    in
    do
        mergedKwargs <- mergeKwargs declKwargs kwargs
        let declsAndInputs = foldl
                (flip $ uncurry M.insert)
                decls $
                argsAsList
                    ++ map (\(s,e) -> (s, (P.Variable e, 0))) mergedKwargs
        evalExpression declsAndInputs expr


getNumeric :: M.Map P.Symbol P.Declaration -> P.Expression -> Either String Rational
getNumeric decls expr = 
    case evalExpression decls expr of
        Right (Numeric v) -> return v
        _ -> Left $ "Argument should be numeric (is: " ++ show expr ++ ")"


evalForAThunk :: M.Map P.Symbol P.Declaration -> P.Expression -> Either String Value
evalForAThunk decls expr = do
    evaled <- evalExpression decls expr
    case evaled of
        Thunk code -> return $ Thunk code
        _ -> Left $ "Was expecting to evaluate a thunk but got: " ++ show evaled 


evalArgedFunctionCall :: 
    M.Map P.Symbol P.Declaration -> 
    P.Symbol ->
    P.Expression -> 
    P.Expression -> 
    [(P.Symbol, P.Expression)] ->
    Either String Value
evalArgedFunctionCall decls fname arg1 arg2 kwargs = 
    case fname of 
        "+" -> do
            left <- getNumeric decls arg1
            right <- getNumeric decls arg2
            return $ Numeric $ left + right
        "-" -> do
            left <- getNumeric decls arg1
            right <- getNumeric decls arg2
            return $ Numeric $ left - right
        _ ->
            case M.lookup fname decls of 
                Just (P.ArgFunctionDeclaration arg1name arg2name declKwargs body, l) -> 
                    let args = ((arg1name, arg1), (arg2name, arg2)) in
                    evalExpressionWithArgsAndKwargs decls declKwargs kwargs (Just (args)) body
                Just (P.Variable expr, l) ->
                    let args = [("_1", arg1), ("_2", arg2)] ++ kwargs in
                    do
                        thunk <- evalForAThunk decls expr
                        Right thunk
                Just _ ->
                    Left $ "Could not find a arged function declaration for " ++ fname
                Nothing ->
                    Left $ "Could not find a arged function declaration for " ++ fname


maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither error Nothing = Left error


evalBox :: M.Map P.Symbol P.Declaration -> [(P.Symbol, P.Expression)] -> Either String Layout
evalBox decls kwargs = 
    let 
        getRational :: P.Expression -> Either String Rational
        getRational expr = do
            case evalExpression decls expr of
                Right (Numeric x) -> return x
                x -> Left $ "Ratio argument is not a number (is: " ++ show x ++ ")"
        getList :: P.Expression -> Either String Layout
        getList expr = do
            case evalExpression decls expr of
                Right (Visual (Container content)) -> return (Container content)
                x -> Left $ "Content argument is not a list (is: " ++ show x ++ ")"
    in
    do
        mergedKwargs <- mergeKwargs [("ratio", (P.Literal 1 L.Percentage, 0)), ("content", (P.List [], 0))] kwargs
        ratio <- getRational =<< maybeToEither "Weird, shouldn't take that value" (M.lookup "ratio" (M.fromList mergedKwargs))
        content <- getList =<< maybeToEither "" (M.lookup "content" (M.fromList mergedKwargs))
        return $ El $ Box ratio ratio
    

evalKwargedFunctionCall :: M.Map P.Symbol P.Declaration -> P.Symbol -> [(P.Symbol, P.Expression)] -> Either String Value
evalKwargedFunctionCall decls fname kwargs = 
    case fname of 
        "box" ->
            evalBox decls kwargs >>= return . Visual
        _ ->
            case M.lookup fname decls of
                Just (P.KwargFunctionDeclaration declKwargs body, l) -> 
                    evalExpressionWithArgsAndKwargs decls declKwargs kwargs Nothing body
                Just (P.Variable expression, l) -> do
                    thunk <- evalForAThunk decls expression
                    Right thunk 
                Just _ ->
                    Left $ "Could not find a kwarged function declaration for " ++ fname
                Nothing ->
                    Left $ "Could not find a kwarged function declaration for " ++ fname


evalIfElse :: M.Map P.Symbol P.Declaration -> P.Expression -> P.Expression -> P.Expression -> Either String Value
evalIfElse decls condition theIf theElse = 
    do
        condition <- evalExpression decls condition
        case condition of
            Boolean b ->
                if b then evalExpression decls theIf else evalExpression decls theElse
            _ -> 
                Left "Error in if/else, condition must be boolean" 
            

evalVariable :: M.Map P.Symbol P.Declaration -> P.Symbol -> Either String Value
evalVariable decls name = trace "evalVar" $
    if name == "box" then
        evalKwargedFunctionCall decls name []
    else 
        case M.lookup name decls of
            Just (P.KwargFunctionDeclaration _ _, l) ->
                evalKwargedFunctionCall decls name []
            Just (P.Variable expr, l) ->
                evalExpression decls expr
            Nothing ->
                Left $ "Variable not found: " ++ name
            unexpected -> 
                Left $ "Got an unexpected " ++ show unexpected


evalExpression :: M.Map P.Symbol P.Declaration -> P.Expression -> Either String Value
evalExpression decls expr = 
    let 
        evalExpressionWithErrorIfNumeric :: M.Map P.Symbol P.Declaration -> P.Expression -> Either String Layout
        evalExpressionWithErrorIfNumeric decls = 
            evalExpression decls >=>
              \case
                Numeric _ -> Left "Lists can only be of visual elements"
                Visual l -> return l
                _ -> Left "What?"
    in
    case expr of 
        (P.List exprs, l) -> 
            Visual . Container <$> mapM (evalExpressionWithErrorIfNumeric decls) exprs
        (P.ArgedFunctionCall arg1 fname arg2 kwargs, l) ->
            evalArgedFunctionCall decls fname arg1 arg2 kwargs
        (P.KwargFunctionCall fname kwargs, l) ->
            evalKwargedFunctionCall decls fname kwargs
        (P.Var name, l) ->
            evalVariable decls name
        (P.Literal value _, l) -> 
            return $ Numeric value
        (P.IfElse condition expr1 expr2, l) ->
            evalIfElse decls condition expr1 expr2
            

runWabull :: M.Map P.Symbol P.Declaration -> Either String Layout
runWabull declarations = 
    let 
        layout = M.lookup "layout" declarations
    in
    case layout of
        Just (P.Variable expr, l)  -> do 
            value <- evalExpression declarations expr 
            case value of 
                Numeric _ ->
                    Left $ "The 'layout' variable has to have type list or box"
                Visual containerOrEl ->
                    return containerOrEl
                it ->
                    Left $ "Got " ++ show it ++ " when expecting a numeric or a visual element (line " ++ show l ++ ")"
        _ ->
            Left "Trying to build a program that does not have a 'layout' variable declaration"