module ExpressionTree where

data Operators = Add | Sub | Mul | Div | Pow deriving (Eq)

opWeight = [(Add, 1), (Sub, 1), (Mul, 2), (Div, 2), (Pow, 3)]

getOpWeight :: Operators -> Integer
getOpWeight op =
    case lookup op opWeight of
        Just w  ->  w
        Nothing ->  0

instance Show Operators where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"

data Expression = Number Integer
    | Symbol String
    | Op Operators Expression Expression
    | Func String Expression

    deriving (Eq)

instance Show Expression where
    show e = showWithParen 0 e where 
        showWithParen _ (Symbol x)       = x
        showWithParen _ (Number x)       = show x
        showWithParen _ (Func name e)    = name ++ "(" ++ show e ++ ")"
        showWithParen  w (Op op e1 e2)   = head ++ showWithParen curOpWeight e1 ++ show op ++ showWithParen curOpWeight e2 ++ foot where
            head = if w > curOpWeight then "(" else ""
            foot = if w > curOpWeight then ")" else ""
            curOpWeight = getOpWeight op
