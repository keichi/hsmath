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
    show (Symbol x)       = x
    show (Number x)       = show x
    show (Func name e)    = name ++ "(" ++ show e ++ ")"
    show (Op op e1 e2)    = show e1 ++ show op ++ show e2

