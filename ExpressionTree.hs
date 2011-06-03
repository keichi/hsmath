module ExpressionTree where

data Operators = Add | Sub | Mul | Div | Pow deriving (Eq)

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

    deriving (Eq, Show)
