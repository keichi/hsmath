module ExpressionParser where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

import ExpressionTree

expr :: Parser Expression
expr = foldr ($) <$> factor <*> many (add <|> sub)
    where add = (Op Add) <$> (char '+' *> factor)
          sub = (flip $ Op Sub) <$> (char '-' *> factor)

factor :: Parser Expression
factor = foldr ($) <$> term <*> many (mul <|> div)
    where mul = (Op Mul) <$> (char '*' *> term)
          div = (flip $ Op Div) <$> (char '/' *> term)

term :: Parser Expression
term = number <|> paren <|> try func <|> variable
    where number = (Number . read) <$> many1 digit
          paren = char '(' *> expr <* char ')'
          variable = Symbol <$> many1 letter
          func = Func <$> (many1 letter) <*> (char '(' *> expr <* char ')')

