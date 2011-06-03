import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

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

getVarVal :: [(String, Double)] -> String -> Double
getVarVal table name =
    case lookup name table of
        Just val    ->  val
        Nothing     ->  0.0

eval :: Expression -> [(String, Double)] -> Double
eval expr table = case expr of
    Number num  ->  fromIntegral num
    Symbol name ->  getVarVal table name
    Op Add l r     ->  eval l table + eval r table
    Op Sub l r     ->  eval l table - eval r table 
    Op Mul l r     ->  eval l table * eval r table
    Op Div l r     ->  eval l table / eval r table
    Func "sin" e    ->  sin $ eval e table
    Func "cos" e    ->  cos $ eval e table
    Func "log" e    ->  log $ eval e table
    Func "tan" e    ->  tan $ eval e table

isFuncOf :: Expression -> String -> Bool
isFuncOf expr name = case expr of
    Number num    ->  False
    Symbol s      ->  if s == name then True else False
    Op _ l r      ->  l `isFuncOf` name || r `isFuncOf` name
    Func _ e      ->  e `isFuncOf` name

diff :: Expression -> String -> Expression
diff expr var = case expr of
    Number num    ->  Number 0
    Symbol s      ->  if s == var then Number 1 else Number 0
    Op Add l r       ->  Op Add (diff l var) (diff r var)
    Op Sub l r       ->  Op Sub (diff l var) (diff r var)
    Op Mul l r       ->  Op Add (Op Mul (diff l var) r) (Op Mul l (diff r var))
    Op Div l r       ->  Op Div (Op Sub (Op Mul (diff l var) r) (Op Mul l (diff r var))) (Op Mul r r)
    Func "sin" e         ->  Op Mul (Func "cos" e) (diff e var)
    Func "cos" e         ->  Op Sub (Number 0) (Op Mul (Func "sin" e) (diff e var))
    Func "log" e         ->  Op Div (diff e var) e
    Func "tan" e         ->  Op Div (diff e var) (Op Mul (Func "cos" e) (Func "cos" e))

simplify :: Expression -> Expression
simplify expr = case expr of
    --数値リテラル同士の計算
    Op Add (Number n1) (Number n2)     ->  Number (n1 + n2)
    Op Sub (Number n1) (Number n2)     ->  Number (n1 - n2)
    Op Mul (Number n1) (Number n2)     ->  Number (n1 * n2)
    Op Div (Number n1) (Number n2)     ->  Number $ floor (fromIntegral n1 / fromIntegral n2)

    --加減算の簡単化
    Op Add e (Number 0)        ->  e
    Op Add (Number 0) e        ->  e
    Op Add e1 e2 | e1 == e2    ->  Op Mul (Number 2) e1
    Op Sub e (Number 0)        ->  e
    Op Sub e1 e2 | e1 == e2    ->  Number 0

    --乗除算の簡単化
    Op Mul _ (Number 0)        ->  Number 0
    Op Mul (Number 0) _        ->  Number 0
    Op Mul e (Number 1)        ->  simplify e
    Op Mul (Number 1) e        ->  simplify e
    Op Div e (Number 1)        ->  e
    Op Div e1 e2 | e1 == e2    ->  Number 1
    Op Div (Number 0) e        ->  Number 0

    --シンボルと数値は簡単化できない
    Symbol _                        ->  expr
    Number _                        ->  expr

    --その他の場合は
    Op op l r        ->     Op op (simplify l) (simplify r)
    Func name e      ->     Func name (simplify e)
    _                ->     expr

simplifyn :: Expression -> Expression
simplifyn expr =
    if simplifiedExpr == expr
        then expr
        else simplifyn simplifiedExpr
    where simplifiedExpr = simplify expr

testexpr2 = Op Mul (Symbol "x") (Func "log" (Symbol "x"))

main = do
    case parse expr "" "sin(x)/x" of
        Left error  ->  print error
        Right exp   ->  print exp
    print $ simplifyn $ diff testexpr2 "x"
