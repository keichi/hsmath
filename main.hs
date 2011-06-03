import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

data Expression = Number Integer
    | Symbol String
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Func String Expression

    deriving (Eq, Show)

expr :: Parser Expression
expr = foldr ($) <$> factor <*> many (add <|> sub)
    where add = Add <$> (char '+' *> factor)
          sub = flip Sub <$> (char '-' *> factor)

factor :: Parser Expression
factor = foldr ($) <$> term <*> many (mul <|> div)
    where mul = Mul <$> (char '*' *> factor)
          div = flip Div <$> (char '/' *> factor)

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
    Add l r     ->  eval l table + eval r table
    Sub l r     ->  eval l table - eval r table 
    Mul l r     ->  eval l table * eval r table
    Div l r     ->  eval l table / eval r table
    Func "sin" e    ->  sin $ eval e table
    Func "cos" e    ->  cos $ eval e table
    Func "log" e    ->  log $ eval e table
    Func "tan" e    ->  tan $ eval e table

isFuncOf :: Expression -> String -> Bool
isFuncOf expr name = case expr of
    Number num    ->  False
    Symbol s      ->  if s == name then True else False
    Add l r       ->  l `isFuncOf` name || r `isFuncOf` name
    Sub l r       ->  l `isFuncOf` name || r `isFuncOf` name
    Mul l r       ->  l `isFuncOf` name || r `isFuncOf` name
    Div l r       ->  l `isFuncOf` name || r `isFuncOf` name
    Func _ e      ->  e `isFuncOf` name

diff :: Expression -> String -> Expression
diff expr var = case expr of
    Number num    ->  Number 0
    Symbol s      ->  if s == var then Number 1 else Number 0
    Add l r       ->  Add (diff l var) (diff r var)
    Sub l r       ->  Sub (diff l var) (diff r var)
    Mul l r       ->  Add (Mul (diff l var) r) (Mul l (diff r var))
    Div l r       ->  Div (Sub (Mul (diff l var) r) (Mul l (diff r var))) (Mul r r)
    Func "sin" e         ->  Mul (Func "cos" e) (diff e var)
    Func "cos" e         ->  Sub (Number 0) (Mul (Func "sin" e) (diff e var))
    Func "log" e         ->  Div (diff e var) e
    Func "tan" e         ->  Div (diff e var) (Mul (Func "cos" e) (Func "cos" e))

simplify :: Expression -> Expression
simplify expr = case expr of
    --数値リテラル同士の計算
    Add (Number n1) (Number n2)     ->  Number (n1 + n2)
    Sub (Number n1) (Number n2)     ->  Number (n1 - n2)
    Mul (Number n1) (Number n2)     ->  Number (n1 * n2)
    Div (Number n1) (Number n2)     ->  Number $ floor (fromIntegral n1 / fromIntegral n2)

    --加減算の簡単化
    Add e (Number 0)        ->  e
    Add (Number 0) e        ->  e
    Add e1 e2 | e1 == e2    ->  Mul (Number 2) e1
    Sub e (Number 0)        ->  e
    Sub e1 e2 | e1 == e2    ->  Number 0

    --乗除算の簡単化
    Mul _ (Number 0)        ->  Number 0
    Mul (Number 0) _        ->  Number 0
    Mul e (Number 1)        ->  simplify e
    Mul (Number 1) e        ->  simplify e
    Div e (Number 1)        ->  e
    Div e1 e2 | e1 == e2    ->  Number 1
    Div (Number 0) e        ->  Number 0

    --シンボルと数値は簡単化できない
    Symbol _                        ->  expr
    Number _                        ->  expr

    --その他の場合は
    Add l r          ->     Add (simplify l) (simplify r)
    Sub l r          ->     Sub (simplify l) (simplify r)
    Mul l r          ->     Mul (simplify l) (simplify r)
    Div l r          ->     Div (simplify l) (simplify r)
    Func name e      ->     Func name (simplify e)
    _                ->     expr

simplifyn :: Expression -> Expression
simplifyn expr =
    if simplifiedExpr == expr
        then expr
        else simplifyn simplifiedExpr
    where simplifiedExpr = simplify expr

testexpr = Add (Add (Mul (Symbol "x") (Symbol "x")) (Symbol "x")) (Number 1)
testexpr1 = Mul(Number 1) (Add (Number 2) (Number 7))
testexpr2 = Mul (Symbol "x") (Func "log" (Symbol "x"))

main = do
    case parse expr "" "sin(x)/x" of
        Left error  ->  print error
        Right exp   ->  print $ simplifyn $ diff exp "x"
    print $ simplifyn $ diff testexpr2 "x"
