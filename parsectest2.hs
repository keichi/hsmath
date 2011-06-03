import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

run :: Show a => Parser a -> String -> IO ()
run p input =
    case (parse p "" input) of
            Left err -> do
                putStr "parse error at "
                print err
            Right x  -> print x

expr :: Parser Float
expr = foldr ($) <$> factor <*> many (add <|> sub)
    where add = ((+) <$> (char '+' *> factor))
          sub = (flip (-) <$> (char '-' *> factor))

factor :: Parser Float
factor = foldr ($) <$> term <*> many (mul <|> div)
    where mul = ((*) <$> (char '*' *> factor))  
          div =  (flip (/) <$> (char '/' *> factor))

term :: Parser Float
term = number <|> paren
    where number = (read <$> many1 digit )
          paren =  (char '(' *> expr <* char ')')

main = do
    run expr "1+2*3/6-(12*12-8/4)"
