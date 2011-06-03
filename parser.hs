import Text.ParserCombinators.Parsec

data Expression = Number Integer
    | Symbol String
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Sin Expression
    | Cos Expression
    | Log Expression
    | Tan Expression

    deriving (Eq, Show)

run :: Show a => Parser a -> String -> IO ()
run p input =
    case (parse p "" input) of
            Left err -> do
                putStr "parse error at "
                print err
            Right x  -> print x

expr :: Parser Float
expr = do
    l <- factoo
    rs <- many ((do
                    char '+'
                    n <- factor
                    return $ (+) n
                    )
                <|>
                (do
                    char '-'
                    n <- factor
                    return $ flip (-) n
                ))
    return $ foldl (\x f -> f x) l rs

factor :: Parser Expression
factor = do
    l <- term1
    rs <- many ((do
                    char '*'
                    n <- term2
                    return $ Mul term1 term2
                    )
                <|>
                (do
                    char '/'
                    n <- term2
                    return $ Div term1 term2
                ))
    return $ foldl (\x f -> f x) l rs

term :: Parser Expression
term = do
    (do
        n <- many1 digit
        return $ Number $ read n
        )
    <|>
    (do
        char '('
        e <- expr
        char ')'
        return e
        )

