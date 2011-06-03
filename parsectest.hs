import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input =
    case (parse p "" input) of
            Left err -> do
                putStr "parse error at "
                print err
            Right x  -> print x

expr :: Parser Float
expr = do
    l <- factor
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

factor :: Parser Float
factor = do
    l <- term
    rs <- many ((do
                    char '*'
                    n <- term
                    return $ (*) n
                    )
                <|>
                (do
                    char '/'
                    n <- term
                    return $ flip (/) n
                ))
    return $ foldl (\x f -> f x) l rs

term :: Parser Float
term = do
    (do
        n <- many1 digit
        return $ read n
        )
    <|>
    (do
        char '('
        n <- expr
        char ')'
        return n
        )

main = do
    run expr "1+2*3/6-(12*12-8/4)"
