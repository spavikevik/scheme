-- Parser code written using Parsec
-- I will write comments for further reference.

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"' -- a string starts with an open quote
    x <- many (noneOf "\"") -- any character that is not a quote is part of the string
    char '"' -- a string ends with an end quote
    return $ String x -- wrap the string into Parser monad

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol -- first character is either a letter or a symbol
    rest <- many (letter <|> digit <|> symbol) -- it is then followed by many letters, digits, or symbols
    let atom = first:rest -- an atom starts with a letter or symbol and followed by the rest of an atom (letters, digits or symbols)
    return $ case atom of
        "#t" -> Bool True -- #t is True boolean
        "#f" -> Bool False -- #f is False boolean
        _    -> Atom atom -- everything else is an atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit -- a number is one or more digits; the function is lifted into the monad

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces -- a list is an expression separated by space(s)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces -- the first elements are like an ordinary list
    tail <- char '.' >> spaces >> parseExpr -- the last element starts with a dot
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\'' -- quoted expressions start with a single quote '
    x <- parseExpr
    return $ List [Atom "quote", x] -- it is an expression represented as data

parseExpr :: Parser LispVal
parseExpr = parseAtom -- an expression is either an atom
          <|> parseString -- or a string
          <|> parseNumber -- or a number
          <|> parseQuoted -- or a quoted expression
          <|> do char '('
            x <- try parseList <|> parseDottedList -- or a list
            char ')'
            return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)