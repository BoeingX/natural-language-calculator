module Lib where

import Control.Applicative
import Data.Char
import Text.Trifecta
import qualified Data.Map as M

-- |Target AST
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Lit Integer
          deriving (Eq, Show)

-- |Evaluate AST
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b

-- |Parse arithmetic expression
-- See https://en.wikipedia.org/wiki/Syntax_diagram
-- for the BNF of arithmetic expressions
expr :: Parser Expr
expr = chainl1 term addOp

term :: Parser Expr
term = chainl1 factor mulOp

factor :: Parser Expr
factor = constant <|> parens expr

constant :: Parser Expr
constant = number <|> word

number :: Parser Expr
number = Lit <$> integer

addOp :: Parser (Expr -> Expr -> Expr)
addOp = add <|> sub

add :: Parser (Expr -> Expr -> Expr)
add =  token (choice [string "+", string "plus"]) 
    >> return Add

sub :: Parser (Expr -> Expr -> Expr)
sub =  token (choice [string "-", string "minus"]) 
    >> return Sub

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = mul

mul :: Parser (Expr -> Expr -> Expr)
mul =  token (choice [string "*", string "times"])
    >> return Mul

-- |Parse number in text form to numerical form
word :: Parser Expr
word = do
    n <- map toLower <$> token (some letter)
    case M.lookup n dict of
        Just n -> return $ Lit n
        Nothing -> fail "Only one to nine are supported"
    where dict = M.fromList $ zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..9]

-- |Wrap everything
opening :: Parser String
opening = choice [ token (string "what is")
                 , token (string "calculate")
                 ]

parse :: Parser Expr
parse = skipOptional opening >> expr
