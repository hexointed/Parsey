{-# LANGUAGE NoMonomorphismRestriction #-}

module Magma where

import Strparse
import Parsey
import Data.Char
import AST
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

type MParser to = Parser String String to

equal :: MParser Char
equal = whchar '='

identifier :: MParser String
identifier = do
	whitespace
	x <- parseIf isAlpha one 
	xs <- many alphanum
	ereplace "expected identifier"
	if isReserved (x:xs)
		then Parsey.error $ "Unexpected keyword: " ++ (x:xs)
		else return (x:xs)

literal :: MParser Expr
literal = do
	whitespace
	xs <- some (parseIf isDigit one <|> char '.')
	return (Val xs)

isReserved = flip elem ["case", "of", "if"]

function :: MParser Expr
function = do
	f <- argument
	xs <- many argument
	return (Fun f xs)

argument :: MParser Expr
argument = 
	literal <|>
	fmap Var identifier <|> 
	parens expression <|>
	lambda <|>
	caseof <|>
	Parsey.error "Unacceptable function or function argument"

expression :: MParser Expr
expression = 
	opr "," $
	opr "+" $
	opl "-" $ 
	opr "*" $ 
	opl "/" $ 
	opl "%" $ 
	opr ":" $
	opl "**" $ 
	opr "==" $
	function
	
opl = expr foldl1
opr = expr foldr1

expr fold p nxt = do
	whitespace
	f  <- nxt
	fs <- many $ do
		whitespace
		string p
		nxt
	return $ fold (\x y -> Fun (Var p) [x,y]) (f:fs)

operdef = do
	string "infix"
	d <- char 'l' <|> char 'r'
	char ' '
	prio <- many1 (mfilter isDigit one)
	char ' '
	op <- many1 (mfilter (\c -> c/=' ' && c /= '\t' && c /= '\n') one)
	case d of
		'l' -> return $ opl op
		'r' -> return $ opr op

decl = context >>> do
	id <- identifier
	equal
	st <- expression
	optional (char '\n')
	return $ Decl id st

context :: MParser String
context = do
	ts <- many (mfilter (=='\t') one)
	l  <- line
	ls <- many (mfilter (isPrefixOf $ '\t':ts) line)
	return $ unlines ((ts++l):ls)

lambda = do
	args <- sqBracks (many identifier)
	body <- expression
	return $ Lambda args body

caseof = do
	keyword "case"
	ex <- expression
	keyword "of"
	char '\n' <|> Parsey.error "expected newline after 'of'"
	matches <- some match
	return $ Case ex matches
			
match :: MParser (Expr, Expr)
match = context >>> do
	pat <- expression
	keyword "->"
	res <- expression
	optional (char '\n')
	return (pat,res)

tunit = do
	many (char '\n')
	d <- decl
	many (char '\n')
	return d

parse file = do
	f <- readFile file
	case apply (some tunit) f of
		Right ("", ast) -> putStrLn . show $ map simplify ast
		Right (xs, ast) -> case apply tunit xs of
			Right (ys, ast) -> putStrLn $ "Error: parse failed near " ++ ys
			Left error      -> putStrLn $ "Error: " ++ error
		Left error -> putStrLn $ "Error: " ++ error
