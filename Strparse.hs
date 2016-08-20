{-# LANGUAGE NoMonomorphismRestriction #-}

module Strparse where

import Parsey
import Data.Char
import Control.Applicative

between start p end = do
	whitespace
	whchar start
	a <- p
	whchar end
	return a

parens p = between '(' p ')'

bracks p = between '{' p '}'

sqBracks p = between '[' p ']'

alphanum = parseIf isAlphaNum one

whitespace = many (char ' ' <|> char '\t' <|> char '\n')

char c = parseIf (==c) one

whchar c = whitespace >> char c

string []     = return []
string (s:ss) = do
	c  <- char s
	cs <- string ss
	return (c:cs)

keyword k = do
	whitespace
	string k

line = do
	l <- many (parseIf (/='\n') one)
	parseIf (=='\n') one <|> (if length l > 0 
		then end 
		else Parsey.error "Expected more lines")
	return l

emptyLine = parseIf (=='\n') one
