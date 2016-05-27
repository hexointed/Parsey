{-# LANGUAGE MultiWayIf #-}
module Parser where

import Document
import Data.Char

type Parser a = String -> Maybe (a, String)

identifier :: Parser Tag
identifier = many $ parseIf isAlphaNum char

content :: Parser Data
content = many $ parseIf (not . isReserved) char

parseIf :: (a -> Bool) -> Parser a -> Parser a
parseIf f p input = case p input of
	Just (a, s) -> if
		| f a       -> Just (a, s)
		| otherwise -> Nothing
	Nothing     -> Nothing

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) p q input = case p input of
	Just (a, s) -> Just (a, s)
	_           -> case q input of
		Just (a', s') -> Just (a', s')
		_             -> Nothing

isReserved :: Char -> Bool
isReserved c
	| c == '['  = True
	| c == ']'  = True
	| c == '{'  = True
	| c == '}'  = True
	| otherwise = False

char :: Parser Char
char input = 
	if length input > 0 
		then Just (head input, tail input) 
		else Nothing

many :: Parser a -> Parser [a]
many p input = case p input of
	Just (s, rest) -> Just (s : fst next, snd next)
		where
			Just next = many p rest
	Nothing        -> Just ([], input)

doc :: Parser Document
doc input = case identifier input of
	Just (id, '{':s) -> case many doc s of
		Just (a, '}':s') -> Just (Div id a, s')
		_                -> Nothing
	Just (id, '[':s) -> case content s of
		Just (a, ']':s') -> Just (Content id a, s')
		_                -> Nothing
	_                -> Nothing
