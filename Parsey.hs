{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parsey where

import Control.Applicative
import Control.Monad
import Data.Default
import Data.Maybe

import Container

type Cause = String
type SuccessMetric = Int

data Parser error from to
	= Parser (Either error from -> Either error (from, to))

apply (Parser p) xs = p (Right xs)

instance Functor (Parser error from) where
	fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative (Parser error from) where
	pure r = Parser $ fmap (,r)
	(<*>) p q = Parser (\xs -> do
			xs <- xs
			(ys, a) <- apply p xs
			(zs, b) <- apply q ys
			return (zs, a b)
		)

instance Monad (Parser error from) where
	return = pure
	(>>=) p f = Parser (\xs -> do
			xs <- xs
			(ys, a) <- apply p xs
			apply (f a) ys
		)

class Error e where
	ezero :: e
	eplus :: e -> e -> e

instance Error [a] where
	ezero = []
	eplus = (++)

instance Error error => Alternative (Parser error from) where
	empty = Parser (\xs -> case xs of
			Right ys -> Left ezero
			Left  e  -> Left e
		)
	(<|>) (Parser p) (Parser q) = Parser (\xs -> case p xs of
			Right (ys, a) -> Right (ys, a)
			Left  e       -> case q xs of
				Right (zs, b) -> Right (zs, b)
				Left f        -> Left (eplus e f)
		)

instance Error error => MonadPlus (Parser error from) where
	mzero = empty
	mplus = (<|>)

id :: Alternative f => Parser error (f a) (f a)
id = Parser (\xs -> case xs of
		Right ys -> Right (empty, ys)
		Left  e  -> Left e
	)

(<<<) p (Parser q) = Parser (\xs -> do
		(ys, a) <- q xs
		(zs, b) <- apply p a
		if get zs == Nothing
			then return (ys, b)
			else Left $ show zs
	)

(>>>) = flip (<<<)

one = Parser (\xs -> case xs of
		Right x -> case get x of
			Just a  -> Right (rest x, a)
			Nothing -> Left ezero
		Left e  -> Left e
	)

end = Parser (\xs -> case xs of
		Right x -> case get x of
			Nothing -> Right (x, undefined)
			_       -> Left ezero
		Left e  -> Left e
	)

emap errf = Parser (\xs -> case xs of
		Right x -> Right (x, ())
		Left e  -> Left (errf e)
	)

ereplace e = Parser (\xs -> case xs of
		Right x -> Right (x, ())
		Left _  -> Left e
	)

error e = Parser (\xs -> case xs of
		Right x -> Left e
		Left e2 -> Left e2
	)

parseIf = mfilter

many1 = some
