module Container where

import Data.Maybe

class Functor f => Container f where
	get :: f a -> Maybe a
	put :: a -> f a -> f a
	rest :: f a -> f a
	null :: f a
--	get null = Nothing
--	get (put a null) = Just a
--  get (rest a) /= get a
--  rest null = null
--  rest (put a null) = null
--	rest (put a (put b null)) 
--		= put a null
--	OR	= put b null

instance Container [] where
	get = listToMaybe
	put = (:)
	rest = tail
	null = []

merge a b = case get a of
	Just a' -> put a' (merge (rest a) b)
	Nothing -> b
