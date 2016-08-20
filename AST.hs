{-# LANGUAGE NoMonomorphismRestriction #-}

module AST where

type Name = String

data Lang = Decl Name Expr
	deriving Show

data Expr
	= Fun Expr [Expr]
	| Lambda [Name] Expr
	| Case Expr [(Expr,Expr)]
	| Var Name
	| Val Name

instance Show Expr where
	show (Fun a xs) = "\nFun: " ++ show' a ++ show'' xs
	show (Lambda a f) = "\nLambda: " ++ show'' a ++ show' f
	show (Case a xs) = "\nCase: " ++ show' a ++ show' xs
	show (Var a) = "\nVar: " ++ show a
	show (Val a) = "\nValue: " ++ show a

show' = unlines . fmap ('\t':) . lines . show
show'' = unlines . fmap ('\t':) . lines .('\n':) . show

simplify (Decl n x) = Decl n (simplify' x)

simplify' (Fun e []) = simplify' e
simplify' (Fun e xs) = Fun e (map simplify' xs)
simplify' (Lambda n e) = Lambda n (simplify' e)
simplify' (Case a xs) = 
	Case (simplify' a) (map (\(e1,e2) -> (simplify' e1, simplify' e2)) xs)
simplify' e = e
