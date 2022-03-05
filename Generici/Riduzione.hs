{-# OPTIONS_GHC -fforce-recomp #-}

module Generici.Riduzione where

data LExpr = Var String        -- Variable
         | App LExpr LExpr    -- Funktionsapplikation
         | Lam String LExpr   -- Lambda-Abstraktion


newVar :: [String] -> String
newVar ls = go "z"
  where go s | filter (==s) ls == [] = s
             | otherwise = go (s++"'")


subst :: LExpr -> String -> LExpr -> LExpr
subst m x (App e1 e2) = App(subst m x e1) (subst m x e2)
subst m x (Var str) | str == x = m
                    | otherwise = Var str
subst m x (Lam str e) | str == x = Lam str e
                      | str/= x && not (elem  str (free m))   = Lam str (subst m x e)
                      | otherwise = subst m x ((\y-> Lam y (subst (Var y) str e)) (newVar (free m)))

free :: LExpr -> [String]
free = error ""

e = Lam "y" (App (Var "f") (App (Var "x") (Var "y")))
m = App (Var "g") (Var "y")

lazy :: LExpr -> LExpr
 lazy  = flip go []
   where
         go (App e1 e2) ls   = go e1 (e2:ls)
         go (Lam y e) (l:ls) = go (subst l y e) ls
         go la@(Lam y e) []  = la
         go v@(Var _)    ls  = foldl App v ls
