module BExpInterp where

data BExp = T | F | Not BExp | Or BExp BExp 
  deriving (Show, Eq)

data BEValue = Tv | Fv
  deriving (Show, Eq)


interp :: BExp -> BEValue

interp T = Tv
interp F = Fv
interp (Not e) = case interp e of
    Tv -> Fv
    Fv -> Tv
interp (Or e1 e2) = case (interp e1, interp e2) of
  (Tv, _) -> Tv
  (_, Tv) -> Tv
  _ -> Fv



main = print $ interp (Not (Or T F ))
