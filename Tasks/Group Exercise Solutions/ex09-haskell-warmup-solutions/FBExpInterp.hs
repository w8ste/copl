module FBExpInterp where

import Data.List

data FBExp = T | F | Not FBExp | Or FBExp FBExp | Id String | Fun String FBExp | App FBExp FBExp
  deriving (Show, Eq)

data FBEValue = Tv | Fv | ClosureV String FBExp Env
  deriving (Show, Eq)

type Env = [(String, FBEValue)]

interp :: FBExp -> Env -> FBEValue
interp T _ = Tv
interp F _ = Fv
interp (Not e) env = case interp e env of
  Tv -> Fv
  Fv -> Tv
interp (Or e1 e2) env = case (interp e1 env, interp e2 env) of
  (Tv, _) -> Tv
  (_, Tv) -> Tv
  _ -> Fv 
interp (Fun param body) env = ClosureV param body env
interp (Id name) env = 
  let (Just value) = lookup name env in value
interp (App fun argument) env =
  let (ClosureV paramName body closureEnv) = interp fun env in
  let paramValue = interp argument env in
  let newEnv = (paramName, paramValue) : closureEnv in
  interp body newEnv


main = print $ interp (App (Fun "x" (Not $ Or (Id "x") F )) (Or T F) ) []
