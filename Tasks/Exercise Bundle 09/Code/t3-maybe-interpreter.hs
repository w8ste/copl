module Assignment where

import Data.List

-- Data Types

data FAE  = NumE Int
          | AddE FAE FAE
          | IdE String
          | FunE String FAE
          | AppE FAE FAE 
  deriving (Show, Eq)

data FAEValue = NumV Int
              | ClosureV String FAE Env
  deriving (Show, Eq)

type Env = [(String, FAEValue)]

-- Interpreter

interp :: FAE -> Env -> FAEValue

interp (NumE n) _ = NumV n

interp (AddE le re) env = NumV (lv + rv)
  where (NumV lv) = interp le env
        (NumV rv) = interp re env

interp (IdE id) env = v
  where (Just v) = lookup id env

interp (FunE id body) env = ClosureV id body env

interp (AppE fe ae) env =
  let (ClosureV id body fenv) = interp fe env
      av = interp ae env
      newenv = (id,av):fenv
  in interp body newenv

-- **** Tests ****
-- uncomment and run the following tests:

{-
test3 =
  [ interp (AppE (FunE "x" (AddE (IdE "x") (NumE 3))) (NumE 5)) []
      == Just (NumV 8),
    interp (AppE (AppE (FunE "x" (FunE "y" (AddE (IdE "x") (IdE "y")))) (NumE 5)) (NumE 3)) []
      == Just (NumV 8),
    interp (IdE "x") []
      == Nothing,
    interp (AppE (NumE 6) (NumE 5)) []
      == Nothing,
    interp (AddE (FunE "x" (IdE "x")) (NumE 5)) []
      == Nothing,
    interp (AddE (FunE "x" (IdE "y")) (NumE 8)) []
      == Nothing
  ]

main = do
  print test3
-}
