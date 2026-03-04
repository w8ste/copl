module Assignment where

import Data.Ratio

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
test2 =
  [ interp (AppE (FunE "x" (AddE (IdE "x") (NumE 3))) (NumE 5)) []
      == NumV 8,
    let NumV x = interp (AppE (FunE "x" (AddE (IdE "x") (NumE 3.2))) (NumE 5.4)) [] 
     in abs (x - 8.6) < 0.01,
    interp (AppE (FunE "x" (AddE (IdE "x") (NumE (2 % 5)))) (NumE (3 % 2))) []
      == NumV (19 % 10)
  ]

main = do
    print test2
-}
