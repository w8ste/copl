
module MonadicAE where

{- Interpreter for arithmetic expressions implemented in the monadic way -}

import Control.Monad.Identity

data Exp = Num Int 
         | Add Exp Exp 
         | Sub Exp Exp 
         | If0 Exp Exp Exp
      deriving Show

data Val = NumV Int 
     deriving Show
     
interp (Num n) = return (NumV n) 
  
interp (Add l r) = 
  do lv <- interp l
     rv <- interp r
     add lv rv
     
interp (Sub l r) = 
  do lv <- interp l
     rv <- interp r
     sub lv rv

interp (If0 c t e) = 
  do cv <- interp c 
     if0 cv t e     

add (NumV lv) (NumV rv) = return (NumV (lv + rv))
sub (NumV lv) (NumV rv) = return (NumV (lv - rv))
if0 (NumV c) t e = if c == 0 then interp t else interp e
        
prog1 = If0 (Sub (Num 2) (Num 2)) 
            (Add (Num 1) (Num 4))
            (Num 0)

interpSimple prog = runIdentity (interp prog)


main = do
   print $ interpSimple prog1
