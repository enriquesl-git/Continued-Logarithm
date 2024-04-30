-----------------------------------------------------------------------------
-- |
-- Module      :  Signed
-- Copyright   :  (c) Enrique Santos, 2017
-- License     :  see LICENSE
-- 
-- Maintainer  :  Enrique Santos
-- Stability   :  internal
-- Portability :  Portable
-- 
-- 'Signed' data type and its operations. 
-- 
-- It is defined a type class 'Signed', with functions to operate
-- with a Bool value, which represents the sign.
-- This way, the sign, which is binary, is separated from the value. 
-- It is like the scalar product, the scalar being the (bool, &&) group. 
--
-- Types Integer, Int, and others can be instances of type class Signed.
-- such that instances can be 'Int', 'Integer', etc,

-- so it should
-- implementmet methods for: Ord, Num, Integral 
-- 
-- 
-----------------------------------------------------------------------------


module Signed where

-- import Prelude -- hiding (abs, signum, negate)
infixl 7 *.
infixl 6 +., -.

-- class (Integral t, Read t) => Signed t where -- Integral not neccessary
class (Num t, Ord t, Read t) => Signed t where
   -- {-#  MINIMAL sgn, (*.), (+.)  #-}
   
   sgnAbs ::                        t -> (Bool, t)
   sgn ::                           t -> Bool
   abs  ::                          t -> t
   (*.), (/.), (+.), (-.) ::        t -> Bool -> t
   -- (+s), (-s) :: Signed c =>        c -> t -> t
   chSgn :: Signed c =>             c -> t -> t


   -- Base function 'sgn', gives False for negatives, True otherwise
   -- It can be viewed as the inverse function of '(*.)',
   -- which takes a sign and a value to give a signed result
   sgn = (>= 0)
   
   -- One step for both values, as in quotRem
   sgnAbs a = (sa, a *. sa)
      where sa = sgn a

   -- abs is the rest of extracting sgn
   abs = snd . sgnAbs


   -- Arithmetic with a boolean: True is like 1, False is like -1
   -- The Signed 'n' is scaled by the boolean
   (*.) n True   = id n
   (*.) n False  = negate n
   (/.)          = (*.)
   
   (+.) n True   = (+ 1) n
   (+.) n False  = (subtract 1) n
   (-.) n        = (n +.) . not


   -- change last argument sign from first argument sign
   -- like (*.) with the sign of 'y'
   chSgn y     = (*. sgn y)
   -- chSgn     = (*.) $ sgn
   
   -- setSgn y  = chSgn y . abs
   -- setSgn y x  = sgn y *. abs x 
   


instance Signed Integer
instance Signed Int
instance Signed Word

instance Signed Bool
--
instance Num Bool where

   fromInteger    = sgn
   abs _          = True
   signum         = id
   negate True    = False
   negate False   = True
   -- Question: (1 + 1) must be 1, or 0?
   -- xor, beter than (||), for the second case.
--    (+)            = (||)
   (+)            = (==) . not   -- definition of 'xor'
   (*)            = (&&)   -- any thing by zero is zero, 0 <==> False


{----------------------------------------------------------------------------
  Definition of a type class for short integers (word size): Term
  
  It is thought to store the single elements of non-positional numerical 
  representations (signum and exponent), or continued fractions and logarithms.
  
  With this definition, zero can be positive or negative, as every other number. 
  Programmer can define if +0 == -0, or +0 /= -0

----------------------------------------------------------------------------}


data Term = S { 
   sSgn :: Bool,  -- Signum of Term
   sAbs :: Int    -- Absolut value (unsigned integer) of Term
} deriving (Eq)
   
sNeg :: Term -> Term
-- Opposite of term
sNeg (S s a) = S (not s) a

incr, decr ::  Int -> Term -> Term
-- increments/decrements the absolute vale,
-- that is, the sign of 'a' modifies 'n', or 'n' is scaled by 'sgn a' and summed
-- like (+.) with the sign of 'a'
-- incr n a    = a + n *. sgn a
incr n (S sa va) = S sa (va + n)
decr n      = incr (-n)


----

instance Enum Term where

   fromEnum (S s a)  = fromEnum a
   toEnum x          = S True (toEnum ax)
      where (sx, ax) = sgnAbs x
   -- pred (S _ 0) = error "Predecessor of '0' or '0-', in Ternary.Term. "
   pred (S s 0) = S s 0    -- in order to be a safe function. 
   pred (S s a) = S s (a - 1)
   succ (S s a) = S s (a + 1)


{- Term as an instance of (Num, Ord, Signed)
-}

instance Ord Term where

   compare (S s1 x1) (S s2 x2) 
      | s1 && s2 = compare x1 x2
      | s1 = GT
      | s2 = LT
      | True = compare x2 x1


instance Signed Term where
   
   (+.) s False   = pred s  -- pred
   (+.) s True    = succ s  -- succ
   sgn (S s _)    = s
   abs (S _ a)    = S True a
   sgnAbs (S s a) = (s, S True a)


instance Num Term where

   fromInteger n  = S s (fromIntegral a)
      where (s, a) = sgnAbs n
   abs = fromIntegral . sAbs
   -- signum      = (*. 1) . sgn  -- for just two values, {-1, 1} 
   signum (S _ 0)       =  0
   signum (S True  _)   =  1
   signum (S False _)   = -1
   negate      = sNeg
   (+) (S xs xv) (S ys yv) 
      | xs == ys  = S xs  (xv + yv)
      | xv >= yv  = S xs  (xv - yv)
      | True      = S ys  (yv - xv)
   (*) (S xs xv) (S ys yv) = S (xs == ys) (xv * yv)


instance Show Term where

   show (S True a) = ' ' : show a
   show (S _    a) = ' ' : show a ++ "-"


instance Read Term where

   readsPrec _ s = 
      let rd = reads s in
      [(S False a, t) | (a, r) <- rd, ("-", t) <- lex r] ++ 
      [(S True  a, t) | (a, r) <- rd, ("+", t) <- lex r] ++ 
      [(S True  a, t) | (a, t) <- rd]
      
   -- readsPrec _ s = 
      -- [(S False a, r) | (a, '-' : r) <- reads r] ++
      -- [(S True a,  r) | (a, '+' : r) <- reads r] ++
      -- [(S True a,  r) | (a,       r) <- reads r]

   -- readsPrec _ s = 
      -- do
      -- (a, '-' : r)   <- reads s
      -- return (S False a, r)

      -- readsPrec _ s = do
      -- (a, rest)   <- reads s
      -- (sg, r)     <- lex rest
      -- pure $ case sg of 
         -- "-"   -> (S False a, r)
         -- "+"   -> (S True a , r)
         -- _     -> (S True a , rest)


-- class BoolSigned t where
   -- {-# MINIMAL signed #-}
   
   -- signed :: Signed s => t -> s
   -- signed (a, b) = if b then a else -a

-- instance BoolSigned SInteger
-- instance BoolSigned Term

