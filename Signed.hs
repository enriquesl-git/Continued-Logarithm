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
   signum  ::                       t -> t
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
   signum 0 = 0
   signum a = if sgn a then 1 else -1


   -- Arithmetic with a boolean: True is like 1, False is like -1
   -- The Signed 'n' is scaled by the boolean
   -- (*.) n True   = id n
   -- (*.) n False  = negate n
   n *. True   = id n
   n *. False  = negate n
   (/.)        = (*.)
   
   n +. True   = (+ 1) n
   n +. False  = (subtract 1) n
   (-.) n      = (n +.) . not


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
  
  It is thought to store the single elements of non-positional numerical notation
  ("digits" with signum and exponent), or continued fractions and logarithms. 
  
  With this definition, zero can be positive or negative, as every other number. 
  Programmer can define if +0 == -0, or +0 /= -0

----------------------------------------------------------------------------}


data Term = T { 
   tSgn :: Bool,  -- Signum of Term
   tVal :: Int    -- Absolut value (unsigned integer) of Term
} deriving (Eq)
   
tNeg :: Term -> Term
-- Opposite of term
tNeg (T s a) = T (not s) a

incr, decr ::  Int -> Term -> Term
-- increments/decrements the absolute vale,
-- that is, the sign of 'a' modifies 'n', or 'n' is scaled by 'sgn a' and summed
-- like (+.) with the sign of 'a'
-- incr n a    = a + n *. sgn a
incr n (T sa va) = T sa (va + n)
decr n      = incr (-n)


----

instance Enum Term where

   fromEnum (T s a)  = fromEnum a
   toEnum x          = T True (toEnum ax)
      where (sx, ax) = sgnAbs x
   -- pred (T _ 0) = error "Predecessor of '0' or '0-', in Ternary.Term. "
   pred (T s 0) = T s 0    -- in order to be a safe function. 
   pred (T s a) = T s (a - 1)
   succ (T s a) = T s (a + 1)


{- Term as an instance of (Num, Ord, Signed)
-}

instance Ord Term where

   compare (T s1 x1) (T s2 x2) 
      | s1 && s2 = compare x1 x2
      | s1 = GT
      | s2 = LT
      | True = compare x2 x1


instance Signed Term where
   
   (+.) s False   = pred s  -- pred
   (+.) s True    = succ s  -- succ
   sgn (T s _)    = s
   abs (T _ a)    = T True a
   sgnAbs (T s a) = (s, T True a)


instance Num Term where

   fromInteger n  = T s (fromIntegral a)
      where (s, a) = sgnAbs n
   abs = fromIntegral . tVal
   -- signum      = (*. 1) . sgn  -- for just two values, {-1, 1} 
   signum (T _ 0)       =  0
   signum (T True  _)   =  1
   signum (T False _)   = -1
   negate      = tNeg
   (+) (T xs xv) (T ys yv) 
      | xs == ys  = T xs  (xv + yv)
      | xv >= yv  = T xs  (xv - yv)
      | True      = T ys  (yv - xv)
   (*) (T xs xv) (T ys yv) = T (xs == ys) (xv * yv)


instance Show Term where

   show (T True a) = ' ' : show a
   show (T _    a) = ' ' : show a ++ "-"


instance Read Term where

   readsPrec _ s = 
      let rd = reads s in
      [(T False a, t) | (a, r) <- rd, ("-", t) <- lex r] ++ 
      [(T True  a, t) | (a, r) <- rd, ("+", t) <- lex r] ++ 
      [(T True  a, t) | (a, t) <- rd]
      
   -- readsPrec _ s = 
      -- [(T False a, r) | (a, '-' : r) <- reads r] ++
      -- [(T True a,  r) | (a, '+' : r) <- reads r] ++
      -- [(T True a,  r) | (a,       r) <- reads r]

   -- readsPrec _ s = 
      -- do
      -- (a, '-' : r)   <- reads s
      -- return (T False a, r)

      -- readsPrec _ s = do
      -- (a, rest)   <- reads s
      -- (sg, r)     <- lex rest
      -- pure $ case sg of 
         -- "-"   -> (T False a, r)
         -- "+"   -> (T True a , r)
         -- _     -> (T True a , rest)


-- class BoolSigned t where
   -- {-# MINIMAL signed #-}
   
   -- signed :: Signed s => t -> s
   -- signed (a, b) = if b then a else -a

-- instance BoolSigned SInteger
-- instance BoolSigned Term

