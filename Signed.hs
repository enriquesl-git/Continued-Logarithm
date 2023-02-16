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
-- It is first defined a type Term such that negative terms are 
-- represented with the '-' sign following the value (exponent of 3). 
--  
-- It should be an instance of 'Int', so it should 
-- implementmet methods for: Ord, Num, Integral 
-- 
-- 
-----------------------------------------------------------------------------


module Signed where

import Prelude hiding (abs, signum, negate)
infixl 7 *.
infixl 6 +., -.

-- class (Integral t, Read t) => Signed t where -- Integral not neccessary
class (Num t, Ord t, Read t) => Signed t where
   -- {-# MINIMAL sgn, (+.) #-}
   chSgn :: Signed c =>             c -> t -> t
   -- (+s), (-s) :: Signed c =>        c -> t -> t
   (*.), (+.), (-.) ::              t -> Bool -> t
   sgnAbs ::                        t -> (Bool, t)
   sgn ::                           t -> Bool
   abs  ::                          t -> t
   incr, decr ::                    t -> t -> t
   signum :: Signed c =>            t -> c

   -- Arithmetic with a boolean: True is like 1, False is like -1
   (*.) a True = a
   (*.) a _    = - a   
   (+.) a True = a + 1
   (+.) a _    = a - 1
   (-.) a b    = a +. not b
   -- (+.) a b    = a + 1 *. b
   -- (-.) a b    = a - 1 *. b  -- 'a -. b'

   -- Base function, gives False for negatives, True otherwise
   sgnAbs a    = (sa, a *. sa)   -- One step for both values, as in quotRem
      where sa = sgn a

   sgn   = (>= 0)
   abs   = snd . sgnAbs   -- abs is the rest of extracting sgn

   -- Useful functions, which avoids direct use of operators
   signum a    = 1 *. sgn a   -- = 0 +. sgn a, returns {1, -1}
   incr n a    = a + n *. sgn a
   decr n a    = a - n *. sgn a
   -- incr a      = a +. sgn a
   -- decr a      = a -. sgn a

   chSgn y     = (*. sgn y)   -- change sign
   -- setSgn y  = chSgn y . abs
   -- setSgn y x  = sgn y *. abs x 

   
instance Signed Integer
instance Signed Int
-- instance Show (Signed t)
-- instance Read (Signed t)


----

-- With this definition, zero can be positive or negative
data SInt = SInt { 
   sSgn :: Bool,   -- Signum of SInt
   sVal :: Word    -- Value (unsigned integer) of SInt
} deriving (Eq)
   
-- Opposite of term
tNeg :: SInt -> SInt
tNeg (SInt s v) = SInt (not s) v


---------------------------------------------------------------------------
{- Instances of SInt -}
---------------------------------------------------------------------------

instance Show SInt where

   show (SInt True v) = ' ' : show v
   show (SInt _    v) = ' ' : show v ++ "-"


instance Read SInt where

   readsPrec _ s = 
      let rd = reads s in
      [(SInt False v, t) | (v, r) <- rd, ("-", t) <- lex r] ++ 
      [(SInt True  v, t) | (v, r) <- rd, ("+", t) <- lex r] ++ 
      [(SInt True  v, t) | (v, t) <- rd]
      
   -- readsPrec _ s = 
      -- [(SInt False v, r) | (v, '-' : r) <- reads r] ++
      -- [(SInt True v,  r) | (v, '+' : r) <- reads r] ++
      -- [(SInt True v,  r) | (v,       r) <- reads r]

   -- readsPrec _ s = 
      -- do
      -- (v, '-' : r)   <- reads s
      -- return (SInt False v, r)

      -- readsPrec _ s = do
      -- (v, rest)   <- reads s
      -- (sg, r)     <- lex rest
      -- pure $ case sg of 
         -- "-"   -> (SInt False v, r)
         -- "+"   -> (SInt True v , r)
         -- _     -> (SInt True v , rest)


{- SInt as an instance of (Num, Ord, Signed)
-}

instance Ord SInt where
   -- compare absolutes!
   compare (SInt _ x1) (SInt _ x2) = compare x1 x2


instance Enum SInt where

   fromEnum (SInt s v)  = fromEnum v -- *. s
   toEnum x          = SInt True (toEnum ax) -- sx ax 
      where (sx, ax) = sgnAbs x
   pred (SInt _ 0) = error "Predecessor of +-1, in NonPosi3.SInt"
   pred (SInt s v) = SInt s (v - 1)
   succ (SInt s v) = SInt s (v + 1)





-- class BoolSigned t where
   -- {-# MINIMAL signed #-}
   
   -- signed :: Signed s => t -> s
   -- signed (a, b) = if b then a else -a

-- instance BoolSigned SInteger
-- instance BoolSigned SInt

   
   
