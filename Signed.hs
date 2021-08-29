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

data SInt = SInt Int Bool
   deriving (Eq)
valor (SInt v s) = v
signo (SInt v s) = s

data SInteger = SInteger Integer Bool
   deriving (Eq)
valo (SInteger v s) = v
sign (SInteger v s) = s

-- instance BoolSigned SInteger
-- instance BoolSigned SInt

class BoolSigned t where
   -- {-# MINIMAL signed #-}
   
   -- signed :: Signed s => t -> s
   -- signed (a, b) = if b then a else -a

   
   
