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
   -- {-# MINIMAL sgn, (+.) #-}
   chSgn :: Signed c =>             c -> t -> t
   -- (+s), (-s) :: Signed c =>        c -> t -> t
   (*.), (/.), (+.), (-.) ::              Bool -> t -> t
   sgnAbs ::                        t -> (Bool, t)
   sgn ::                           t -> Bool
   abs  ::                          t -> t
   incr, decr ::                    t -> t -> t
   signum :: Signed c =>            t -> c

   -- Arithmetic with a boolean: True is like 1, False is like -1
   (*.) True   = id
   (*.) False  = negate
   (/.)        = (*.)
   (+.) True   = (+ 1)
   (+.) False  = subtract 1
   (-.)        = (+.) . not

   -- Base function, gives False for negatives, True otherwise
   -- It can be viewed as the inverse function of '(*.)',
   -- which takes a sign and a value to give a signed result
   sgnAbs a    = (sa, sa *. a)   -- One step for both values, as in quotRem
      where sa = sgn a

   sgn   = (>= 0)
   abs   = snd . sgnAbs   -- abs is the rest of extracting sgn

   -- Useful functions, which avoids direct use of operators
   signum      = (*. 1) . sgn   -- = 0 +. sgn a, returns {1, -1}
   -- increments/decrements the absolute vale,
   -- that is, the sign of 'a' modifies 'n'
   incr n a    = a + sgn a *. n
   decr n a    = a - sgn a *. n
   -- incr a      = sgn a +. a
   -- decr a      = sgn a -. a

   chSgn y     = (sgn y *.)   -- change sign
   -- setSgn y  = chSgn y . abs
   -- setSgn y x  = sgn y *. abs x 

   
instance Signed Integer
instance Signed Int
instance Signed Word

-- instance Signed SInt
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
  Definition of a type class for short integers (word size): SInt
  With this definition, zero can be positive or negative, 
  as every other number

----------------------------------------------------------------------------}


data SInt = S { 
   sSgn :: Bool,   -- Signum of SInt
   sAbs :: Word    -- Absolut value (unsigned integer) of SInt
} deriving (Eq)
   
sNeg :: SInt -> SInt
-- Opposite of term
sNeg (S s a) = S (not s) a


----

instance Num SInt where

   fromInteger n  = S s (fromIntegral a)
      where (s, a) = sgnAbs n
   abs = fromIntegral . sAbs
   signum 0    = 0
   signum n
      | sSgn n =  1
      | True   = -1
   negate      = sNeg
   (+)         = (+)
   (*)         = (*)


instance Show SInt where

   show (S True a) = ' ' : show a
   show (S _    a) = ' ' : show a ++ "-"


instance Read SInt where

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


{- SInt as an instance of (Num, Ord, Signed)
-}

instance Ord SInt where
   -- compare absolutes!
   compare (S _ x1) (S _ x2) = compare x1 x2


instance Enum SInt where

   fromEnum (S s a)  = fromEnum a
   toEnum x          = S True (toEnum ax)
      where (sx, ax) = sgnAbs x
   pred (S _ 0) = error "Predecessor of '0' or '0-', in NonPosi3.SInt. "
   pred (S s a) = S s (a - 1)
   succ (S s a) = S s (a + 1)



-- class BoolSigned t where
   -- {-# MINIMAL signed #-}
   
   -- signed :: Signed s => t -> s
   -- signed (a, b) = if b then a else -a

-- instance BoolSigned SInteger
-- instance BoolSigned SInt

