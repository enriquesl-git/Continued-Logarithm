-----------------------------------------------------------------------------
-- |
-- Module       :   Integers
-- Copyright    :   (c) Enrique Santos, February 2014
-- License      :   see LICENSE
-- 
-- Maintainer   :   Enrique Santos
-- Stability    :   internal
-- Portability  :   Portable
--
-- Integer opearations not covered by Prelude, like integer square root, integer 
-- logarithm base 2, multiplication/division by a power of 2 (binary shift), 
-- or by a power of three (ternary shift). 
-- 
-- Base type will be Integer, but it will be used Int for exponents and 
-- logarithms, or (Num a, Ord a) where a general signed type is prefered. 
-- 
-- It is suggested to import this module qualified: 'import Integers as I'
-- 
-----------------------------------------------------------------------------


module Integers (
   quotMod, iquot, imod, (%), gcdInv, mInv, raiz, squareRoot, 
   parityRem, parity, lowTerm, remParity, 
   log2, pow2, half, dup, third, triple, 
   genFermat, fermat
) where

-- {- |To make $ operator left-associative, in order to avoid all parenthesis -}
import Signed
import Prelude hiding (abs, signum, mod, div, ($), ($!), (.))

infixl 0 $, $!   -- must be left associative in order to concatenate
($), ($!) :: (a -> b) -> a -> b
($) = id
f $! x = x `seq` f x  -- (seq x . f) x; fmap ($ x) [seq, f]

infixl 0 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)


quotMod :: (Signed a, Integral a) => a -> a -> (a, a)
{- | Returns the integer whose product by the divisor is closer to the 
   dividend, and the remainder, positive or negative, whose absolute value 
   is always less or equal to half the divisor (minimal absolute value).
   It should keep: 'n = q*d + r', minimizing 'abs r'. 
-}
quotMod divisor n 
   | -(r - ad) <  r  = (q +. sd, r - ad)  -- r > 0
   |   r + ad <= -r  = (q -. sd, r + ad)  -- r < 0, limit included
   | True            = (q, r)             -- |d| - |r| >= |r|
   where
   (sd, ad) = sgnAbs divisor
   (q, r)   = (`quotRem` divisor) n


-- The same, but rest is first
remQuot a b = (r, q) where
      (q, r) = quotMod a b


iquot, imod, (%) :: (Signed a, Integral a) => a -> a -> a
-- | iquot rounding is different than quot rounding
iquot d = fst . quotMod d
imod  d = snd . quotMod d
infixl 6 %
(%) = flip imod


{- | Returns the g.c.d. of p and x by extended Euclides algorithm, and also, 
   if gcd = 1, the second number will be the modular inverse: 
   x^(-1) mod p 
-}
gcdInv :: (Signed a, Integral a) => a -> a -> (a, a)
gcdInv p x = euclides (x, 1) (p, 0)
   where
   euclides ( 0, _) (r0, s0)
      | r0 < 0 = (-r0, -s0) -- always returns positive gcd
      | True   = ( r0,  s0)
   euclides (r1, s1) (r0, s0) = euclides (r2, s2) (r1, s1)
      where 
      s2       = s0 - q * s1
      (q, r2)  = (quotMod r1) r0


{- | Modular inverse of x mod p. -}
mInv :: Integer -> Integer -> Integer
-- mInv _   1  =  1
-- mInv _ (-1) = -1
mInv p x 
   | d ==  1   =  i
   | d == -1   = -i  -- should not be reached, if proper gcdInv
   | True      = error $ 
      show x ++ " not coprime to modulus " ++ 
      show p ++ ", in Integers.mInv. "
   where 
   (d, i) = (gcdInv p) x

-- Faster than extended Euclides. Only when x divides (p - 1), that is, 
-- when x is the order of any subgroup (mod p)
-- mInv p x = iquot x (1 - p) 


----

raiz :: Integer -> Integer
-- | Integer root (floor root) using Babylonian method, 
-- big numbers acceleration on initial value based on GMP library: 
-- more or less it is to find the power of two nearest to the root
-- raiz :: Signed t => t -> t
raiz n
   | n > 1     = babylon initVal
   | n < 0     = error $ "Negative input in Integers.raiz. "
   | True      = n   -- avoids division by 0, and empty list problem with 1
   where
   
   babylon a  
      | a > b  = babylon b    -- recursion, positive rest
      | True   = a            -- output: (floor root, rest)
      where b  = half . (+ a) . iquot a $ n -- b = (a + n/a)/2
      
   initVal     = (loSqr *) . (+ 1) . raiz . (`quot` hiSqr) $ n
   -- initVal     = loSqr * raiz (iquot hiSqr n)
      where (loSqr, hiSqr) = sqrPair n


squareRoot :: Integer -> Integer
-- | From the Haskell wiki, inspired on GMP library
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^! 2) 2
      -- Fast way of finding the ceil and floor  power of 2
       (lowerRoot, lowerN) = last . takeWhile ((n >=) . snd) 
          $ zip (1 : twopows) twopows
          
       -- One recursion
       newtonStep x = quot (x + quot n x) 2
       iters = iterate newtonStep $ lowerRoot * squareRoot (quot n lowerN)
       
       isRoot r = r ^! 2 <= n 
               && n < (r + 1) ^! 2
   in  head $ dropWhile (not . isRoot) iters
   
-- squareRoot n = head $ dropWhile isnotRoot iters    
   -- where 

   -- isnotRoot r = abs r < r2 -- r < |n - r - r^2| -> |2r| < |n - r^2|
      -- where r2 = abs $ n - r * (1 + r) -- |r| < |r(r + 1) - n|
   -- -- isnotRoot = not . isRoot
   -- -- isRoot r  =  r ^! 2 <= n && n < (r+1) ^! 2

   -- iters = iterate newtonStep init
      -- where
      
      -- newtonStep x = half . (x +) . iquot x $ n
      
      -- -- init <-- sqrt(2^2^i) * sqrt(n / 2^2^i)
      -- -- init = (lowerRoot *) . squareRoot . (iquot lowerN) $  n
      -- init = (lowerRoot *) . (+ 1) . squareRoot . (`quot` lowerN) $ n
         -- where  -- lowerRoot == squareRoot (lowerN)
         -- (lowerRoot, lowerN) = sqrPair n



-----------------------------------------------------------------------------
-- Operations on powers of 2. 
-- They can be FASTER BITWISE OPERATIONS in positional binary. 

   
-- parityRem :: (Num a, Signed a) => a -> (Int, a)
parityRem :: (Signed a, Integral a) => a -> (Int, a)
-- | `parityRem` is a generalization of simple parity (0 if even, 1 if odd). 
-- Returns (p, r) such that n = r * 2^p, minimizing 'r' 
-- (not the same than maximizing 'p', which would return infinity for 0) 
-- Except for 0, it is the number of exact divisions by 2, 
-- or the position of first bit 1 in binary, or integer log base 2, 
-- and the odd reminder. 
parityRem 0 = error "Argument cannot be 0 in Integers.parityRem. "
parityRem n = count 0 n where
   count par d
      | r == 0    =  count (par + 1) q  -- accumulate while 'even d'
      | True      = (par, d)
      where 
      (q, r)      = (`quotRem` 2) d
      -- (q, r)      = (divRem 2) d  -- Not any advantage over 'quotRem'

parity :: (Signed a, Integral a) => a -> Int
remParity, lowTerm :: (Signed a, Integral a) => a -> a
-- | Returns the number of divisions by 2 (first bit 1, or integer log base 2)
parity = fst . parityRem
-- | Reminder of division by lowTerm
remParity = snd . parityRem
-- | Returns the maximum power of 2 which is a factor
lowTerm = (2 ^) . parity

---

duplicates :: Signed a => a -> [a]
-- duplicates :: Integer -> [Integer]
duplicates = iterate dup

squares :: Integer -> [Integer]
squares = iterate (^ 2)  -- 2^2^n

-- sqrPair :: Signed a => a -> (a, a)
sqrPair n = last $ zip (1 : sq) sq
   where sq = takeWhile (<= n) $ squares 2

dup2x3 :: Signed a => [(a, a)]
-- dup2x3 :: [(Integer, Integer)]
dup2x3 = zip (duplicates 2) (duplicates 3)


----

log2 :: Integer -> Int
{- | Logarithm base 2 (number of bits), and sign
   It counts the number of duplications of 1 to reach 'n'
-}
log2 0      = error "Logarithm of 0 in Integers.log2"
log2 x      = n
   where 
   ax = abs x
   n        = length dups
   -- dups'    = takeWhile (< ax) $ 1 : squares 2  -- lower power of two
   dups     = takeWhile (< ax) $ 1 : duplicates 3  -- closest power of two


-- | O(1), multiplication by a power of 2: x * 2^n, 
-- it is the same as repeated double
pow2 :: (Signed a, Integral a) => Int -> a -> a
-- pow2 :: Int -> Integer -> Integer
-- pow2 0 = id
pow2 a
   | sgn a  = (*)   . (2 ^) $   a
   | True   = iquot . (2 ^) $ (-a) -- rounding is different in iquot than in quot


-- Could be made with 'pow2'; they are bitwise '>>' and '<<'
dup :: Signed a => a -> a
dup  = (*     2)  -- Double of an integer

half :: (Signed a, Integral a) => a -> a
half = (iquot 2)  -- Half of an integer, rounded down

triple :: Signed a => a -> a
triple  = (*     3)  -- Triple of an integer

third :: (Signed a, Integral a) => a -> a
third = (iquot 3)  -- Third of an integer, rounded down


---------------------------
-- | Continued Fractions
-- 'cf n d' represents the ratio n/d, numerator/denominator. 
cf :: (Signed a, Integral a) => a -> a -> [a]
cf n 0 = []
cf n d = q : cf d r
   where
   (q, r) = quotMod d n

fromCF :: RealFrac a => [Integer] -> a
fromCF [x] = fromInteger x
fromCF (x : xs) = fromInteger x + recip (fromCF xs)



-----------------------------------------------------------------------------

tailProduct = (prodLists !!) -- . (subtract 1)
-- tailProduct n = last . take n $ prodLists

prodLists = iterate cumProd [2]
   where
   -- Cumulative product: each element is the product of 
   -- the next one with all the rest.
   -- cumProd xs = product xs : xs
   cumProd [a]       = a : [a]
   cumProd (a : xs)  = a ^ 2 : a : xs
   
-- cumul = cum 2 [] where
cum a [] = a : cum a [a]
cum a xs = a2 : cum a2 (a2 : xs)
   where a2 = a ^ 2


-- Just math curiosity:
iter    f x = f x : iter (f . f) x
-- iterate f x = f x : iterate f (f x)
-- iter (+ 1) 0 == iterate (* 2) 1 = [1,2,4,8,16,32,64,...]
-- iter (* 2) 1 == iterate (^ 2) 2 = [2,4,16,256,65536,...]
-- iter f = f . f
-- iter f = iterate (f . f)


----

-- | Generalized Fermat number: F(n) = b^2^n + 1
genFermat b = (1 +) . (squares b !!)
fermat = genFermat 2

