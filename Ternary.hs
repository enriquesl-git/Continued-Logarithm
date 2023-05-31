-----------------------------------------------------------------------------
-- |
-- Module      :  Ternary
-- Copyright   :  (c) Enrique Santos, June 2022
-- License     :  see LICENSE
-- 
-- Maintainer  :  Enrique Santos
-- Stability   :  internal
-- Portability :  Portable
-- 
-- Nonpositional ternary ('Ternary') data type and its operations.
-- 
-- It is first defined a type Term such that negative terms are
-- represented with the '-' sign following the value (exponent of 3). 
--  
-- Then, the type Ternary is defined as a list of Term:
-- An example is: 377 = NT [6,5-,4-,3-,0-]
 
-- It should be an instance of 'Integral' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module Ternary (
   Term(..), Ternary(..), PairNT, -- data types
   half, dup, sgn, -- from imported modules
   i2terms, terms2i, oneTerm, pair2i, -- conversion
   neg, add, sub, -- arithmetic & comparation
   pw3, ntTripl, ntThird, mul, divStep, sqr, npSqr,   -- quarter, geometric
   -- sqrtStep, sqrtRem, sqrtMod,  -- inverse
) where

-- import Prelude hiding (abs, signum)
import Integers -- as I
import Signed as S
import Data.Foldable
-- import Data.List -- unfold is not in Data.Foldable
-- import Data.Sequence   -- better than List, faster
-- import qualified Data.Sequence as Sequence

default ()



--------------------------------------------------------------
-- 
-- | Continued Logarithm (CL) == Differential Non-positional Ternary
--   Accumulated C. Logarithm == Non-positional Ternary (NB) 
-- 
---------------------------------------------------------------------------

{- | Ternary will be an instance of:
   (Ord, Num, Integral, Signed, Num, Integral, Real, Foldable?) -}
newtype Ternary = NT [Term] deriving (Eq, Show, Read)

-- type NT = Ternary   -- just for brevity
type PairNT = (Ternary, Ternary)

pair2i :: PairNT -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)


---------------------------------------------------------------------------
{- Instances of Ternary -}
---------------------------------------------------------------------------

instance Ord Ternary where

   (<=) (NT (S sx ax : xs)) (NT (S sy ay : ys))
      | sx /= sy  = sy
      | ax /= ay  = sy && ax < ay
      | True      = NT xs <= NT ys
   (<=) (NT (S sx _ : _)) _  = not sx
   (<=) _ y  = sgn y


instance Enum Ternary where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Signed Ternary where

   (+.) a s          = a + NT [S s 0] -- a +- 1
   sgn (NT [])       = True
   sgn (NT x)        = sSgn $ last x


instance Num Ternary where

   fromInteger       = NT . i2terms
   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NT x)     = NT (neg x)
   (+) (NT x) (NT y) = NT (add x y)
   (*) (NT x) (NT y) = NT (mul x y)


instance Integral Ternary where

   toInteger (NT t) = terms2i t

   {- Euclidean recursive division, minimum absolute rest -}
   divMod _ 0  = error "Division by 0, in Ternary.divMod. "
   divMod rs ds
      | end    = (0, rNext) 
      | True   = (qNew, rNew) 
      where
      end   = sAbs qNext == 0   -- rs < ds  ==>  qNew == q
      qNew  = NT [qNext] + q    -- accumulates q, without passing an accumulator
      (q, rNew)      = divMod rNext ds
      (qNext, rNext) = divStep rs ds

   {- | Returns quotient with positive rest always -}
   quotRem n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = divMod n d


instance Real Ternary where

   toRational = toRational . toInteger


-- instance Foldable Ternary where

   -- foldMap f (NT x) = NT (foldMap f x)

   -- foldr f z (NT x) = NT (foldr f z x)


---------------------------------------------------------------------------
{- = 
   Conversion from and to Decimal base notation. 
   Continued Logarithm == continued product, by Euler transform
   Here it is really cLog(x + 1) 
-}
---------------------------------------------------------------------------

{- | From non-positional ternary to Integer, admits negative terms,
   it also defines the recursive homographic transformation. 
   Used in Integral instance. -}
terms2i :: [Term] -> Integer
terms2i = foldr transform 0 where 
   transform (S s a) 
      = (+) $ (*. s) . (^ a) $ 3
      -- = (+) $ (*. s) . (3 ^) $ a
      -- = (+) $ (*. s) $ 3 ^ a

{- | Gives in each step the closest power of 3. 
   That way, truncation will give the best approximation. 
   
   If odd subtract 1 and write 0, if even divide by 3 and count. 
   Write times counted. 
     x0 = 3^a0*sgn + x1; [a0,a1,...,an] == 3^a0 + 3^a1 + ... + 3^an
   Should be an 'unfold' or equivalent to it.
   Used in Num instance. -}
i2terms :: Integer -> [Term]  -- is: Integer Signed
i2terms = i2terms' 0 where -- reverse $
   i2terms' _ 0 = []
   i2terms' a n
      | r == 0    = i2terms' (1 + a) q
      | True      = S (sgn r) a : i2terms' (1 + a) q
      where (q, r) = quotMod 3 n

oneTerm :: Int -> Ternary
oneTerm x
   | sgn x  = NT [S True x]
   | True   = error "Negative argument in Ternary.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

{- | Addition, for both, positive and negative terms 
-}
add, sub :: [Term] -> [Term] -> [Term]
add xs [] = xs
add [] xs = xs
add [y] (x:xs)
   | x == sNeg y     = xs
   | x == y          = sNeg y : add [succ y] xs    -- carry
   | sAbs x < sAbs y = x : add [y] xs
   | True            = y : x : xs
add (y:ys) xs        = add [y] $ add ys xs

sub = add . neg

neg :: [Term] -> [Term]
neg = fmap sNeg

{- | Carry for arithmetic operations, 
   carry is to reorganize terms to avoid intermediate repeated or 
   consecutive terms, thus minimizing the number of terms. It should 
   get each truncation the closest possible to the real value. Final 
   consecutive terms can stay only if previous term has the same sign. 
   O(log n) because recursion is not all of the times, 
   so it will stop recurring in a length proportional to log n, as much.
-}
{-
carry :: [Term] -> [Term]
carry (a : b : xs)
   -- | a > succ b      = a : carry (b : xs)
   | negA == b       = carry xs
   | a == b          = sucA : carry (negA : xs)
   -- | negA == sucB    = carry (negB : xs)
   -- | a == sucB       = carry $ sucA : carry (negB : xs)
   | a < b           = carry $ b : a : xs    -- sort
   where
   negA = sNeg a
   negB = sNeg b
   sucA = succ a
   sucB = succ b
carry xs = xs
-}

{- | carryAll is the recursive application of carry. Not used -}
-- carryAll :: [Term] -> [Term]
-- carryAll (x : xs) = carry $ x : carryAll xs
-- carryAll _        = []


-----------------------------------------------------------------------
-- * Base operations: bit-shift, triplicate, third,
---------------------------------------------------------------------------

-- | Triplicate: multiply by 3, faster than add; 
-- third: like dividing by 3; 
-- they are a particular case of product by power of 2: pw3
ntTripl, ntThird :: Ternary -> Ternary
ntTripl (NT x)   = NT $ pw3   1  x  -- takes more comparisons of conditions
ntThird (NT x)   = NT $ pw3 (-1) x

ntDup x    = x + x
ntQuad x   = x + ntTripl x

-- ntHalf, ntQuarter :: [Term] -> [Term]

-- ntHalf (NT n) = NT . h $ reverse n
   where
h [a, b]
   | a == 0    = [] -- a : b : xs
   | sgn a /= sgn b    = reverse [b .. pred a] 
   -- | True = a : fmap negate $ reverse [b .. pred a] 

   -- | True      = add [a1] $ h (a1 : b : xs)
   -- where [a1] = pw3 (-1) [a]
-- h [0] = []
-- h [a] = pw3 (-1) [a]
-- h [] = []

-- ntQuarter = ntHalf . ntHalf

-- quarter NT x = q x
--    where
--    (a : b : c : d : xs) = reverse x
--    [a1] = pw3 (-1) [a]
--    q x
--       | a > b  = a1 : a1 : a1 : b : c : d : xs
--       | a > c  = b : a1 : a1 : a1 : c : d : xs
--       | a > d  = b : c : a1 : a1 : a1 : d : xs
--       | True   =  a, q xs
--
--       | a == b && a == c && a == div= a : xs


-- | Multiplication by power of 3, MULtiple TRIPLication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
pw3 :: Int -> [Term] -> [Term]
pw3 = fmap . incr
-- pw3  = fmap . pw3term
   -- where
   -- pw3term n (S s v)
      -- | sgn vn    = S s $ vn -- conversion to natural
      -- where vn    = v + n    -- conversion from natural
   -- pw3term _ _    = error "Third of non-multiple of 3, in Ternary.pw3. "


-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

-- mul, mulF  :: Ternary -> Ternary -> Ternary
mul, mulE  :: [Term] -> [Term] -> [Term]

-- product by a one term element, equivalent to pw3
-- mul [S sa aa] [S sb ab] = [S (sa == sb) (aa + ab)]
-- mul [S sx ax] (S s v : ys) = S (sx == s) (ax + v) : mul [S sx ax] ys
mul [S True ax] = pw3 ax
mul [S _    ax] = pw3 ax . neg 
-- mul xs [y] = mul [y] xs

-- general product, several methods can be chosen
mul x = mulE x
-- mul x y = mulS x y
-- mul x y = mulF x y

{- | Multiplication
   Egyptian method, O(n^2) -}
mulE xs (S sb ab : ys) = add (mul xs [S sb ab]) (mulE xs ys)
mulE _ _  = []

{- | Straightforward multiplication, O(n^2) -}
mulS (S sa aa : xs) (S sb ab : ys) = 
   (S (sa == sb) (aa + ab) :) 
   . add (mul xs [S sb ab])
   . add (mul [S sa aa] ys)
   $ mulS xs ys
mulS _ _ = []

{- | We define mulF to be x*y = 1/4(a^2 - b^2),
   where a = x + y; b = x - y ('F' for 'Fast' or 'Fermat').
   But ... quarter is not simple in Ternary.
   Complexity is mainly on sqr, as the rest of the algorithm is <~ O(size). -}

-- mulF xs ys = quarter $ sub sqSub sqSum   -- 1/4(A^2 - B^2)
--    where sqSub = sqr $ sub xs ys   -- squared difference: (ys - xs)^2
--          sqSum = sqr $ add xs ys   -- squared sum:        (ys + xs)^2


{- | Square, O(n^2), faster than @mul xs xs@ -}
sqr :: [Term] -> [Term]

-- | recursive version:  x^2 + (2*x*xs + xs^2)
-- supposedly faster, as the term x^2 is calculated appart in a simpler way
sqr (S s x : xs)
   = (S True (dup x) :) -- x^2 +
   . add (sqr xs)       -- xs^2 +
   . mul xs             -- xs *
   $ [S (not s) x, S s (x + 1)]  -- (x + x), in ternary

-- | non-recursive version:  x^2 + (2*x + xs)*xs
-- sqr (S s x : xs) =
--    (S True (dup x) :)
--    . mul xs
--    $ S (not s) x: S s (x + 1) : xs

sqr _ = []

-- npSqr :: Ternary -> Ternary
npSqr (NT x) = NT (sqr x)
-- npSqr (NT [S _ x]) = oneSInt (dup x) -- single term square


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------


divStep :: Ternary -> Ternary  -> (Term, Ternary)
{- | Single division step. (Not used yet)
   Returns  SINGLE TERM  quotient which minimizes absolute rest, 
   and the new rest; keeps 'r + q*d' = constant, where: 
   d = divisor; q = quotient; r = rest or module. -}
divStep _ 0 = error "Divided by 0 in Ternary.divStep. "
divStep 0 _ = (S True 0, 0)
divStep (NT r) (NT d) = minimumBy comp candidates
   where
   -- positive rNew, reverse for optimization of steps 
   candidates  = reverse $ (S True 0, NT r) : fmap qrPair digits
      where
      (S sr ar, S sd ad)  = (last r, last d)
      digits   = [0 .. 1 + ar - ad]
      
      qrPair q = (qNew, NT rNew)
         where 
         qNew  = S (sr == sd) q
         rDif  = mul [qNew] d
         rNew  = sub rDif r
   
   comp (_, ra) (_, rb)
      | res == EQ && sgn rb   = GT  -- in order to prioritize positive rest
      | True                  = res
      where 
      res = compare (S.abs ra) (S.abs rb)
      

-- divStep (NT r) (NT d) =
--    where
--    (r, d) = ()
--



---------------------------------------------------------------------------
   
