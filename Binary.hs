-----------------------------------------------------------------------------
-- |
-- Module      :  Binary
-- Copyright   :  (c) Enrique Santos, February 2019
-- License     :  see LICENSE
-- 
-- Maintainer  :  Enrique Santos
-- Stability   :  internal
-- Portability :  Portable
-- 
-- Nonpositional binary (Binary) data type and its operations.
-- 
-- It is used a type Term for digit terms, such that negative terms are
-- represented with the '-' sign following the value (exponent of 2). 
--  
-- Then, the type Binary is defined as a list of Term terms:
-- An example is: 377 = NB [9, 7-, 3-, 0]
 
-- It should be an instance of 'Integer' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module Binary (
   Term(..), Binary(..), PairNB, -- data types
   half, dup, sgn, -- from imported modules
   i2terms, terms2i, oneTerm, pair2i, -- conversion
   neg, carry, add, sub, -- arithmetic & comparation
   pw2, npDup, npHlf, sqr, mul, npSqr, divStep,   -- geometric
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
-- | Continued Logarithm (CL) == Differential Non-positional Binary
--   Accumulated C. Logarithm == Non-positional Binary (NB) 
-- 
---------------------------------------------------------------------------

{- | Binary will be an instance of:
   (Ord, Num, Integral, Signed, Num, Integral, Real, Foldable?) -}
newtype Binary = NB [Term] deriving (Eq, Show, Read)

-- type NB = Binary   -- just for brevity
type PairNB = (Binary, Binary)

pair2i :: PairNB -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)


---------------------------------------------------------------------------
{- Instances of Binary -}
---------------------------------------------------------------------------

instance Ord Binary where

   (<=) (NB (T sx ax : xs)) (NB (T sy ay : ys))
      | sx /= sy  = sy
      | ax /= ay  = sy && ax < ay
      | True      = NB xs <= NB ys
   (<=) (NB (T sx _ : _)) _  = not sx
   (<=) _ y  = sgn y


instance Enum Binary where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Signed Binary where

   (+.) a s          = a + NB [T s 0] -- a +- 1
   sgn (NB (x : _))  = tSgn x
   sgn _             = True


instance Num Binary where

   fromInteger       = NB . i2terms
   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NB x)     = NB (neg x)
   (+) (NB x) (NB y) = NB (add x y)
   (*) (NB x) (NB y) = NB (mul x y)


instance Integral Binary where

   toInteger (NB t) = terms2i t

   {- Euclidean recursive division, minimum absolute rest -}
   divMod _ 0  = error "Division by 0, in Binary.divMod. "
   divMod rs ds
      | end    = (0, rNext) 
      | True   = (qNew, rNew) 
      where
      end   = tVal qNext == 0   -- rs < ds  ==>  qNew == q
      qNew  = NB [qNext] + q    -- accumulates q, without passing an accumulator
      (q, rNew)      = divMod rNext ds
      (qNext, rNext) = divStep rs ds

   {- | Returns quotient with positive rest always -}
   quotRem n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = divMod n d


instance Real Binary where

   toRational = toRational . toInteger


-- instance Foldable Binary where

   -- foldMap f (NB x) = NB (foldMap f x)

   -- foldr f z (NB x) = NB (foldr f z x)


---------------------------------------------------------------------------
{- = 
   Conversion from and to Decimal base notation. 
   Continued Logarithm == continued product, by Euler transform
   Here it is really cLog(x + 1) 
-}
---------------------------------------------------------------------------

{- | From non-positional binary to Integer, admits negative terms,
   it also defines the recursive homographic transformation. 
   Used in Integral instance. -}
terms2i :: [Term] -> Integer
terms2i = foldr transform 0 where
   transform (T s a)
      = (+) $ (*. s) . (^ a) $ 2

{- | Gives in each step the closest power of 2 (the lesser when two 
   equals). That way, truncation will give the best approximation. 
   If odd subtract 1 and write 0, if even divide by 2 and count, 
   write times counted. 
   x0 = 2^a0*sgn + x1; [a0,a1,...,an] == 2^a0 + 2^a1 + ... + 2^an
   Should be an 'unfold' or equivalent to it.
   Used in Num instance. -}
i2terms :: Integer -> [Term]  -- is: Integer Signed
i2terms x = reverse $ i2terms' 0 x where 
   i2terms' _ 0 = []
   i2terms' a n
      | r ==  1   = T True  a : i2terms' (2 + a) q
      | r == -1   = T False a : i2terms' (2 + a) q
      | r ==  0   = i2terms' (2 + a) q 
      | True      = i2terms' (1 + a) (half n) -- 1 + dup q
      where (q, r) = quotMod 4 n

oneTerm :: Int -> Binary
oneTerm x 
   | sgn x  = NB [T True x]
   | True   = error "Negative argument in Binary.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

{- | Addition, for both, positive and negative terms 
-}
add, sub :: [Term] -> [Term] -> [Term]
add [] ys = ys
add xs (b : ys)
   | head xs <= b    = carry . (b :) . add xs $ ys
add xs ys            = add ys xs

sub = add . neg

neg :: [Term] -> [Term]
neg = fmap tNeg

{- | Carry for arithmetic operations, 
   carry is to reorganize terms to avoid intermediate repeated or 
   consecutive terms, thus minimizing the number of terms. It should 
   get each truncation the closest possible to the real value. Final 
   consecutive terms can stay only if previous term has the same sign. 
   O(log n) because recursion is not all of the times, 
   so it will stop recurring in a length proportional to log n, as much.
-}
carry :: [Term] -> [Term]
carry (a : b : xs)
   -- | a > succ b      = a : carry (b : xs)
   | negA == b       = carry xs
   | a == b          = sucA : carry xs
   | negA == sucB    = carry (negB : xs)
   | a == sucB       = carry $ sucA : carry (negB : xs)
   | a < b           = carry $ b : a : xs    -- sort
   where
   negA = tNeg a
   negB = tNeg b
   sucA = succ a
   sucB = succ b
carry xs = xs

{- | carryAll is the recursive application of carry. Not used -}
-- carryAll :: [Term] -> [Term]
-- carryAll (x : xs) = carry $ x : carryAll xs
-- carryAll _        = []


-----------------------------------------------------------------------
-- * Base operations: bit-shift, duplicate, halve, 
---------------------------------------------------------------------------

-- | Multiplication by power of 2, MULtiple DUPlication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
-- names: pw2, mulDup, manyDup, DupN, timesDup, dupDup, mul2pow, dupRep/repDup, shift
pw2 :: Int -> [Term] -> [Term]
pw2 n  = fmap (pw2term n)
   where
   pw2term n (T s v)
      | vn < 0 = error "Third of non-multiple of 3, in Ternary.pw3. "
      | True   = T s $ fromIntegral vn -- conversion to natural
      where vn = fromIntegral v + n    -- conversion from natural

-- | quarter: like dividing by 4; 
quarter :: [Term] -> [Term]
quarter x      = pw2 (-2) x
-- | Duplicate: multiply by 2, faster than add xs xs; 
-- half: like dividing by 2; 
-- they are a particular case of product by power of 2: pw2
npDup, npHlf, npSqr :: Binary -> Binary
npDup (NB x)   = NB $ pw2 1 x  -- takes more comparisons of conditions
npHlf (NB x)   = NB $ pw2 (-1) x


-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

{- | Square, O(n^2), faster than @mul xs xs@ -}
-- sqr :: Binary -> Binary
sqr :: [Term] -> [Term]
-- sqr (T s x : xs) = carry
   -- . (T True (dup x) :)
   -- . mul xs 
   -- $ (T s (x + 1) : xs) -- (2*x + xs)*xs
sqr (T s x : xs) = carry
   . (T True (dup x) :)
   . add (mul [T s (x + 1)] xs)  -- multiplication by one term, more efficient
   $ sqr xs -- (2*x + xs)*xs
sqr _ = []

npSqr (NB x) = NB (sqr x)
-- npSqr (NB [T _ x]) = oneTerm (dup x) -- single term square

-- mul, mulF  :: Binary -> Binary -> Binary
mul, mulE, mulF  :: [Term] -> [Term] -> [Term]

-- product by a one term element, equivalent to pw2
-- mul [T sa aa] [T sb ab] = [T (sa == sb) (aa + ab)]
-- mul [T sx ax] (T s v : ys) = T (sx == s) (ax + v) : mul [T sx ax] ys
mul [T True ax] ys = pw2 (fromIntegral ax) ys
mul [T _    ax] ys = pw2 (fromIntegral ax) $ neg ys
mul xs [y] = mul [y] xs
-- general product, several methods can be chosen
mul x y = mulE x y
-- mul x y = mulS x y
-- mul x y = mulF x y

{- | Multiplication
   Egyptian method, O(n^2) -}
mulE xs (T sb ab : ys) = add (mul xs [T sb ab]) (mulE xs ys)
mulE _ _  = []

{- | Straightforward multiplication, O(n^2) -}
mulS (T sa aa : xs) (T sb ab : ys) = carry
   . (T (sa == sb) (aa + ab) :)
   . add (mul xs [T sb ab])
   . add (mul [T sa aa] ys)
   $ mulS xs ys
mulS _ _ = []

{- | We define clMul to be x*y = 1/4(a^2 - b^2),
   where a = x + y; b = x - y ('F' for 'Fast' or 'Fermat').
   Complexity is mainly on sqr, as the rest of the algorithm is <~ O(size). -}
mulF xs ys = quarter $ sub sqSub sqSum   -- 1/4(A^2 - B^2)
   where sqSub = sqr $ sub xs ys   -- squared difference: (ys - xs)^2
         sqSum = sqr $ add xs ys   -- squared sum:        (ys + xs)^2


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------


divStep :: Binary -> Binary  -> (Term, Binary)
{- | Single division step. (Not used yet)
   Returns  SINGLE TERM  quotient which minimizes absolute rest, 
   and the new rest; keeps 'r + q*d' = constant, where: 
   d = divisor; q = quotient; r = rest or module. -}
divStep _ 0 = error "Divided by 0 in Binary.divStep. "
divStep 0 _ = (T True 0, 0)
divStep (NB r) (NB d) = minimumBy comp candidates
   where
   -- positive rNew, reverse for optimization of steps 
   candidates  = reverse $ (T True 0, NB r) : fmap qrPair digits
      where
      T sr ar  = head r
      T sd ad  = head d
      digits   = [0 .. 1 + ar - ad]
      
      qrPair q = (qNew, NB rNew)
         where 
         qNew  = T (sr == sd) q
         rDif  = mul [qNew] d
         rNew  = sub rDif r
   
   comp (_, ra) (_, rb)
      | res == EQ && sgn rb   = GT  -- in order to prioritize positive rest
      | True                  = res
      where 
      res = compare (S.abs ra) (S.abs rb)
      


---------------------------------------------------------------------------
   
