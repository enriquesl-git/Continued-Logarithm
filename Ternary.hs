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
-- It is first defined a type Term, and then an ordered list of them 
-- starting by the greatest one, Terms = [Term], such that negative terms 
-- are represented with the '-' sign behind the value (exponent of 3). 
-- An example is: 
--   377 = [6, 5-, 4-, 3-, 0-] 
--       = - 3^0 - 3^3 - 3^4 - 3^5 + 3^6
--       = 3^6 - 3^5 - 3^4 - 3^3 - 3^0
--  
-- Then, the type Ternary is defined as a reversed list of the cummulative 
-- Terms, such that an element value is the sum of them up to it:
--   377 = NT [0-,3-,1- 1- 1] 
--       = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(1 + 3^1))))

-- It should be an instance of 'Integral' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module Ternary (
   Term(..), Ternary(..), PairNT, -- data types
   half, dup, sgn, -- from imported modules
   toTerms, fromTerms, oneTerm, pair2i, -- conversion
   cmp, sgnTerms, absTerms, compareAbs, minAbs,  -- comparation
   neg, add, sub, -- arithmetic 
   pw3, ntTripl, ntThird, mul, sqr, npSqr,   -- quarter, geometric
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

   -- Needed to be processed from greater term down to the smaller one
   -- (<=) (NT []) ys  = sgn ys
   -- (<=) (NT xs) (NT ys)
      -- | sx /= sy  = sy
      -- | ax /= ay  = sy && ax < ay
      -- | True      = NT (init xs) <= NT (init ys)
      -- where
      -- S sx ax = last xs
      -- S sy ay = last ys
      
   compare x y = cmp (toTerms x) (toTerms y)


-- instance Foldable Ternary where

   -- foldMap f (NT x) = NT (foldMap f x)

   -- foldr f z (NT x) = NT (foldr f z x)


instance Enum Ternary where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Real Ternary where

   toRational = toRational . toInteger


instance Signed Ternary where

   (+.) a s          = a + NT [S s 0] -- a +- 1
   sgn (NT [])       = True
   sgn (NT x)        = sSgn $ last x

{- = 
   Conversion from and to Decimal base notation: 
      fromInteger, toInteger
   Continued Logarithm == continued product, by Euler transform
   Here it is really cLog(x + 1) 
-}

instance Num Ternary where

   {- | Gives in each step the closest power of 3. 
      That way, truncation will give the best approximation. 
      
      If odd subtract 1 and write 0, if even divide by 3 and count. 
      Write times counted. 
        x0 = 3^a0*sgn + x1; [a0,a1,...,an] == 3^a0 + 3^a1 + ... + 3^an
      Should be an 'unfold' or equivalent to it.
      Used in Num instance. -}
      
   -- fromInteger       = fromTerms . i2terms
   -- i2terms :: Integer -> [Term]  -- is: Integer Signed
   -- i2terms = fromI 0 where -- reverse $
   fromInteger = NT . fromI 0  -- reverse $
      where
      fromI _ 0 = []
      fromI a n
         | r == 0    =               fromI (1 + a) q
         -- | True      = S (sgn r) a : fromI (1 + a) q
         | True      = S (sgn r) a : fromI (1 ) q -- Differential
         where (q, r) = quotMod 3 n -- minimizes absolute rest

   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NT x)     = NT (neg x)
   (+) x y = fromTerms $ add (toTerms x) (toTerms y)
   -- (*) x y = fromTerms $ mul (toTerms x) (toTerms y)
   (*) (NT x) (NT y) = fromTerms $ mul x y



instance Integral Ternary where

   
   {- | From non-positional ternary to Integer, admits negative terms,
   it also defines the recursive homographic transformation. 
   Used in Integral instance. -}
   -- toInteger = terms2i . toTerms
   -- terms2i :: [Term] -> Integer
   -- terms2i = foldr transform 0 where 
   toInteger (NT t) = foldr transform 0 t where 
      transform (S s a) 
         -- = (+) $ (*. s) . (^ a) $ 3
         -- = (+) $ (*. s) . (3 ^) $ a
         -- = (+) $ (*. s) $ 3 ^ a
         = (* 3 ^ a) . (+. s)    -- Differential
   -- terms2i (S s a : xs) = ((terms2i xs +. s) * 3 ^ a)
   -- terms2i _      = 0


   {- | Returns quotient with positive rest always -}
   -- divMod = quotRem
   -- quotRem n d
      -- | sgn r  = (q    , r)
      -- | sgn d  = (q - 1, r + d)
      -- | True   = (q + 1, r - d)
      -- where
      -- (q, r) = divMod n d


   {- Euclidean recursive division, minimum absolute rest.
      Division is a recursive subtraction.  -}
   quotRem _ 0  = error "Division by 0, in Ternary.quotRem. "
   quotRem rs ds 
      | dupR >  d    = (qNew + 1, rNew - d)
      | dupR <= - d  = (qNew - 1, rNew + d)
      | dupR <= d    = (qNew, rNew)
      
      where
      -- use the divisor always positive
      (d, r)   | S.sgn ds  = (ds, rs)
               | True      = (- ds, - rs)
      (qTerm, rTerm) = divStep (toTerms d) [] (toTerms r)
      (qNew, rNew) = (fromTerms qTerm, fromTerms rTerm)
      dupR = rNew + rNew



-- divStep :: Ternary -> Ternary -> (Ternary, Ternary)
divStep :: [Term] -> [Term] -> [Term] -> ([Term], [Term])
{- | Returns quotient which gives rest less than divisor. 
   On each single division step calculates SINGLE TERM 
   quotient which minimizes absolute rest, and the new rest; 
   
   keeps '|d*q| + r' = constant, where: 
   d = divisor; q = quotient; r = rest or module. -}
divStep _ q [] = (q, [])
divStep d q r
   | ar < ad   = (q, r)
   -- | dupr >      S.abs d   = (sub rNew d, [succ qDif])
   -- | dupr < neg (S.abs d)  = (add rNew d, [pred qDif])
   | True      = divStep d qNew rNew   
   -- | True      =  (qNew, rNew)   
   where
   -- dupr = add rNew rNew
   S sd ad = head d   -- divisor greatest term
   S sr ar = head r   -- rest greatest term
   
   qDif = [S (sr == sd) (ar - ad)] -- oneTerm (ar - ad)
   rDif = mul d qDif
   qNew = add qDif q
   rNew = sub rDif r    -- rNew = r - d * q
   


---------------------------------------------------------------------------
{- = 
   Conversion from and to 'Terms' (list of 'Term': [Term]).
   'Terms' are greatest first, and accumulated, 
      21 = [ 3, 2-, 1] = 3^3 - 3^2 + 3
   'Ternary' is smallest first, and differential.
      21 = NT [ 1, 1-, 1] = ((( 1 )*3 - 1 )*3 + 1 )*3
-}
---------------------------------------------------------------------------

-- to Terms from Ternary
-- cumulate, greatest first
toTerms :: Ternary -> [Term]
toTerms (NT t) = reverse $ zipWith (S) s a where
   s = fmap sSgn t
   a = scanl1 (+) . fmap sAbs $ t

-- from Terms to Ternary
-- uncumulate, differentiate, smallest first
fromTerms :: [Term] -> Ternary
fromTerms t = NT $ zipWith S s d where
   rt = reverse t
   s = fmap sSgn rt
   a = fmap sAbs rt
   d = zipWith (-) a (0 : a)

oneTerm :: Int -> Ternary
oneTerm x
   | sgn x  = NT [S True x]
   | True   = error "Negative argument in Ternary.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   compare, addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

cmp :: [Term] -> [Term] -> Ordering 
cmp (x : xs) (y : ys)
      | x < y  = LT 
      | x > y  = GT
      | True   = cmp xs ys      
cmp (x : _) [] = if sSgn x then GT else LT
cmp [] (y : _) = if sSgn y then LT else GT
cmp [] []    = EQ

sgnTerms :: [Term] -> Bool
sgnTerms [] = True
sgnTerms x = sSgn $ head x

absTerms :: [Term] -> [Term]
absTerms [] = []
absTerms x = if sgnTerms x then x else neg x 

compareAbs :: [Term] -> [Term] -> Ordering 
compareAbs x y = cmp (absTerms x) (absTerms y)

{- | Minimum of the absolute values, 
   must be used for every rest. -}
minAbs :: [Term] -> [Term] -> [Term] 
minAbs x y
   = case compareAbs x y of
   LT -> x
   GT -> y
   EQ -> x  -- being equal abs, the first is chosen 


{- | Addition, for both, positive and negative terms 
-}
add, sub :: [Term] -> [Term] -> [Term]
-- Addition when notation is smallest term first
-- add [y] (x:xs)
   -- | x == sNeg y     =                       xs
   -- | x == y          = sNeg y : add [succ y] xs -- carry & recursion
   -- | sAbs x > sAbs y =      y : x :          xs
   -- | True            =      x : add [y]      xs -- recursion
-- add xs [] = xs
-- add [] xs = xs
-- add (y:ys) xs        = add [y] $ add ys xs

-- Addition when notation is greatest term first
add [y] (x:xs)
   | x == sNeg y     =                       xs
   | x == y          = succ y : sNeg x :     xs -- carry
   | sAbs x < sAbs y =      y : x :          xs
   | True            = add [x] $ add [y]     xs -- recursion
add xs [] = xs
add [] xs = xs

add (y:ys) xs        = add [y] $ add xs ys


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
   -- where
   -- h [a, b]
      -- | a == 0    = [] -- a : b : xs
      -- | sgn a /= sgn b    = reverse [b .. pred a] 
      -- | True = a : fmap negate $ reverse [b .. pred a] 

      -- -- | True      = add [a1] $ h (a1 : b : xs)
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

-- | Product by a one term element, equivalent to pw3
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
mulE xs (S sb ab : ys) = add (mul [S sb ab] xs) (mulE xs ys)
mulE _ _  = []

{- | Straightforward multiplication, O(n^2) -}
mulS (S sa aa : xs) (S sb ab : ys) = 
   (S (sa == sb) (aa + ab) :) 
   . add (mul [S sb ab] xs)
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
   = (S True (x + x) :) -- x^2 +
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


-- divStep (NT r) (NT d) =
--    where
--    (r, d) = ()
--

