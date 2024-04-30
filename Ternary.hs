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
-- Nonpositional Ternary ('Ternary', NT) data type and its operations.
-- 
-- It is first defined a type Term, and then, an ordered list of them 
-- starting by the greatest one, 'Ternary = NT [Term]', such that negative 
-- terms are represented with the '-' sign behind the value (exponent of 3). 
-- An example is: 
--   377 = NT [6, 5-, 4-, 3-, 0-] 
--       = - 3^0 - 3^3 - 3^4 - 3^5 + 3^6
--       = 3^6 - 3^5 - 3^4 - 3^3 - 3^0
--  
-- Then, the type Continued Ternary (CT) is defined as a reversed list of the 
-- Nonpositional Ternary (cummulative Terms), such that an element value is 
-- the sum of them up to it:
--   377 = NT [0-,3-,1- 1- 1] 
--       = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(1 + 3^1))))

-- They both should be an instance of 'Integral' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module Ternary (
   Term(..), Ternary(..), PairNT, -- data types
   half, dup, sgn,   -- from imported modules
   oneTerm, pair2i,  -- conversion
   neg, add, sub,    -- arithmetic 
   cmp, sgnTerms, absTerms, compareAbs, minAbs, -- comparation
   pw3, ntTripl, ntThird, mul, sqr, ntSqr,      -- quarter, geometric
   divStep, rootStep, root, rootRem, log3       -- inverse
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
--   Accumulated C. Logarithm == Non-positional Ternary (NT) 
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
   compare (NT x) (NT y) = cmp x y


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
   -- sgn (NT x)        = sSgn $ last x
   sgn (NT x)        = sSgn $ head x

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
      
   fromInteger = NT . reverse . fromI 0  -- reverse $
      where
      fromI _ 0 = []
      fromI a n
         | r == 0    =               fromI (1 + a) q
         | True      = S (sgn r) a : fromI (1 + a) q  -- Cummulative
         -- | True      = S (sgn r) a : fromI (1 ) q -- Differential
         where (q, r) = quotMod 3 n -- minimizes absolute rest

   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NT x)     = NT (neg x)
   -- (+) x y = fromTerms $ add (toTerms x) (toTerms y)
   -- (*) x y = fromTerms $ mul (toTerms x) (toTerms y)
   (+) (NT x) (NT y) = NT $ add x y
   (*) (NT x) (NT y) = NT $ mul x y


instance Integral Ternary where

   
   {- | From non-positional ternary to Integer, admits negative terms,
   it also defines the recursive homographic transformation. 
   Used in Integral instance. -}
   -- toInteger (NT t) = foldl transform 0 t where 
   toInteger (NT t) = foldr transform 0 t where 
      transform (S s a) 
         -- = (+) $ (*. s) . (^ a) $ 3
         -- = (+) $ (*. s) . (3 ^) $ a
         = (+) $ (*. s) $ 3 ^ a  -- Cummulative
         -- = (* 3 ^ a) . (+. s)    -- Differential


   {- | Returns quotient with positive rest always -}
   divMod n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = quotRem n d


   {- Euclidean recursive division, minimum absolute rest.
      Division is a recursive subtraction.  -}
   quotRem _ 0 = error "Division by 0, in Ternary.quotRem. "
   quotRem 0 _ = (0, 0)
   quotRem (NT r) (NT d) = divStep (NT r) (0, NT d) aq
      where
      ad = sAbs $ head d   -- divisor greatest term
      ar = sAbs $ head r   -- rest greatest term
      aq = 1 + ar - ad     -- initial q to try


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------

{- Recursive square root, minimum absolute rest.
 -}
rootRem :: Ternary -> (Ternary, Ternary)
rootRem (NT (S False _ : _))  = error "Square root of negative number, in Ternary.root"
rootRem 0 = (0, 0)
rootRem r0  | r0 > q*(1 + q)  = (1 + q, r - (1 + q + q))  -- for min absolute
            | True            = (    q, r)
   where
   (q, r) = rootStep (qNew, rNew) aq

   -- Initial values when odd 'ar': rNew = r0 - qNew*qNew; 2*aq = ar - 1   
   (qNew, rNew) 
      | odd ar && r0 > level  = (NT [S sr aq], r0 - NT [S sr (ar - 1)]) 
      | True                  = (0, r0)     

   -- 'level' is the semisum of consecutive squares
   level  = NT . fmap (toTerm) . reverse $ [1 .. ar - 1]
   toTerm x = S (odd x) x

   NT (S sr ar : _) = r0   -- rest greatest term and sign
   aq = half ar            -- initial q to try, rounding down when 'ar' odd


root :: Ternary -> Ternary
root = fst . rootRem


-- recursively tries q values, from aq down to 0 
divStep :: Ternary -> (Ternary, Ternary) -> Int -> (Ternary, Ternary)
divStep d (q, r) aq
   | aq < 0    = (q,  r)  -- RESULT
   | acceptQ   = divStep d (qNew, rNew) (aq - 1)
   | True      = divStep d (q,    r   ) (aq - 1)
   where
   sd = sgn d
   sr = sgn r
   acceptQ  | sr    = rNew >  -r   -- rDif < 2*r
            | True  = rNew <= -r   -- '=' to promote positive rest
   
   (qDif, rDif) = (NT [S (sr == sd) aq], qDif * d)   
   (qNew, rNew) = (q + qDif, r - rDif)


-- recursively tries q values, from aq down to 0
rootStep :: (Ternary, Ternary) -> Int -> (Ternary, Ternary)
rootStep (q, r) aq
   | aq < 0    = (q,  r)  -- RESULT
   | acceptQ   = rootStep (qNew, rNew) (aq - 1)
   | True      = rootStep (q,    r   ) (aq - 1)
   where
   
   sr = sgn r
   acceptQ  | sr    = rNew >  -r    -- rDif < 2*r
            | True  = rNew <= -r    -- '=' to promote positive rest
   
   (qDif, rDif) = (NT [S sr aq], qDif * (q + qNew) )   
   (qNew, rNew) = (q + qDif, r - rDif)


{- | Closest power of 3 -}
log3 :: [Term] -> Int
log3 (x0 : x1 : xs)
   |  sNeg x0 == succ x1
   && (sgn (NT xs) == sSgn x1
   || xs == [])
                     = sAbs x1
log3 (x0 : _)        = sAbs x0
log3 _               = error "Logarithm of 0 in Ternary.log3"



---------------------------------------------------------------------------
{- = 
   Conversion from and to 'Terms' (list of 'Term': [Term]).
   'Terms' are greatest first, and accumulated, 
      21 = [ 3, 2-, 1] = 3^3 - 3^2 + 3
   'Ternary' is smallest first, and differential.
      21 = NT [ 1, 1-, 1] = ((( 1 )*3 - 1 )*3 + 1 )*3
-}
---------------------------------------------------------------------------

{-
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
-}

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

{- | Minimum when comparing absolute values, 
   should be used for every rest. -}
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
-- add xs [] = xs
-- add [] xs = xs
-- add [y] (x : xs)
   -- |      x ==      y   = sNeg y : add [succ y] xs -- carry
   -- |      x == sNeg y   =                       xs
   -- | sAbs x >  sAbs y   =      y :          x : xs
   -- | True               =      x :      add [y] xs -- carry
-- add (y : ys) xs         = add [y] $     add ys  xs -- recursion

-- Addition when notation is greatest term first
add xs [] = xs
add [] xs = xs
add [y] (x : xs)
   |      x ==     y    = succ y : sNeg x : xs -- carry
   | sNeg x ==     y    =                   xs
   | sAbs x < sAbs y    =      y :      x : xs
   | True               = add [x ,      y]  xs -- carry
add (y : ys) xs         = add [y] $ add ys  xs -- recursion

sub = add . neg

neg :: [Term] -> [Term]
neg = fmap sNeg



-----------------------------------------------------------------------
-- * Base operations: bit-shift, triplicate, third,
---------------------------------------------------------------------------

-- | Multiplication by power of 3, MULtiple TRIPLication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
pw3 :: Int -> [Term] -> [Term]
pw3 = fmap . incr


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
-- mul x = mulE x
mul x = mulS x
-- mul x = mulF x

{- | Multiplication. Egyptian method, O(n^2). It is a 
   'concatMap', or 'foldMap', mapping 'mul' and folding 'add' 
-}
mulE xs (S sb ab : ys) 
   = add (mul [S sb ab] xs) 
   $ mulE ys xs   -- faster because of symetry
mulE _ _  = []

{- | Straightforward multiplication, O(n^2), -}
mulS (S sa aa : xs) (S sb ab : ys) 
   = add [S (sa == sb) (aa + ab)] 
   . add (mul [S sa aa] ys)
   . add (mul [S sb ab] xs)
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
-- sqr (S s x : xs)
   -- = add (S True (x + x) : sqr xs)     -- x^2 + xs^2 + 
   -- $ mul [S s (x + 1), S (not s) x] xs -- xs * (x + x), in ternary

-- | non-recursive version:  x^2 + (2*x + xs)*xs
sqr (S s x : xs) =
   add [S True (x + x)] 
   . mul xs $ S s (x + 1) : S (not s) x : xs

sqr _ = []

-- ntSqr :: Ternary -> Ternary
ntSqr (NT x) = NT (sqr x)
-- ntSqr (NT [S _ x]) = oneSInt (dup x) -- single term square


