-----------------------------------------------------------------------------
-- |
-- Module      :  NonPosi3
-- Copyright   :  (c) Enrique Santos, June 2022
-- License     :  see LICENSE
-- 
-- Maintainer  :  Enrique Santos
-- Stability   :  internal
-- Portability :  Portable
-- 
-- Nonpositional ternary ('NonPosi3') data type and its operations. 
-- 
-- It is first defined a type Term such that negative terms are 
-- represented with the '-' sign following the value (exponent of 3). 
--  
-- Then, the type NonPosi3 is defined as a list of Term:
-- An example is: 377 = NP [6,5-,4-,3-,0-]
 
-- It should be an instance of 'Integral' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module NonPosi3 (
   Term(..), NonPosi3(..), NP, PairNP, -- data types
   half, dup, sgn, -- from imported modules
   i2terms, terms2i, oneTerm, pair2i, -- conversion
   tNeg, neg, add, sub, -- arithmetic & comparation
   pw3, npTripl, npThird, mul, divStep, sqr, npSqr,   -- geometric
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


---------------------------------------------------------------------------
-- | Data type of the non-positional digits
---------------------------------------------------------------------------

-- Term of Continued Logarithm

-- type Termi = (Bool, Int)

-- tAbs :: Int -> Termi
-- tAbs (_, v) = v

-- tSgn :: Termi -> Bool
-- tSgn (s, _) = s

data Term = T { 
   tSgn :: Bool,  -- Signum of term
   tAbs :: Int    -- Value of term
} deriving (Eq)

-- Opposite of term
tNeg :: Term -> Term
tNeg (T s v) = T (not s) v


---------------------------------------------------------------------------
{- Instances of Term -}
---------------------------------------------------------------------------

instance Show Term where

   show (T True v) = ' ' : show v
   show (T _    v) = ' ' : show v ++ "-"


instance Read Term where

   readsPrec _ s = 
      let rd = reads s in
      [(T False v, t) | (v, r) <- rd, ("-", t) <- lex r] ++ 
      [(T True  v, t) | (v, r) <- rd, ("+", t) <- lex r] ++ 
      [(T True  v, t) | (v, t) <- rd]
      
   -- readsPrec _ s = 
      -- [(T False v, r) | (v, '-' : r) <- reads r] ++
      -- [(T True v,  r) | (v, '+' : r) <- reads r] ++
      -- [(T True v,  r) | (v,       r) <- reads r]

   -- readsPrec _ s = 
      -- do
      -- (v, '-' : r)   <- reads s
      -- return (T False v, r)

      -- readsPrec _ s = do
      -- (v, rest)   <- reads s
      -- (sg, r)     <- lex rest
      -- pure $ case sg of 
         -- "-"   -> (T False v, r)
         -- "+"   -> (T True v , r)
         -- _     -> (T True v , rest)


{- Term as an instance of (Num, Ord, Signed)
-}

instance Ord Term where
   -- compare absolutes!
   compare (T _ x1) (T _ x2) = compare x1 x2


instance Enum Term where

   fromEnum (T s v)  = v -- *. s
   toEnum x          = T True ax -- sx ax 
      where (sx, ax) = sgnAbs x
   pred (T _ 0) = error "Predecessor of +-1, in NonPosi3.Term"
   pred (T s v) = T s (v - 1)
   succ (T s v) = T s (v + 1)



--------------------------------------------------------------
-- 
-- | Continued Logarithm (CL) == Differential Non-positional Ternary
--   Accumulated C. Logarithm == Non-positional Ternary (NB) 
-- 
---------------------------------------------------------------------------

{- | NonPosi3 will be an instance of: 
   (Ord, Num, Integral, Signed, Num, Integral, Real, Foldable?) -}
newtype NonPosi3 = NP [SInt] deriving (Eq, Show, Read)
type NP = NonPosi3   -- just for brevity
type PairNP = (NonPosi3, NonPosi3)

pair2i :: PairNP -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)


---------------------------------------------------------------------------
{- Instances of NonPosi3 -}
---------------------------------------------------------------------------

instance Ord NonPosi3 where

   (<=) (NP (S sx ax : xs)) (NP (S sy ay : ys))
      | sx /= sy  = sy
      | ax /= ay  = sy && ax < ay
      | True      = NP xs <= NP ys
   (<=) (NP (S sx _ : _)) _  = not sx
   (<=) _ y  = sgn y


instance Enum NonPosi3 where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Signed NonPosi3 where

   (+.) s a          = a + NP [S s 0] -- a +- 1
   sgn (NP (x : _))  = sSgn x
   sgn _             = True


instance Num NonPosi3 where

   fromInteger       = NP . i2terms
   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NP x)        = NP (neg x)
   (+) (NP x) (NP y) = NP (add x y)
   (*) (NP x) (NP y) = NP (mul x y)


instance Integral NonPosi3 where

   toInteger (NP t) = terms2i t

   {- Euclidean recursive division, minimum absolute rest -}
   divMod _ 0  = error "Division by 0, in NonPosi3.divMod. " 
   divMod rs ds
      | end    = (0, rNext) 
      | True   = (qNew, rNew) 
      where
      end   = sAbs qNext == 0   -- rs < ds  ==>  qNew == q
      qNew  = NP [qNext] + q    -- accumulates q, without passing an accumulator
      (q, rNew)      = divMod rNext ds
      (qNext, rNext) = divStep rs ds

   {- | Returns quotient with positive rest always -}
   quotRem n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = divMod n d


instance Real NonPosi3 where

   toRational = toRational . toInteger


-- instance Foldable NonPosi3 where

   -- foldMap f (NP x) = NP (foldMap f x)

   -- foldr f z (NP x) = NP (foldr f z x)


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
terms2i :: [SInt] -> Integer
terms2i = foldr transform 0 where
   transform (S s a) x 
      = x + s *. 3 ^ a
      -- | s      = x + 3 ^ a
      -- | True   = x - 3 ^ a  -- for negative terms

{- | Gives in each step the closest power of 3. 
   That way, truncation will give the best approximation. 
   
   If odd subtract 1 and write 0, if even divide by 3 and count, 
   write times counted. 
   x0 = 3^a0*sgn + x1; [a0,a1,...,an] == 3^a0 + 3^a1 + ... + 3^an
   Should be an 'unfold' or equivalent to it.
   Used in Num instance. -}
i2terms :: Integer -> [SInt]  -- is: Integer Signed
i2terms = i2terms' 0 where -- reverse $
   i2terms' _ 0 = []
   i2terms' a n
      | r ==  1   = S True  a : i2terms' (1 + a) q 
      | r == -1   = S False a : i2terms' (1 + a) q 
      | True      = i2terms' (1 + a) q 
      where (q, r) = quotMod 3 n

oneTerm :: Word -> NP
oneTerm x 
   | sgn x  = NP [S True x]
   | True   = error "Negative argument in NonPosi3.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

{- | Addition, for both, positive and negative terms 
-}
add, sub :: [SInt] -> [SInt] -> [SInt]
add xs [] = xs
add [] xs = xs
add (x:xs) [y] 
   | x == sNeg y  = xs
   | x == y       = sNeg x : add xs [succ x] -- carry
   | x < y        = x : add xs [y]
   | x > y        = y : x : xs
add xs (y:ys) = add (add xs [y]) ys


sub = add . neg

neg :: [SInt] -> [SInt]
neg (a : xs) = sNeg a : neg xs
neg _ = []

{- | Carry for arithmetic operations, 
   carry is to reorganize terms to avoid intermediate repeated or 
   consecutive terms, thus minimizing the number of terms. It should 
   get each truncation the closest possible to the real value. Final 
   consecutive terms can stay only if previous term has the same sign. 
   O(log n) because recursion is not all of the times, 
   so it will stop recurring in a length proportional to log n, as much.
-}
{-
carry :: [SInt] -> [SInt]
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
-- carryAll :: [SInt] -> [SInt]
-- carryAll (x : xs) = carry $ x : carryAll xs
-- carryAll _        = []


-----------------------------------------------------------------------
-- * Base operations: bit-shift, duplicate, halve, 
---------------------------------------------------------------------------

-- | Multiplication by power of 3, MULtiple TRIPLication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
pw3 :: Int -> [SInt] -> [SInt]
pw3 n  = fmap (pw3term n)
   where
   pw3term :: Int -> SInt -> SInt
   pw3term n (S s v)
      | vn < 0 = error "Third of non-multiple of 3, in NonPosi3.pw3. "
      | True   = S s $ fromIntegral vn    -- (v + n)
      where vn = fromIntegral v + n -- Int


-- dup
-- half
-- | quarter: like dividing by 9; 
-- quarter :: [SInt] -> [SInt]
-- quarter x      = pw3 (-2) x

-- | Triplicate: multiply by 3, faster than add; 
-- third: like dividing by 3; 
-- they are a particular case of product by power of 2: pw3
npTripl, npThird :: NonPosi3 -> NonPosi3
npTripl (NP x)   = NP $ pw3   1  x  -- takes more comparisons of conditions
npThird (NP x)   = NP $ pw3 (-1) x


-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

{- | Square, O(n^2), faster than @mul xs xs@ -}
sqr :: [SInt] -> [SInt]

-- x^2 + (2*x + xs)*xs
-- sqr (S s x : xs) = 
   -- (S True (dup x) :)
   -- . mul xs 
   -- $ S (not s) x: S s (x + 1) : xs 

-- recursive version   
-- x^2 + (2*x*xs + xs^2)
sqr (S s x : xs) 
   = (S True (dup x) :) -- x^2 +
   . add (sqr xs)       -- xs^2 +
   $ mul [S (not s) x, S s (x + 1)] xs  -- (2*x)*xs, in ternary 
   -- multiplication by two terms, more efficient 
   
-- recursive version   
-- (x^2 + 2*x*xs + xs^2)
-- sqr (S s x : xs) 
   -- = (S True (dup x) :)    -- x^2 + 
   -- . add (sqr xs)          -- xs^2 +
   -- $ mul xs [S (not s) x, S s (x + 1)]  -- xs*2x
   -- multiplication by two terms, more efficient 
   
sqr _ = []

-- npSqr :: NonPosi3 -> NonPosi3
npSqr (NP x) = NP (sqr x)
-- npSqr (NP [S _ x]) = oneTerm (dup x) -- single term square

-- mul, mulF  :: NonPosi3 -> NonPosi3 -> NonPosi3
mul, mulE  :: [SInt] -> [SInt] -> [SInt]

-- product by a one term element, equivalent to pw3
-- mul [S sa aa] [S sb ab] = [S (sa == sb) (aa + ab)]
-- mul [S sx ax] (S s v : ys) = S (sx == s) (ax + v) : mul [S sx ax] ys
mul [S True ax] ys = pw3 (fromIntegral ax) ys
mul [S _    ax] ys = pw3 (fromIntegral ax) $ neg ys
mul xs [y] = mul [y] xs

-- general product, several methods can be chosen
mul x y = mulE x y
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

{- | We define clMul to be x*y = 1/4(a^2 - b^2),
   where a = x + y; b = x - y ('F' for 'Fast' or 'Fermat').
   Complexity is mainly on sqr, as the rest of the algorithm is <~ O(size). -}
-- mulF xs ys = quarter $ sub sqSub sqSum   -- 1/4(A^2 - B^2)
   -- where sqSub = sqr $ sub xs ys   -- squared difference: (ys - xs)^2
         -- sqSum = sqr $ add xs ys   -- squared sum:        (ys + xs)^2


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------


divStep :: NonPosi3 -> NonPosi3  -> (SInt, NonPosi3)
{- | Single division step. (Not used yet)
   Returns  SINGLE TERM  quotient which minimizes absolute rest, 
   and the new rest; keeps 'r + q*d' = constant, where: 
   d = divisor; q = quotient; r = rest or module. -}
divStep _ 0 = error "Divided by 0 in NonPosi3.divStep. "
divStep 0 _ = (S True 0, 0)
divStep (NP r) (NP d) = minimumBy comp candidates
   where
   -- positive rNew, reverse for optimization of steps 
   candidates  = reverse $ (S True 0, NP r) : fmap qrPair digits 
      where
      S sr ar  = last r
      S sd ad  = last d
      digits   = [0 .. 1 + ar - ad]
      
      qrPair q = (qNew, NP rNew)
         where 
         qNew  = S (sr == sd) q
         rDif  = mul [qNew] d
         rNew  = sub rDif r
   
   comp (_, ra) (_, rb)
      | res == EQ && sgn rb   = GT  -- in order to prioritize positive rest
      | True                  = res
      where 
      res = compare (S.abs ra) (S.abs rb)
      


---------------------------------------------------------------------------
   
