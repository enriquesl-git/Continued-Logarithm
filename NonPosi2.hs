-----------------------------------------------------------------------------
-- |
-- Module      :  NonPosi2
-- Copyright   :  (c) Enrique Santos, February 2019
-- License     :  see LICENSE
-- 
-- Maintainer  :  Enrique Santos
-- Stability   :  internal
-- Portability :  Portable
-- 
-- Nonpositional binary (NonPosi2) data type and its operations. 
-- 
-- It is first defined a type Term such that negative terms are 
-- represented with the '-' sign following the value (exponent of 2). 
--  
-- Then, the type NonPosi2 is defined as a list of Term:
-- An example is: 377 = NP [9, 7-, 3-, 0]
 
-- It should be an instance of 'Integer' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module NonPosi2 (
   Term(..), NonPosi2(..), NP, PairNP, -- data types
   half, dup, sgn, -- from imported modules
   i2terms, terms2i, pair2i, oneTerm, -- conversion
   tNeg, neg, carry, add, sub, -- arithmetic & comparation
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
   pred (T _ 0) = error "Predecessor of +-1, in NonPosi2.Term"
   pred (T s v) = T s (v - 1)
   succ (T s v) = T s (v + 1)



--------------------------------------------------------------
-- 
-- | Continued Logarithm (CL) == Differential Non-positional Binary
-- Accumulated C. Logarithm == Non-positional Binary (NB) 
-- 
---------------------------------------------------------------------------

{- | NonPosi2 will be an instance of: 
   (Ord, Num, Integral, Signed, Num, Integral, Real, Foldable?) -}
newtype NonPosi2 = NP [Term] deriving (Eq, Show, Read)
type NP = NonPosi2   -- just for brevity
type PairNP = (NonPosi2, NonPosi2)


---------------------------------------------------------------------------
{- Instances of NonPosi2 -}
---------------------------------------------------------------------------

instance Ord NonPosi2 where

   (<=) (NP (T sx ax : xs)) (NP (T sy ay : ys))
      | sx /= sy  = sy
      | ax /= ay  = sy && ax < ay
      | True      = NP xs <= NP ys
   (<=) (NP (T sx _ : _)) _  = not sx
   (<=) _ y  = sgn y


instance Enum NonPosi2 where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Signed NonPosi2 where

   (+.) a s          = a + NP [T s 0] -- a +- 1
   sgn (NP (x : _))  = tSgn x
   sgn _             = True


instance Num NonPosi2 where

   fromInteger       = NP . i2terms
   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NP x)     = NP (neg x)
   (+) (NP x) (NP y) = NP (add x y)
   (*) (NP x) (NP y) = NP (mul x y)


instance Integral NonPosi2 where

   toInteger (NP t) = terms2i t

   {- Euclidean recursive division, minimum absolute rest -}
   divMod _ 0  = error "Division by 0, in NonPosi2.divMod. " 
   divMod rs ds
      | end    = (0, rNext) 
      | True   = (qNew, rNew) 
      where
      end   = qNext == 0   -- rs < ds  ==>  qNew == q
      qNew  = qNext + q    -- accumulates q, without passing an accumulator
      (q, rNew)      = divMod rNext ds
      (qNext, rNext) = divStep rs ds

   {- | Returns quotient with positive rest always -}
   quotRem n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = divMod n d


instance Real NonPosi2 where

   toRational = toRational . toInteger


-- instance Foldable NonPosi2 where

   -- foldMap f (NP x) = NP (foldMap f x)

   -- foldr f z (NP x) = NP (foldr f z x)


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
   transform (T s a) x 
      = x + 2 ^ a *. s
      -- | s      = x + 2 ^ a
      -- | True   = x - 2 ^ a  -- for negative terms

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

pair2i :: (NonPosi2, NonPosi2) -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)

oneTerm :: Int -> NP
oneTerm x 
   | sgn x  = NP [T True x]
   | True   = error "Negative argument in NonPosi2.oneTerm. "

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
neg (a : xs) = tNeg a : neg xs
neg _ = []

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
pw2 n (T s v : xs) 
   | n < -v = error "Half of odd number in CLog.clPw2. "
   | True   = T s (v + n) : pw2 n xs
pw2 _ _     = []

-- | quarter: like dividing by 4; 
quarter :: [Term] -> [Term]
quarter x      = pw2 (-2) x
-- | Duplicate: multiply by 2, faster than add xs xs; 
-- half: like dividing by 2; 
-- they are a particular case of product by power of 2: pw2
npDup, npHlf, npSqr :: NonPosi2 -> NonPosi2
npDup (NP x)   = NP $ pw2 1 x  -- takes more comparisons of conditions
npHlf (NP x)   = NP $ pw2 (-1) x


-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

{- | Square, O(n^2), faster than @mul xs xs@ -}
-- sqr :: NonPosi2 -> NonPosi2
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

npSqr (NP x) = NP (sqr x)
-- npSqr (NP [T _ x]) = oneTerm (dup x) -- single term square

-- mul, mulF  :: NonPosi2 -> NonPosi2 -> NonPosi2
mul, mulE, mulF  :: [Term] -> [Term] -> [Term]

-- product by a one term element, equivalent to pw2
-- mul [T sa aa] [T sb ab] = [T (sa == sb) (aa + ab)]
-- mul [T sx ax] (T s v : ys) = T (sx == s) (ax + v) : mul [T sx ax] ys
mul [T True ax] ys = pw2 ax ys
mul [T _    ax] ys = pw2 ax $ neg ys
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


divStep :: NonPosi2 -> NonPosi2  -> PairNP
{- | Single division step. 
   Returns  SINGLE TERM  quotient which minimizes absolute rest, 
   and the new rest; keeps 'r + q*d' = constant, where: 
   d = divisor; q = quotient; r = rest or module. -}
divStep _ 0 = error "Divided by 0 in NonPosi2.divStep. "
divStep 0 _ = (0, 0)
divStep (NP r) (NP d) = minimumBy comp candidates
   where
   
   comp (_, ra) (_, rb)
      | res == EQ && sgn rb   = GT  -- in order to prioritize positive rest
      | True                  = res
      where 
      res = compare (S.abs ra) (S.abs rb)
      
   -- positive rNew, reverse for optimization of steps 
   candidates  = reverse $ (0, NP r) : fmap qrPair digits 
      where
      T sr ar  = head r
      T sd ad  = head d
      digits   = [0 .. 1 + ar - ad]
      qrPair q = (qNew, rNew)
         where 
         qNew  = NP [T (sr == sd) q]
         rNew  = NP r - qNew * NP d

---------------------------------------------------------------------------
   