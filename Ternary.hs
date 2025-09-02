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
-- starting by the greatest one, such that negative terms are represented 
-- with the '-' sign behind the value (exponent of 3). 

-- An example is: 
--   377 = NT [6, 5-, 4-, 3-, 0-] 
--       = - 3^0 - 3^3 - 3^4 - 3^5 + 3^6
--       = 3^6 - 3^5 - 3^4 - 3^3 - 3^0
--  
-- Then, the type 'Ternary = NT [Term]' is defined as a reversed list of the 
-- Nonpositional Ternary (differential Terms), such that an element value is 
-- the sum of them up to it:
--   377 = NT [0-,3-,1- 1- 1] 
--       = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(-1 + 3^1*(1 + 0)))))

-- This way has advantages: 
--    - multiplying/dividing by power of 3 (shift) is O(n) instead of O(1),
--      so that full multiplication is quite slower
--    - addition is also slower, because carry is backwards
-- Cons, advantages of accumulated:
--    - first term gives information of sign and "size";
--    - comparations are faster
--
-- They both should be an instance of 'Integral' and 'Signed', so it should 
-- implementmet methods for: Ord, Num, Signed, Integral 
-- 
-----------------------------------------------------------------------------

module Ternary (
   Term(..), Ternary(..), NT(..), PairNT,    -- data types
   oneTerm, pair2i, accumulate, decumulate,  -- conversion
   half, dup, sgn,   -- from imported modules
   neg, add, sub, mul, sqr, sigma,    -- terms arithmetic 
   cmp, sgnTerms, absTerms, compareAbs, minAbs, isEven, isOdd,    -- comparation
   pw3, ntSqr, ntTri, ntThird, ntDup, ntHalf, ntQuad, ntQuarter,  -- geometric
   log3, digitSquares, divStep, rootStep, rootRem, rootMod, root  -- inverse
) where

-- import Prelude hiding () -- (abs, signum)
-- import Prelude as P --  hiding (abs, signum, mod, div, ($), ($!), (.))
import Integers -- as I
import Signed -- as S
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
type Terms = [Term]  -- synonym, for type correctness 
type NT = Ternary    -- just for brevity
-- newtype Ternary = NT { toTerms :: Terms } deriving (Eq, Show, Read)
newtype Ternary = NT Terms deriving (Eq, Show, Read)
toTerms (NT t) = t

type PairNT = (Ternary, Ternary)
pair2i :: PairNT -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)

type PairTerms = (Terms, Terms)
pairt2i :: PairTerms -> (Integer, Integer)
pairt2i (x, y) = (toInteger $ NT x, toInteger $ NT y)


---------------------------------------------------------------------------
{- Instances of Ternary -}
---------------------------------------------------------------------------

instance Ord Ternary where

   -- Needed to be processed from greater term down to the smaller one
   compare (NT x) (NT y) = cmp x y  -- Cummulative
   -- compare (NT x) (NT y) = cmp accX accY  -- Differential
      -- where -- greatest term first
      -- accX = reverse $ accumulate x
      -- accY = reverse $ accumulate y

-- instance Foldable Ternary where

   -- foldMap f (NT x) = NT (foldMap f x)

   -- foldr f z (NT x) = NT (foldr f z x)


instance Enum Ternary where

   fromEnum          = fromEnum . toInteger
   toEnum            = fromInteger . toEnum -- not right


instance Real Ternary where

   toRational = toRational . toInteger


instance Signed Ternary where

   (+.) a s          = a + NT [T s 0] -- a +- 1
   sgn (NT [])       = True   -- positive means (>= 0)
   sgn (NT x)        = tSgn $ last x   -- differential, smaller first
   -- sgn (NT x)        = tSgn $ head x   -- accumulated, greatest first

{- = 
   Conversion from and to Decimal base notation: 
      fromInteger, toInteger
   Continued Logarithm == continued product, by Euler transform
   Here it is really cLog(x + 1) 
-}

instance Num Ternary where

   {- | Gives in each step the closest power of 3. 
      That way, truncation will give the best approximation. 
      
      If (n mod 3 == 0) divide by 3 and count, else subtract 1 and write 0
      Write times counted. 
        x0 = 3^a0*sgn + x1; [a0,a1,...,an] == 3^a0 + 3^a1 + ... + 3^an
      Should be an 'unfold' or equivalent to it.
      Used in Num instance. -}
      
   fromInteger = NT . fromI 0  -- reverse $
      where
      fromI _ 0 = []
      fromI a n
         | r == 0    =               fromI (1 + a) q
         -- | True      = T (sgn r) a : fromI (1 + a) q  -- Cummulative
         | True      = T (sgn r) a : fromI (1 ) q -- Differential
         where (q, r) = quotMod 3 n -- minimizes absolute rest

   abs               = snd . sgnAbs
   signum 0          =  0
   signum n | sgn n  =  1
   signum _          = -1
   negate (NT x)     = NT (neg x)
   -- (+) x y = fromTerms $ add (toTerms x) (toTerms y)
   -- (*) x y = fromTerms $ mul (toTerms x) (toTerms y)
   (+) (NT x) (NT y) = NT $ add y x
   (*) (NT x) (NT y) = NT $ mul y x


instance Integral Ternary where

   
   {- | From non-positional ternary to Integer, admits negative terms,
   it also defines the recursive homographic transformation. 
   Used in Integral instance. -}
   -- toInteger (NT t) = foldl transform 0 t where 
   toInteger (NT t) = foldr transform 0 t where 
      transform (T s a) 
         -- = (+) $ (*. s) . (^ a) $ 3
         -- = (+) $ (*. s) . (3 ^) $ a
         -- = (+) $ (*. s) $ 3 ^ a  -- Cummulative
         = (* 3 ^ a) . (+. s)    -- Differential


   {- | Returns quotient with positive rest always -}
   divMod n d
      | sgn r  = (q    , r)
      | sgn d  = (q - 1, r + d)
      | True   = (q + 1, r - d)
      where
      (q, r) = quotRem n d


   {- Euclidean recursive division, minimum absolute rest.
      Division is a recursive subtraction; recursively tries q values, 
      from aq down to 0 -}
   quotRem _ 0 = error "Division by 0, in Ternary.quotRem. "
   quotRem 0 _ = (0, 0)
   quotRem r d = foldr (divStep d) (0, r) [0 .. aq]
      where
      aq = 1 + ar - ad  -- initial q to try
         where
         -- divisor and rest greatest absolute terms 
         (T _ ar : _) = accumulate $ toTerms r 
         (T _ ad : _) = accumulate $ toTerms d 


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------

{- Returns new value if it produces lesser absolute rest. -}
divStep :: Ternary -> Int -> PairNT -> PairNT
divStep _ _ (q, 0) = (q, 0)   -- not neccessary, but faster just in case
divStep d aq (q, r)
   | acceptQ   = (qNew, rNew)  -- rDif < 2*r 
   | True      = (q   , r   )
   where
   (qNew, rNew) = (             q + qDif, r - rDif ) 
   (qDif, rDif) = ( NT [T (sr == sd) aq], d*qDif )                                        
   sr = sgn r
   sd = sgn d

   -- The way here is almost like using abs, but using less comparitions
   --    rNew <= -r (rNew too much negative), '=' to promote positive rest
   --    rNew >  -r (rNew too much)
   acceptQ = sr == (-rNew < r) -- rDif < 2·r

   -- r > d*qDif - r;  d*qDif < 2*r


-- Rooooots ------

rootStep :: Int -> PairNT -> PairNT

-- rootStep aq (q, r) = divStep d aq (q, r)
rootStep aq (q, r) = (qNew, rNew)
   where
   (qNew, rNew) = (     q + qDif, r - rDif )
   (qDif, rDif) = ( NT [T sr aq], d*qDif )  -- q*qDif + q*qDif + qDif^2 
   -- rDif = d*qDif = 2·q·qDif + qDif^2  = (q + qDif)^2 - q^2
   d = q + qNew   -- now it is almost like dividing r by d
   sr = sgn r


digitSquares :: [Ternary]
-- digitSquares = fmap (\i -> NT [T True i]) [0, 2 ..] 
-- digitSquares = fmap (NT . (: []) . T True) [0, 2 ..] 
digitSquares = fmap oneTerm [0, 2 ..] 


{- Square root, minimum absolute rest. -}
root :: Ternary -> Ternary
root = fst . rootRem


rootRem, rootMod :: Ternary -> PairNT


-- This is the integer square root with minimal negative rest, from digit thQ.  
-- That is, the minimal natural number whose square covers the input 'n'.
rootMod n 
   | 0 < r  = rootStep 0 (q, r)
   | True   = (q, r)
   where
   (q, r)   = rootRem n 

   
{- Square root and minimum absolute rest.
   Recursively tries q values, from half aq down to 0. 
   Negative input does not produce error, instead, and correctly, 
   it is just integer root 0, and rest the input mumber. 
-}
rootRem n = foldr rootTerm (0, n) [0 .. ar]
   where
   -- In order to try a first digit for integer square root of n, 
   -- we should start by the higher accumulated digit of 
   -- 'range' form which is the closest greater to n. 
   ar = length indexRange -- greatest digit of a square greater than n
      where
      -- i: maximum digit which is the same that 
      -- the maximum digit of the root of n: 
      -- [2·i] <= 4·n ;  [2·i] == [i]^2  
      indexRange = takeWhile (<= 4*n) digitSquares
         
   {- Returns new value if it produces lesser absolute rest. 
      Root step tries division of the rest over the new root tried, 
      to see if it produces a lesser rest. -}
   rootTerm :: Int -> PairNT -> PairNT
   rootTerm aq (q, r) 
      | acceptQ   = (qNew, rNew)
      | True      = (q   , r   )
      where
      (qNew, rNew) = rootStep aq (q, r)
      sr = sgn r
      
         -- Using abs
      -- acceptQ = ( range aq < Signed.abs r - Signed.abs q*qDif ) 

         -- The way here is almost like using abs, but using less comparitions

         -- Not using 'qDif'
      acceptQ = sr == ( -rNew <= r + sigma (2*aq - 1) ) -- should work, correct
      -- acceptQ = sr == ( 0 <= r + rNew + sigma (aq*2 - 1) ) -- should work, correct
      -- acceptQ = sr == ( 0 < r + rNew + sigma (2*aq - 1) ) -- but this seems to work
      -- acceptQ = sr == ( -rNew <= r + 2 * range aq ) -- !!!
      -- acceptQ = sr == ( -rNew < r ) -- not correct!

         -- Using 'qDif'
     -- acceptQ = sr == ( range aq < r - q*qDif ) -- the faster one  
      -- acceptQ = sr == ( range aq < rNew + qNew*qDif ) 
      -- acceptQ = sr == ( 0 <= 2*(r + rNew) + ntSqr qDif ) 
      -- acceptQ = sr == ( 2*range aq       < r + rNew + ntSqr qDif )
      -- acceptQ = sr == ( 2*rDif <= 4*r + ntSqr qDif )  -- Slower
         -- using 'sigma'
      -- acceptQ = sr == ( sigma (aq*2 - 1) < 2*(r - q*qDif) ) -- !!!
      -- acceptQ = sr == ( 0 < r + rNew + ntSqr qDif - sigma (aq*2 - 1) ) 

         -- r   >  range aq + q*qDif;   q*qDif < r - range aq
         -- 2·r >  ([2·aq] - 1)/2 + 2·q·[aq] ~= ( ([aq] + q)^2 - q^2 )/2 + q·[aq]
         
         -- 0 <= r + 3·rNew + 2·qNew·qDif
         -- qDif^2 <= 4·(r - q·qDif)
         -- (qNew + 3·q)·qDif <= 4·r


----

-- Here, 'range i' is the maximum number whose integer square root has 
-- all accummulated ternary digits less than 'i'
-- The maximum number whose integer square root has maximum accummulated ternary 
-- digit i=4 is the square of x = [ 0, 1, 1, 1, 1] plus itself, equal to: 
--    x*(x + 1) = [ 0, 1, 1, 1, 1]*[ 0-, 1-, 1-, 1-, 1-, 1] 
--              = [ 0-, 1, 1-, 1, 1-, 1, 1-, 1, 1-, 1] 
--              = tQuarter [0-, 2*(i + 1)] ;  ([i + 1]^2 - [0])/4
-- Correspondingly, it is one less than the minimum number whose integer 
-- square root has maximum ternary digit i=5 
range, sigma :: Int -> Ternary

-- The sum of divisors of 3^i = NT [i], equal to the 
-- maximum number whose maximum ternary digit is i. 
--    2 * sigma (2*i - 1) = [2*i] - 1, by the usual calculation for sigma function
--       i=0  --> [2*i] - 1 = 1 - 1 = 0
--       i=2  --> 2*(1 + 3 + 9 + 27) = 81 - 1
--    sigma 4 = NT [ 0, 1, 1, 1, 1] 
--    sigma (2*i - 1) = 2 * range i
-- sigma (-1) = 0 -- strictly 1/3, but must be integer
sigma i | sgn i  
   = NT $ 0 : replicate i 1
sigma _ = 0 -- strictly not zero, but it must be integer
-- sigma _ = error "Less than -1, in Ternary.sigma. "
      
-- range i = NT . concat . take i $ [-0, 1] : repeat [-1,1]
-- range i = NT $ take (2*i) infiRange
-- range i = NT . concat . ([-0, 1] :) . replicate i $ [-1,1]
range i = NT . concat . take i $ [-0, 1] : repeat [-1,1]

-- infinite lazy global constants, should be slightly faster and lighter access
-- Not used, it is in fact slower, not faster
allRanges = fmap range [0 ..] -- list of all possible ranges
-- infiRange = concat $ [-0, 1] : repeat [-1,1] -- infinite range

-- head . dropWhile (< abs x) $ iterate (* 3) 2
   

{- | Closest power of 3 -}
log3 :: Terms -> Int
log3 (x0 : x1 : xs)  -- abs <= [xo, tNeg (pred x0)]
   |  tNeg x0 == succ x1
   && (sgn (NT xs) == tSgn x1
   || null xs)
                     = tVal x1
log3 (x0 : _)        = tVal x0
log3 _               = error "Logarithm of 0 in Ternary.log3"



---------------------------------------------------------------------------
{- = 
   Conversion from and to accumulated 'Terms' (list of 'Term': Terms).
   'Terms' are greatest first, and accumulated, 
      21 = [ 3, 2-, 1] = 3^3 - 3^2 + 3
   'Ternary' should be smallest first, and differential.
      21 = NT [ 1, 1-, 1] = ((( 1 )*3 - 1 )*3 + 1 )*3
-}
---------------------------------------------------------------------------


-- accumulate, greatest first
accumulate :: Terms -> [Term]
accumulate r = reverse t where
   t = zipWith T s a
   s = fmap tSgn r
   v = fmap tVal r
   a = scanl1 (+) v 

-- decumulate, differentiate, smallest first
decumulate :: [Term] -> Terms
decumulate t = zipWith T s d where
   r = reverse t
   s = fmap tSgn r
   v = fmap tVal r
   d = zipWith (-) v (0 : v)
   

oneTerm :: Int -> Ternary
oneTerm x
   | sgn x  = NT [T True x]
   | True   = error "Negative argument in Ternary.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   compare, addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

cmp :: Terms -> Terms -> Ordering 
-- Needs greatest term first on inputs. 
cmp x y = cmp' (accumulate x) (accumulate y)  -- Differential
   where 
   -- recursion first, faster
   cmp' (x : xs) (y : ys) | x == y        = cmp' xs ys 
   
   cmp' (T True  vx : _) (T True  vy : _) = compare vx vy
   cmp' (T True _   : _) _                = GT
   cmp' _                (T True _   : _) = LT
   cmp' (T _ vx : _)     (T _ vy     : _) = compare vy vx
   
   cmp' [] [] = EQ 
   cmp' []  _ = GT 
   cmp' _   _ = LT 


sgnTerms :: Terms -> Bool
sgnTerms [] = True
sgnTerms x  = sgn $ NT x

absTerms :: Terms -> Terms
absTerms [] = []
absTerms x  = if sgnTerms x then x else neg x 

compareAbs :: Terms -> Terms -> Ordering 
compareAbs x y = cmp (absTerms x) (absTerms y)

{- | Minimum when comparing absolute values, 
   should be used for every rest. -}
minAbs :: Terms -> Terms -> Terms 
minAbs x y = case compareAbs x y of
   GT -> y
   LT -> x
   EQ -> x  -- being equal abs, the first is chosen 


{- | Addition, for both, positive and negative terms 
-}
add, sub :: Terms -> Terms -> Terms

-- Addition when notation is smallest term first. 
-- Symetric version. Faster to put recursion first:  
add (T sy vy : ys) (T sx vx : xs) 
   | vx > vy   = T sy vy       : add (T sx (vx - vy) : xs)  ys
   | vx < vy   = T sx vx       : add (T sy (vy - vx) : ys)  xs 
   | sx == sy  = T (not sx) vx : add [T sx 1]  (add xs ys) -- carry term
   | True      = pw3 vx (add ys xs) -- x==-y ; accumulate xs
add ys [] = ys

-- Single term version. Very much slower and mem consumer. 
-- add [T sy vy] (T sx vx : xs) 
   -- | vx < vy   = T sx vx       : add [T sy (vy - vx)]  xs 
   -- | vx > vy   = T sy vy       :      T sx (vx - vy) : xs
   -- | sx == sy  = T (not sx) vx : add [T sx 1        ]  xs -- carry term
   -- | True      = pw3 vx xs    -- x==-y ; accumulate xs
-- add [x] [] = [x]
-- general terms
-- add (y : ys) xs 
   -- = add [y] 
   -- . add xs 
   -- $ pw3 (tVal y) ys -- swap, much faster
add [] xs = xs


sub (T sy vy : ys) (T sx vx : xs) 
   | vx > vy   = T (not sy) vy       : sub ys (T sx (vx - vy) : xs) 
   | vx < vy   = T sx vx       : sub (T sy (vy - vx) : ys)  xs 
   | sx /= sy  = T sy vx : add [T sx 1]  (sub ys xs) -- carry term
   | True      = pw3 vx (sub ys xs) -- x==-y ; accumulate xs
sub ys [] = neg ys

-- sub (T sy vy : ys) xs
   -- = add [T (not sy) vy] 
   -- . add xs 
   -- $ (mul [T False vy] ys) -- swap, like in 'add'
sub [] xs = xs

-- sub = add . neg   -- faster or equal than the explicit version


neg :: Terms -> Terms
neg = fmap tNeg



-----------------------------------------------------------------------
-- * Base operations: bit-shift, triplicate, third,
---------------------------------------------------------------------------

-- | Multiplication by power of 3, MULtiple TRIPLication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
pw3 :: Int -> Terms -> Terms
pw3 n (x : xs) = incr n x : xs 
pw3 n _ = [] 


-- Terms double, triple and quadruple
tDup, tQuad, tTri, tHalf, tQuarter, tThird :: Terms -> Terms
-- tDup x   = add x x
tDup (T sx vx : xs) = T (not sx) vx : add [T sx 1] (tDup xs)   -- just carry
tDup [] = []
-- tDup  x  = sub x $ tTri x   -- should not be slower than add x x, because simpler cases
tQuad x  = add x $ tTri x  -- faster than tDup . tDup
-- tQuad  = tDup . tDup

-- | Triplicate: multiply by 3, faster than add; 
-- third: like dividing by 3; 
-- they are a particular case of product by power of 2: pw3
tTri     = pw3 1     -- mul [1]
tThird   = pw3 (-1)  -- div [1]

half' :: [Term] -> [Term]  -- not Terms type, because Terms is not accumulated 
half' (x : y : xs)
   | x == -y   =      half' xs
   | x == y    = x  : half' xs
half' [T False 0] = [T False 0]  -- half (-1), in order to keep floor of half 
half' [T _ 0] = []               -- half (+1) 
half' (x : xs) = carry $ x1 : half' (x1 : xs)
   where 
   x1 = pred x
   carry (x : y : ys) 
      | x == -y   = ys
      | x == y    = succ x : -y : ys
   carry xs = xs
half' _  = []

tHalf    = decumulate . half' . accumulate
tQuarter = decumulate . half' . half' . accumulate
   

ntTri, ntThird, ntDup, ntHalf, ntQuad, ntQuarter :: Ternary -> Ternary
ntTri     (NT x)  = NT $ tTri x  -- takes more comparisons of conditions
ntThird   (NT x)  = NT $ tThird x

ntDup     (NT x)  = NT $ tDup x
ntHalf    (NT x)  = NT $ tHalf x

ntQuad    (NT x)  = NT $ tQuad x
ntQuarter (NT x)  = NT $ tQuarter x



-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

mul, mulE  :: Terms -> Terms -> Terms  -- , mulS, mulF

-- | Product by a one term element, equivalent to pw3
-- mul [T sa aa] [T sb ab] = [T (sa == sb) (aa + ab)]
-- mul [T sx ax] (T s v : ys) = T (sx == s) (ax + v) : mul [T sx ax] ys
mul [T True ax] = pw3 ax
mul [T _    ax] = pw3 ax . neg 
-- mul xs [y] = mul [y] xs

-- General product, several methods can be chosen
mul x = mulS x
-- mul x = mulE x
-- mul x = mulF x

{- | Multiplication. Egyptian method, O(n^2). It is a 
   'concatMap', or 'foldMap', mapping 'mul' and folding 'add' 
-}
mulE (y : ys) xs  -- y : ys = add [y] (pw3 (tVal y) ys)
   = add (mul [y] xs) 
   . mulE xs 
   $ pw3 (tVal y) ys   -- swap, faster because of symetry; accumulate ys
mulE _ _  = [] 

-- mulE xs ys = foldr f 0 $ accumulate ys

-- mulE xs ys = foldr f [] ys
   -- where 
   -- f (T s v) 
      -- | s      = pw3 v . (add xs)   -- Differential
      -- | True   = pw3 v . (sub xs) 
   
--   377 = NT [0-,3-,1- 1- 1] 
--       = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(-1 + 3^1*(1 )))))
   -- toInteger (NT t) = foldr transform 0 t where 
      -- transform (T s a) 
         -- = (* 3 ^ a) . (+. s)    -- Differential


{- | Straightforward multiplication, O(n^2), -}
mulS (T sx vx : xs) (T sy vy : ys) 
   = ( T (sx == sy) (vx + vy) : ) 
   . ( if sy then add else sub ) xs 
   . ( if sx then add else sub ) ys  
   $ mulS xs ys 
mulS _ _ = []


{- | We define mulF to be x*y = 1/4(a^2 - b^2),
   where a = x + y; b = x - y ('F' for 'Fast' or 'Fermat').
   Complexity is mainly on sqr, as the rest of the algorithm is <~ O(size). 
   Speed depends on 'sub' speed, not just on 'add' speed -}
mulF xs ys = tQuarter $ sub sqSub sqSum   -- 1/4(A^2 - B^2)
   where sqSub  = sqr $ sub xs ys   -- squared difference: (ys - xs)^2
         sqSum  = sqr $ add xs ys   -- squared sum:        (ys + xs)^2


{- | Square, O(n^2), it should be faster than @mul xs xs@ -}
sqr :: Terms -> Terms
-- | recursive version:  2*x*xs + (x^2 + xs^2), should be slightly faster than just 
-- multiplying, as the terms x^2 and 2·x are calculated appart in a simpler way
sqr (T sx vx : xs) 
   = (T True (vx + vx) :)  -- x^2
   . ( if sx then add else sub ) (tDup xs)   -- (+-) 2·x
   $ (sqr xs)  -- + xs^2
sqr _ = []

ntSqr :: Ternary -> Ternary
ntSqr (NT x) = NT (sqr x)


-- All powers of 3 are odd, so it is easy to know if a Ternary is even or odd
isEven (NT x) = even $ length x
isOdd  (NT x) = odd  $ length x

