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
   Term(..), Ternary(..), NT(..), PairNT, -- data types
   oneTerm, pair2i, accumulate, decumulate,  -- conversion
   half, dup, sgn,   -- from imported modules
   neg, add, sub, mul, sqr,    -- terms arithmetic 
   cmp, sgnTerms, absTerms, compareAbs, minAbs, -- comparation
   pw3, ntTripl, ntThird, ntDup, ntHalf, ntQuad, ntQuarter, ntSqr,      -- geometric
   log3, root, rootRem, divStep, rootStep       -- inverse
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
newtype Ternary = NT [Term] deriving (Eq, Show, Read)
type NT = Ternary   -- just for brevity

type PairNT = (Ternary, Ternary)
pair2i :: PairNT -> (Integer, Integer)
pair2i (x, y) = (toInteger x, toInteger y)

type PairTerms = ([Term], [Term])
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
   sgn (NT [])       = True
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
   (+) (NT x) (NT y) = NT $ add x y
   (*) (NT x) (NT y) = NT $ mul x y


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
   -- quotRem (NT r) (NT d) = divStep d qs (0, r)
   -- quotRem (NT r) (NT d) = (NT q, NT r)
      -- where
      -- (q, r) = foldl' (divStep d) qs ([], r)
   quotRem (NT r) (NT d) = (NT quotient, NT remainder)
      where
      -- (quotient, remainder) = foldr f ([], r) qs
      -- f x xs = divStep d xs x
      -- (quotient, remainder) = f qs where
         -- f (x : xs) = divStep d x (f xs)
         -- f [] = ([], r)
      (quotient, remainder) = foldr (divStep d) ([], r) qs
      
      ad = tVal . head . accumulate $ d   -- divisor greatest term
      ar = tVal . head . accumulate $ r   -- rest greatest term
      aq = 1 + ar - ad  -- initial q to try
      qs = [0 .. aq]    -- list of q to try


---------------------------------------------------------------------------
{- = Inverse geometric operations, with integer rest: 
   division, square root, gcd, -}
---------------------------------------------------------------------------

{- Returns new value if it produces lesser absolute rest -}
divStep :: [Term] -> Int -> ([Term], [Term]) -> ([Term], [Term])
divStep _ _ (q, []) = (q, [])
divStep d aq (q, r)
   | rejectQ   = (q   , r   )
   | True      = (qNew, rNew)  -- rDif < 2*r 
   where
   -- '=' to promote smaller quotient, positive rest
   -- rejectQ = S.abs r <= S.abs rNew (not rDif < 2*r)
   -- The way here is almost like using abs, but using less 
   -- comparitions, and gives more control of cases
   rejectQ  | sr    = not (GT == cmp rNew (neg r)) -- rNew <= -r (rNew too much negative)
            | True  =      GT == cmp rNew (neg r)  -- rNew >  -r (rNew too much)
   (qNew, rNew) = ( add q qDif, sub rDif r )
      where
      qDif = [T (sr == sd) aq]                                          
      rDif = mul qDif d   -- (sr == sd) *. pw3 aq $ d
   sr = sgn $ NT r
   sd = sgn $ NT d


{- Square root, minimum absolute rest.
   Recursively tries q values, from half aq down to 0. -}
rootRem :: Ternary -> PairNT
-- rootRem ()  = error "Square root of negative number, in Ternary.root"
rootRem (NT n) = (NT quotient, NT remainder)
   where
   (quotient, remainder) = foldr rootStep (qNew, rNew) qs
   -- (quotient, remainder) = f qs
      -- where
      -- f [] = ([], n)
      -- f (x : xs) = rootStep x (f xs)
   sr    = sgn $ NT n
   qNew  = [T sr ar] 
   rDif  = [T True (ar + ar)] -- mul qNew qNew
   rNew  = sub rDif n
      
   qs = [0 .. ar - 1]   -- up to half of rest greatest term

   -- The maximum number whose integer square root has maximum ternary digit 4 
   -- is the square of x = [ 0, 1, 1, 1, 1] plus itself, equal to 
   --    x*(x + 1) = [ 0, 1, 1, 1, 1]*[ 0-, 1-, 1-, 1-, 1-, 1] = 
   --    =  [ 0-, 1, 1-, 1, 1-, 1, 1-, 1, 1-, 1]
   -- Then, in order to try a first digit for integer square root of n, 
   -- we should start by the integer half of the higher accumulated digit of 
   -- this form which is the closest greater to n. 
   -- 
   -- Greatest digit of a square greater than n
   -- ar = half $ length (maxi n) - 1
   ar = fst (maxi n)
maxi :: [Term] -> (Int, [Term])
maxi [] = (0,[])
maxi n = last . takeWhile ((GT ==) . cmp n . snd) $ indexRange
indexRange = zip [0 ..] $ fmap range [0 ..]

   
-- (i, x): maximum number x whose maximum digit i is the same that 
-- the maximum digit of the root of n: 
range i = take (2*i) ranges

-- List of maximum numbers for each ternary digit
ranges :: [Term]
-- ranges = iterate (++ [-1, 1]) [-0, 1] -- (++ [-1,1])
ranges = concat . ([-0, 1] :) $ repeat [-1,1]


root :: Ternary -> Ternary
root = fst . rootRem


{- Returns new value if it produces lesser absolute rest. 
   Root step tries division of the rest over the new root tried, 
   to see if it produces a lesser rest. -}
rootStep :: Int -> ([Term], [Term]) -> ([Term], [Term])
rootStep _ (q, []) = (q, [])  -- exact root
-- rootStep aq (q, r) = divStep d aq (q, r)
-- rootStep aq (q, r) = (qNew, rNew)
rootStep aq (q, r) 
   | rejectQ   = (q   , r   )
   | True      = (qNew, rNew)
   where
   sr    = sgn $ NT r
   qDif  = [T sr aq] 
   qNew  = add q qDif
   
   d     = add qNew q   -- now it is like dividing r by d
   rDif  = mul qDif d 
   rNew  = sub rDif r
   -- '=' to promote smaller quotient, positive rest
   -- rejectQ = S.abs r <= S.abs rNew (not rDif < 2*r)
   -- The way here is almost like using abs, but using less 
   -- comparitions, and gives more control of cases
   -- rejectQ  | sr    = not (GT == cmp rNew (neg r)) -- rNew <= -r (rNew too much negative)
            -- | True  =      GT == cmp rNew (neg r)  -- rNew >  -r (rNew too much)
   rejectQ  | sr    = not (GT == cmp r maxRem) -- rNew <= -r (rNew too much negative)
            | True  =     (GT == cmp r maxRem)  -- rNew >  -r (rNew too much)
   maxRem = add (range aq) $ mul qDif q   -- maximum remainder
   
   
   -- (qNew, rNew) = ( add qDif q, sub rDif r )
   -- d = add qNew q
   -- qDif = [T sr aq]                                          
   -- rDif = mul qDif d   -- (sr == sd) *. pw3 aq $ d



{- | Closest power of 3 -}
log3 :: [Term] -> Int
log3 (x0 : x1 : xs)  -- abs <= [xo, tNeg (pred x0)]
   |  tNeg x0 == succ x1
   && (sgn (NT xs) == tSgn x1
   || xs == [])
                     = tVal x1
log3 (x0 : _)        = tVal x0
log3 _               = error "Logarithm of 0 in Ternary.log3"
-- head . dropWhile (< abs x) $ iterate (* 3) 2



---------------------------------------------------------------------------
{- = 
   Conversion from and to accumulated 'Terms' (list of 'Term': [Term]).
   'Terms' are greatest first, and accumulated, 
      21 = [ 3, 2-, 1] = 3^3 - 3^2 + 3
   'Ternary' should be smallest first, and differential.
      21 = NT [ 1, 1-, 1] = ((( 1 )*3 - 1 )*3 + 1 )*3
-}
---------------------------------------------------------------------------


-- accumulate, greatest first
accumulate :: [Term] -> [Term]
accumulate t = reverse $ zipWith T s a where
   -- t = reverse rt
   s = fmap tSgn t
   v = fmap tVal t
   a = scanl1 (+) v 

-- decumulate, differentiate, smallest first
decumulate :: [Term] -> [Term]
decumulate rt = zipWith T s d where
   t = reverse rt
   s = fmap tSgn t
   v = fmap tVal t
   d = zipWith (-) v (0 : v)
   

oneTerm :: Int -> Ternary
oneTerm x
   | sgn x  = NT [T True x]
   | True   = error "Negative argument in Ternary.oneTerm. "

---------------------------------------------------------------------------
{- = Arithmetic operations: 
   compare, addition, substraction, opposite, absolute, (un)carry, successor, -}
---------------------------------------------------------------------------

cmp :: [Term] -> [Term] -> Ordering 
-- Needs greatest term first on inputs
cmp x y = cmp' accX accY  -- Differential
   where -- greatest term first
   accX = accumulate x
   accY = accumulate y

   cmp' (x : xs) (y : ys)
         | x < y  = LT 
         | x > y  = GT
         | True   = cmp' xs ys      
   cmp' (x : _) [] = if tSgn x then GT else LT
   cmp' [] (y : _) = if tSgn y then LT else GT
   cmp' [] []    = EQ

sgnTerms :: [Term] -> Bool
sgnTerms [] = True
sgnTerms x = sgn $ NT x

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
-- Addition when notation is smallest term first. 

-- -- A good way to do it is to accumulate, do the calculus, and deccumulate 
-- add xs ys = decumulate $ addCumul (accumulate xs) (accumulate ys)
   -- -- where
-- addCumul xs [] = xs 
-- -- addCumul [] ys = ys
 -- -- single term
-- addCumul [y] (x : xs) 
   -- | tVal x > tVal y    = y : x : xs
   -- | tVal x < tVal y    = x : addCumul [y] xs
   -- | x == y             = tNeg y : addCumul [succ x] xs   -- Carry
   -- | True               = xs  -- x == tNeg y 
-- -- Now, recursion: 
-- -- addCumul (y : ys) xs = addCumul [y] $ addCumul xs ys -- swap, much faster
-- addCumul ys xs = foldr f ys xs
   -- where f y = addCumul [y]


-- Now, recursion: 
add [] xs = xs
add (T sy vy : ys) xs = addTerm (T sy vy) . add xs $ pw3 vy ys -- swap, much faster; accumulate ys
   where
-- add ys xs = foldr f xs ys
   -- where 
   -- f (T sy vy) = (addTerm (T sy vy) . pw3 vy)
   -- single term
   addTerm x [] = [x] 
   addTerm (T sy vy) (T sx vx : xs) 
      | vx > vy   = T sy vy       :          T sx (vx - vy) : xs
      | vx < vy   = T sx vx       : addTerm (T sy (vy - vx))  xs
      | sx == sy  = T (not sy) vy : addTerm (T sy 1        )  xs -- Carry
      | True      = pw3 vx xs -- x == tNeg y ; accumulate xs


-- sub [] xs = xs
-- sub (T sy vy : ys) xs = subTerm (T sy vy) . sub (pw3 vy ys) $ xs  -- accumulate ys
   -- where
   -- f (T sy vy) = subTerm (T sy vy) . pw3 vy
   -- -- single term
   -- subTerm x [] = [tNeg x] 
   -- subTerm (T sy vy) (T sx vx : xs) 
      -- | vx > vy   = T (not sy) vy :    T sx (vx - vy) : xs
      -- | vx < vy   = T sx vx : subTerm (T sy (vy - vx))  xs
      -- | sx /= sy  = T sy vy : subTerm (T sy 1        )  xs -- Carry
      -- | True      = pw3 vx xs -- x == y ; accumulate xs


sub = add . neg   -- faster than the upper explicit version

neg :: [Term] -> [Term]
neg = fmap tNeg



-----------------------------------------------------------------------
-- * Base operations: bit-shift, triplicate, third,
---------------------------------------------------------------------------

-- | Multiplication by power of 3, MULtiple TRIPLication, bitwise left shift.
-- Power can be negative, which will give division, but
-- it should be checked that n >= -v; otherwise result would be rounded, 
-- (or fractional CLog should be defined).
pw3 :: Int -> [Term] -> [Term]
pw3 n (x : xs) = incr n x : xs 
pw3 n _ = [] 


-- Terms double, triple and quadruple
tDup, tQuad, tTri, tHalf, tQuarter, tThird :: [Term] -> [Term]
tDup x   = add x x
-- tDup  x  = sub x $ tTri x   -- should not be slower than add x x, because simpler cases
tQuad x  = add x $ tTri x

-- | Triplicate: multiply by 3, faster than add; 
-- third: like dividing by 3; 
-- they are a particular case of product by power of 2: pw3
tTri     = pw3 1            -- mul [1]
tThird   = pw3 (-1)            -- mul [1]

half' :: [Term] -> [Term]
half' (x : y : xs)
   | x == y          = x  : half' xs
   | x == tNeg y     =      half' xs
half' [T _ 0] = []
half' (x : xs) = x1 : half' (x1 : xs)
   where x1 = pred x
half' _  = []

tHalf    = decumulate . half' . accumulate
tQuarter = decumulate . half' . half' . accumulate
   

ntTripl, ntThird, ntDup, ntQuad, ntHalf, ntQuarter :: Ternary -> Ternary
ntTripl (NT x)   = NT $ tTri  x  -- takes more comparisons of conditions
ntThird (NT x)   = NT $ tThird x

ntDup x    = x + x
ntQuad x   = x + ntTripl x

ntHalf    (NT x) = NT . tHalf $ x
ntQuarter (NT x) = NT . tQuarter $ x



-----------------------------------------------------------------------
-- * Geometric direct operations: multiplication, square
---------------------------------------------------------------------------

-- mul, mulF  :: Ternary -> Ternary -> Ternary
mul, mulE  :: [Term] -> [Term] -> [Term]

-- | Product by a one term element, equivalent to pw3
-- mul [T sa aa] [T sb ab] = [T (sa == sb) (aa + ab)]
-- mul [T sx ax] (T s v : ys) = T (sx == s) (ax + v) : mul [T sx ax] ys
mul [T True ax] = pw3 ax
mul [T _    ax] = pw3 ax . neg 
-- mul xs [y] = mul [y] xs

-- General product, several methods can be chosen
mul x = mulE x
-- mul x = mulS x
-- mul x = mulF x

{- | Multiplication. Egyptian method, O(n^2). It is a 
   'concatMap', or 'foldMap', mapping 'mul' and folding 'add' 
-}
-- mulE xs ys = foldr f 0 $ accumulate ys

-- mulE xs ys = foldr f [] ys
   -- where 
   -- f (T s v) 
      -- | s      = pw3 v . (add xs)   -- Differential
      -- | True   = pw3 v . (sub xs) 
   
mulE (y : ys) xs 
   = add (mul [y] xs) 
   $ mulE xs (pw3 (tVal y) ys)   -- faster because of symetry
mulE _ _  = [] 

--   377 = NT [0-,3-,1- 1- 1] 
--       = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(-1 + 3^1*(1 )))))
   -- toInteger (NT t) = foldr transform 0 t where 
      -- transform (T s a) 
         -- = (* 3 ^ a) . (+. s)    -- Differential





{- | Straightforward multiplication, O(n^2), -}
mulS (T sa aa : xs) (T sb ab : ys) 
   = add [T (sa == sb) (aa + ab)] 
   . add (mul [T sa aa] ys)
   . add (mul [T sb ab] xs)
   $ mulS xs ys
mulS _ _ = []

{- | We define mulF to be x*y = 1/4(a^2 - b^2),
   where a = x + y; b = x - y ('F' for 'Fast' or 'Fermat').
   But ... quarter is not simple in Ternary.
   Complexity is mainly on sqr, as the rest of the algorithm is <~ O(size). -}
mulF xs ys = tQuarter $ sub sqSub sqSum   -- 1/4(A^2 - B^2)
   where sqSub = sqr $ sub xs ys   -- squared difference: (ys - xs)^2
         sqSum = sqr $ add xs ys   -- squared sum:        (ys + xs)^2


{- | Square, O(n^2), it should be faster than @mul xs xs@, but it is much slower -}
sqr :: [Term] -> [Term]
-- | recursive version:  2*x*xs + (x^2 + xs^2), should be slightly faster than just 
-- multiplying, as the terms x^2 and 2·x are calculated appart in a simpler way
sqr (T s x : xs) = T True (x + x) : sqrTail 
   where
   sqrTail 
      | s       = add (tDup xs) $ sqr xs
      | True    = sub (tDup xs) $ sqr xs
   --  . (if s then add else sub) (tDup xs) $ sqr xs
sqr _ = []

ntSqr :: Ternary -> Ternary
ntSqr (NT x) = NT (sqr x)
-- ntSqr (NT [T _ x]) = oneSInt (dup x) -- single term square


-- All powers of 3 are odd, so it is easy to know if a Ternary is even or odd
isEven (NT x) = even $ length x
isOdd  (NT x) = odd  $ length x
