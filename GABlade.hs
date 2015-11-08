module GABlade
       (bladeProd, bladeGrade, bladeList, bladeShow)
       where

{- Only want to crate one new datatype - the ADT for multivector.
Blades are an internal concept, not exposed. A blade is multivector with one component.
Biggest design choice is how to deal with negative values in the basis blade products. 
Decision here is to use id and negate functions in the product -}

--import Data.Char
import Data.List



-- Blade representation
{- Blades are represented internally as a positive Int or Integer. 
When a multivector is constructed you chose which is appropriate. 
Int is valid for up to 15 dimensions, though this may be platform dependent.  
For the internal product we convert the integer to a binary list.
-}

int2bin :: (Integral n) => n -> [Int]
int2bin 0 = [0]
int2bin 1 = [1]
int2bin n 
  | even n = 0: int2bin (n `div` 2)
  | otherwise = 1: int2bin ((n-1) `div` 2)

bin2int :: (Integral n) => [Int] -> n
bin2int [0] = 0
bin2int [1] = 1
bin2int (x:xs) 
  | x ==0 = 2 * (bin2int xs)
  | otherwise = 1 + 2 * (bin2int xs)


--Blade product. 
{- 
The product of two blades starts with them stored as Ints or Integers. 
These are converted to a binary list. 
The countswap operation brings the combined list into correct order.
The countneg operation counts the number of negative norm basis vectors.
The final operation returns the final basis Int / Integer and a function.
TODO Should memoise bldProd
-}

countSwap :: [Int] -> [Int] -> Int
countSwap xs ys = countswap' 0 xs (0:(scanl1 (+) ys))
  where countswap' n [x] ys = n + x*(head ys)
        countswap' n xs [y] =  countswap' (n + y* (head xs)) (tail xs) [y]
        countswap' n (x:xs) (y:ys) = countswap' (n+x*y) xs ys
                        
countNeg :: [Int] -> [Int] -> Int
countNeg xs ys = sum res
  where res' = zipWith (*) xs ys   
        res = zipWith (*) res' (cycle [0,1]) 

resBld :: [Int] -> [Int] -> [Int]
resBld [] [] = []
resBld xs [] = xs
resBld [] ys = ys
resBld (x:xs) (y:ys) = ((x+y) `mod` 2) : (resBld xs ys)

bldProd :: (Integral n, Num a) => n -> n -> (n, a->a)
bldProd n m = ((bin2int (resBld nb mb)),fn)
  where nb = int2bin n
        mb = int2bin m
        tmp = ((countSwap nb mb) + (countNeg nb mb)) `mod` 2
        fn = if tmp == 0 then id else negate
        
bladeProd :: (Num a, Integral n) => (n,a) -> (n,a) -> (n,a)
bladeProd (n,a) (m,b) = (r,x)
  where (r,fn) = bldProd n m
        x = fn (a*b) 
  
--Show Function

bin2str :: [Int] -> String
bin2str str = bin2str' str efs lst 
  where efs = cycle ['E','F']
        lst = merge' [1..] [1..]
        bin2str' (n:ns) (e:es) (l:ls) = (res n e l)++(bin2str' ns es ls)
        bin2str' [] _ _ = []
        res n e l = if n==0 then [] else e:(show l) 

bladeShow :: (Num a, Show a, Integral n) => (n,a) -> String
bladeShow (n,a)
   | n == 0 = show a
   | otherwise = show a++"*"++(bin2str (int2bin n))
  
--Utlity Functions
bladeGrade :: (Integral n) => n -> Int
bladeGrade = sum.int2bin

--Utility function to make list [1,1,2,2,3,3...]
merge' :: [a] -> [a] -> [a]
merge' [] ys = ys
merge' (x:xs) ys = x:merge' ys xs

--BladeList: returns a blade as a list of unit basis vectors 
bladeList :: (Integral n, Num a) => (n,a) -> [(n,a)]
bladeList (n,_) = map toBld (int2IntList n) 
  where toBld i = (i,1)


int2IntList :: (Integral n) => n -> [n]
int2IntList n = int2IntList' 0 n
  where int2IntList' _ 0 = []
        int2IntList' i n 
          | even n = int2IntList' (i+1) (n `div` 2)
          | otherwise = (2^i) : int2IntList' (i+1) ((n-1) `div` 2)


