module GABlade
       (bladeProd, bladeGrade, bladeList, bladeShow, bldProd)
       where

{- Only want to create one new datatype - the ADT for multivector.
Blades are an internal concept, not exposed. A blade is multivector with one component.
Biggest design choice is how to deal with negative values in the basis blade products.
Compute this separately and then apply id or negate.-}

--imports: Data.Char
import Data.List
import Data.Array


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

bladeGrade :: (Integral n) => n -> Int
bladeGrade = sum.int2bin


--Blade product. 
{- 
The product of two blades starts with them stored as Ints or Integers. 
These are converted to a binary list. 
The countswap operation brings the combined list into correct order.
The countneg operation counts the number of negative norm basis vectors.
The final operation returns the final basis Int / Integer and a function.
-}

zeroes :: [Int]
zeroes = 0:zeroes

countSwap :: [Int] -> [Int] -> Int
countSwap xs ys =foldl' (+) 0 zs 
  where zs = zipWith (*) (tail xs) (scanl1 (+) (ys++zeroes))

countNeg :: [Int] -> [Int] -> Int
countNeg xs ys = sum res
  where res' = zipWith (*) xs ys   
        res = zipWith (*) res' (cycle [0,1]) 

--resBld is essentially a binary xor, but cannot use zip as we need to keep the terms in the longer list
resBld :: [Int] -> [Int] -> [Int]
resBld xs [] = xs
resBld [] ys = ys
resBld (x:xs) (y:ys) = ((x+y) `mod` 2) : (resBld xs ys)

bldProd :: (Integral n) => n -> n -> (n, Int)
bldProd n m = ((bin2int (resBld nb mb)),sn)
  where nb = int2bin n
        mb = int2bin m
        sn = ((countSwap nb mb) + (countNeg nb mb)) `mod` 2
        
bladeProd :: (Num a, Integral n) => (n,a) -> (n,a) -> (n,a)
bladeProd (n,a) (m,b) = (r,x)
  where (r,sn) = bldProd n m
        x = if sn==0 then (a*b) else negate (a*b) 
  
--Show Function
bin2str :: [Int] -> String
bin2str str = bin2str' str efs lst 
  where efs = cycle ['E','F']
        lst = concatMap (replicate 2) [1..]
        bin2str' (n:ns) (e:es) (l:ls) = (res n e l)++(bin2str' ns es ls)
        bin2str' [] _ _ = []
        res n e l = if n==0 then [] else e:(show l) 

bladeShow :: (Num a, Show a, Integral n) => (n,a) -> String
bladeShow (n,a)
   | n == 0 = show a
   | otherwise = show a++"*"++(bin2str (int2bin n))
  

--BladeList: returns a blade as a list of unit basis vectors. Use this in the outermorphism.
--Note that this discards the scale, so returns a unit blade. 
bladeList :: (Integral n, Num a) => (n,a) -> [(n,a)]
bladeList (n,_) = map toBld (int2IntList n) 
  where toBld i = (i,1)

int2IntList :: (Integral n) => n -> [n]
int2IntList n = int2IntList' 0 n
  where int2IntList' _ 0 = []
        int2IntList' i n 
          | even n = int2IntList' (i+1) (n `div` 2)
          | otherwise = (2^i) : int2IntList' (i+1) ((n-1) `div` 2)


