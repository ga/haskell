import Data.Char
import Data.List

--Datatypes and constructors
newtype Blade a n = Blade (a,n) 
blade :: (Num a) => (a,String) -> Blade a Int   
blade (a,s) = Blade (a,bld2int s)

longBlade :: (Num a) => (a,String) -> Blade a Integer
longBlade (a, s) = Blade (a,bld2int s)

newtype Multivector a n = Mv [Blade a n]
mv :: (Num a) => [(a,String)] -> Multivector a Int  
mv xs = Mv (sort (map blade xs))

longMv :: (Num a) => [(a,String)] -> Multivector a Integer  
longMv xs = Mv (sort (map longBlade xs))

--Extracting value from blade

bldVal :: (Num a, Integral n) => Blade a n -> a
bldVal (Blade (x,_)) = x 

--Read and show functions

instance (Show a, Integral n) => Show (Multivector a n) where
  show (Mv []) = "0"       
  show (Mv [x]) = show x
  show (Mv (x:xs)) = show x++"+"++(show (Mv xs))

instance (Show a, Integral n) => Show (Blade a n) where
         show (Blade (a, n))
           | n == 0 = show a
           | otherwise = show a++"*"++(bin2str (int2bin n))

bldRead :: String -> [(Char,Int)] 
bldRead [] = []
bldRead st = (ef,n): (bldRead st')
  where ef = head st
        (n',st') = break (not.isDigit) (tail st)
        n = read n' :: Int

bin2str :: [Int] -> String
bin2str str = bin2str' str efs lst 
  where efs = cycle ['E','F']
        lst = merge' [1..] [1..]
        bin2str' (n:ns) (e:es) (l:ls) = (res n e l)++(bin2str' ns es ls)
        bin2str' [] _ _ = []
        res n e l = if n==0 then [] else e:(show l) 

--Utility function to make list [1,1,2,2,3,3...]
merge' :: [a] -> [a] -> [a]
merge' [] ys = ys
merge' (x:xs) ys = x:merge' ys xs
 
mvgroup :: (Num a, Integral n) => Multivector a n -> [[Blade a n]]
mvgroup (Mv xs) = groupBy cmp xs 
  where cmp (Blade b1) (Blade b2) = (grade (Blade b1)) == (grade (Blade b2))

bld2int :: (Integral n) => String -> n
bld2int str = foldl stp 0 (bldRead str) 
  where stp acc x = acc + (bvec2int x)
        
homogshow :: (Show a, Integral n) => [(Blade a n)] -> String
homogshow ([]) = ""       
homogshow ([x]) = show x
homogshow (x:xs) = (show  x)++opr++(homogshow xs) 
  where opr = if (head (show (head xs))) == '-' then "" else "+" 


--Equality and Number Classes
        
instance (Num a, Integral n) => Eq (Blade a n ) where 
  Blade (_,m) == Blade (_,n) = m == n

instance (Num a, Integral n) => Ord (Blade a n ) where
  Blade (_,m) <= Blade (_,n) 
    | sum (int2bin m) < sum (int2bin n) = True
    | sum (int2bin m) > sum (int2bin n) = False
    | otherwise = (m <= n)

instance (Eq a, Num a, Integral n) => Eq (Multivector a n ) where
  Mv xs == Mv ys = xs==ys

  
  
--Multivector Functions

mvsort :: (Num a, Integral n) => Multivector a n -> Multivector a n
mvsort (Mv xs) = Mv (sort xs)

simpbdlist :: (Eq a, Num a, Integral n) => [(Blade a n)] -> [(Blade a n)]
simpbdlist = removeZeros.bdlssimp.sort

mvsimp :: (Eq a, Num a, Integral n) => Multivector a n -> Multivector a n
mvsimp (Mv xs) = Mv (simpbdlist xs)

mvadd :: (Num a, Eq a, Integral n) => Multivector a n -> Multivector a n  -> Multivector a n  
mvadd (Mv xs) (Mv ys) =  Mv (simpbdlist xs++ys)

mvprod :: (Num a, Eq a, Integral n) => Multivector a n -> Multivector a n  -> Multivector a n  
mvprod (Mv xs) (Mv ys) = Mv (simpbdlist [bladeprod x y | x<- xs, y<-ys])

project :: (Num a, Integral n) => (Multivector a n, Int) -> Multivector a n 
project ((Mv xs), m) = Mv (filter isgrade xs)
  where isgrade x = (grade x) == m

sclpart :: (Eq a, Num a, Integral n) => Multivector a n -> a
sclpart mvin = foldl (+) 0 (map bldVal xs)
  where Mv xs = mvsimp (project (mvin,0))

toMv :: (Num a, Integral n) => a -> Multivector a n
toMv x = Mv [Blade (x,0)]
 
mvNeg :: (Num a, Integral n) => Multivector a n-> Multivector a n 
mvNeg (Mv xs) = Mv (map bldNeg xs) 
  where bldNeg (Blade (x,n)) = Blade (negate x,n)

 
instance (Eq a, Num a, Integral n) => Num (Multivector a n ) where
  (+)  =  mvadd
  (*)  =  mvprod 
  fromInteger m = toMv (fromInteger m)
  negate = mvNeg
-- abs and signum don't make sense for non-division algebras, so just set
  abs = id
  signum _ = toMv 1
  

--Interchange representations

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
                
bvec2int :: (Integral n) => (Char,Int) -> n
bvec2int (ch,n) 
  | ch == 'E' = 2^(2*(n-1))
  | ch == 'F' = 2^(2*n-1)
  | otherwise = error("invalid blade definition") 
                
                
--The Geometric Product for Blades

bladeprod :: (Num a, Integral n) => Blade a n -> Blade a n -> Blade a n
bladeprod (Blade (a,n)) (Blade (b,m)) = Blade (x,r)
  where (fn,r) = bldprod n m
        x = fn (a*b) 

bldprod :: (Integral n, Num a) => n -> n -> (a->a,n)
bldprod n m = (fn,(bin2int (resbld nb mb)))
  where nb = int2bin n
        mb = int2bin m
        tmp = ((countswap nb mb) + (countneg nb mb)) `mod` 2
        fn = if tmp == 0 then id else negate

countswap :: [Int] -> [Int] -> Int
countswap xs ys = countswap' 0 xs (0:(scanl1 (+) ys))

countswap' :: Int -> [Int] -> [Int] -> Int
countswap' n [x] ys = n + x*(head ys)
countswap' n xs [y] =  countswap' (n + y* (head xs)) (tail xs) [y]
countswap' n (x:xs) (y:ys) = countswap' (n+x*y) xs ys

countneg :: [Int] -> [Int] -> Int                
countneg xs ys = countneg' 0 0 xs ys

countneg' _ acc [] _ = acc
countneg' _ acc _ [] = acc
countneg' n acc (x:xs) (y:ys) = countneg' n' acc' xs ys
  where n' = (n+1) `mod` 2
        acc' = acc + n*x*y
                
resbld :: [Int] -> [Int] -> [Int]
resbld [] [] = []
resbld xs [] = xs
resbld [] ys = ys
resbld (x:xs) (y:ys) = ((x+y) `mod` 2) : (resbld xs ys)
        

--Utility Functions
        
grade :: (Num a, Integral n) => Blade a n -> Int
grade (Blade (_, n)) = sum (int2bin n)
        
bldcoeff :: (Num a, Integral n) => Blade a n -> a 
bldcoeff (Blade (a,_)) = a

-- Only use this when the basis blades are the same
bldadd :: (Num a, Integral n) => Blade a n -> Blade a n -> Blade a n 
bldadd (Blade (x,n)) (Blade (y,_)) = Blade (x+y,n)

bdlssimp :: (Num a, Integral n)  =>  [(Blade a n)] -> [(Blade a n)]
bdlssimp [] = []
bdlssimp [x] = [x]
bdlssimp (x:y:xs)   
  | x == y = bdlssimp ((bldadd x y):xs)
  | otherwise = x:bdlssimp (y:xs)      

removeZeros :: (Eq a, Num a, Integral n) => [(Blade a n)] -> [(Blade a n)]
removeZeros xs = filter bldnotzero xs
  where bldnotzero (Blade (x,_)) = if x==(negate x) then False else True
  
bldReverse :: (Num a, Integral n)  =>  Blade a n -> Blade a n
bldReverse (Blade (x,m)) = if mod (a*(a-1)) 4 == 0 then (Blade (x,m)) else (Blade (negate x,m))
  where a = sum (int2bin m)

mvReverse :: (Num a, Integral n)  =>  Multivector a n -> Multivector a n
mvReverse (Mv xs) = Mv $ map bldReverse xs