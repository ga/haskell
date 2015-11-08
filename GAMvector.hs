import GABlade
import Data.List
import Data.Char

--Datatypes and constructors
{- 
One single ADT with two constructors, depending on size of space.
-}

newtype Multivector n a = Mv [(n,a)] 

mv :: (Num a, Eq a) => [(a,String)] -> Multivector Int a  
mv xs = Mv (bladeListSimp (sortBy bladeComp (map blade xs)))

longMv :: (Num a, Eq a) => [(a,String)] -> Multivector Integer a  
longMv xs = Mv (bladeListSimp (sortBy bladeComp (map blade xs)))



--Input functions
{-
Basis blades are input as strings "E1", "F1F2". 
Need a function to convert these to either an Int or an Integer 
Rely on choice of mv or longMv to cast the result
-}

--Converts string "E1E2" into [(E,1), (E,2)] etc.
bldRead :: String -> [(Char,Int)] 
bldRead [] = []
bldRead st = (ef,n): (bldRead st')
  where ef = head st
        (n',st') = break (not.isDigit) (tail st)
        n = read n' :: Int
        
--Takes pair (E,1) and converts to the corresponding int/integer and a 1.
basisVec2Bld :: (Integral n) => (Char,Int) -> (n,Int)
basisVec2Bld (ch,n) 
  | ch == 'E' = (2^(2*(n-1)),1)
  | ch == 'F' = (2^(2*n-1),1)
  | otherwise = error("invalid blade definition") 
                
                
--Takes the full string and converts to the equivalent blade (Int/Integer n, +-1).                
str2Bld :: (Integral n) => String -> (n,Int)
str2Bld str = foldl' bladeProd (0,1) lst
  where lst = map basisVec2Bld $ bldRead str

--Final funtion used in constructor
blade :: (Num a, Integral n) => (a,String) -> (n,a)
blade (a,str) = (n,a')
  where (n,tst) = str2Bld str
        a' = if tst==1 then a else negate a
  
--Output
instance (Num a, Show a, Show n, Integral n) => Show (Multivector n a) where
  show (Mv []) = "0"       
  show (Mv [x]) = bladeShow x
  show (Mv (x:xs)) = bladeShow x++"+"++(show (Mv xs))  
        
--Simplifying multivectors
{- 
Going to assume throughout that all multivectors are ordered. ADT should ensure this.
-}

--Comparison by grade then number. Should be better for projecting and filtering
bladeComp :: (Integral n) => (n,a) -> (n,a) -> Ordering
bladeComp (n,_) (m,_)
  | bladeGrade n < bladeGrade m = LT
  | bladeGrade m < bladeGrade n = GT                                 
  | otherwise = compare n m
    
bladeListSimp :: (Integral n, Num a, Eq a) => [(n,a)] -> [(n,a)]    
bladeListSimp = removeZeros.bladeListSimp'

bladeListSimp' :: (Integral n, Num a) => [(n,a)] -> [(n,a)]    
bladeListSimp' [] = []
bladeListSimp' [x] = [x]
bladeListSimp' (x:y:xs) = 
  if bladeComp x y == EQ then bladeListSimp' ((bldAdd x y):xs) 
  else x:(bladeListSimp' (y:xs))
 
bldAdd :: (Integral n, Num a) => (n,a) -> (n,a) -> (n,a)    
bldAdd (n,a) (_,b) = (n,a+b)

removeZeros :: (Eq a, Num a, Integral n) => [(n, a)] -> [(n, a)]
removeZeros xs = filter bldnotzero xs
  where bldnotzero (_,x) = if x==(negate x) then False else True

--Blade List sums and products
bldListMerge :: (Integral n, Num a) => [(n,a)] -> [(n,a)] -> [(n,a)]    
bldListMerge [] ys = ys
bldListMerge xs [] = xs
bldListMerge lf@(x:xs) rt@(y:ys) 
  | bladeComp x y == LT = x:bldListMerge xs rt
  | otherwise = y:bldListMerge lf ys                      

bladeListAdd :: (Integral n, Num a, Eq a) => [(n,a)] -> [(n,a)] -> [(n,a)]    
bladeListAdd xs ys = bladeListSimp(bldListMerge xs ys)

bldListNegate :: (Integral n, Num a) => [(n,a)] -> [(n,a)] 
bldListNegate xs = map bldNegate xs
  where 
    bldNegate (n,a) = (n, negate a)
    
bladeListProduct :: (Integral n, Num a, Eq a) => [(n,a)] -> [(n,a)] -> [(n,a)] 
bladeListProduct xs ys = bladeListSimp (sortBy bladeComp res)
  where res = [bladeProd x y | x <- xs, y <- ys]
        
outerProd ::  (Integral n, Num a, Eq a) => Multivector n a -> Multivector n a -> Multivector n a
outerProd (Mv xs) (Mv ys) = Mv (bladeListSimp (sortBy bladeComp res))        
  where res = [bladeProd x y | x <- xs, y <- ys, bladeGrade' (bladeProd x y) == (bladeGrade' x) + (bladeGrade' y)]
        bladeGrade' (fst,_) = bladeGrade fst
        
--Multivector Ops
projGrade :: (Integral n, Num a) => Multivector n a -> Int -> Multivector n a
projGrade (Mv xs) m = Mv (filter isGrade xs)
  where isGrade (fst,_) = bladeGrade fst == m
        
scalarPart :: (Integral n, Num a) => Multivector n a -> a
scalarPart (Mv xs) = isScalar (head xs)
  where isScalar (fst,snd) = if fst == 0 then snd else 0

mvReverse :: (Integral n, Num a) => Multivector n a -> Multivector n a
mvReverse (Mv xs) = Mv (map bladeReverse xs)
  where bladeReverse (n,x) 
          | mod (bladeGrade n) 4 == 0 = (n,x) 
          | mod (bladeGrade n) 4 == 1 = (n,x) 
          | otherwise = (n,negate x)

--The bar operator flips the sign of odd grade elements
mvBar :: (Integral n, Num a) => Multivector n a -> Multivector n a       
mvBar (Mv xs) = Mv (map bldBar xs)
  where bldBar (n,x) 
          | even (bladeGrade n)= (n,x) 
          | otherwise = (n,negate x)        

mvEven :: (Integral n, Num a) => Multivector n a -> Multivector n a       
mvEven (Mv xs) = Mv (filter bldEven xs)
  where bldEven(n,_) = even (bladeGrade n)

mvOdd :: (Integral n, Num a) => Multivector n a -> Multivector n a       
mvOdd (Mv xs) = Mv (filter bldOdd xs)
  where bldOdd(n,_) = odd (bladeGrade n)
        
mvFilter :: (Integral n, Num a) => (a -> Bool) -> Multivector n a -> Multivector n a       
mvFilter f (Mv xs) = Mv (filter mvFilter' xs)
  where mvFilter' (_,x) = f x



--Outermorphism
--Assume have a function f that takes grade 1 to grade 1
--Need to decompose a basis blade into a product of basis vectors
outermorph :: (Integral n, Num a, Eq a) => (Multivector n a -> Multivector n a) -> Multivector n a -> Multivector n a
outermorph f (Mv xs) = foldl1' (+) (map (blademorph f) xs)

blademorph :: (Integral n, Num a, Eq a) => (Multivector n a -> Multivector n a) -> (n,a) -> Multivector n a
blademorph f (n,a) = foldl' outerProd (Mv [(0,a)]) xs
  where 
    xs' = map toMv (bladeList (n,a)) 
    toMv (i,x) = Mv [(i,x)]
    xs = map f xs'

--Number Classes
instance (Integral n, Eq a) => Eq (Multivector n a) where
  (Mv xs) == (Mv ys) = xs == ys
  
instance (Integral n, Num a, Eq a) => Num (Multivector n a) where
  (Mv xs) * (Mv ys) = Mv (bladeListProduct xs ys)
  (Mv xs) + (Mv ys) = Mv (bladeListAdd xs ys)
  fromInteger n = Mv [(0,fromInteger n)]
  negate (Mv xs) = Mv (bldListNegate xs)
  abs (Mv xs) = Mv xs
  signum (Mv xs) = Mv xs
  
instance Functor (Multivector n) where  
  fmap f (Mv xs) = Mv (map (mapSnd f) xs)
  
mapSnd :: (a -> b) -> (c,a) -> (c,b)        
mapSnd f (x,y) = (x, f y)

