import Data.List
import Data.Array

{--  Maximum Value Contiguous Subsequence
Given an array of n integers find the maximum sum in any contiguous subsequence of the input.
For example, [31,-41,59,26,-53,58,97,-93,-23,84]. The max sum is 59+26-53+58+97=187

M[i] = max (elem i) (M[i-1] + elem i)
M[0] = elem 0
--}

maxsum :: Array Int Int -> Int
maxsum arr = maximum $ elems table
  where table = array (0, h-l) [(i,f i) | i <- [0..(h-l)]] 
        (l,h) = bounds arr
        f i = if i == 0 then 0
               else max (arr ! (i-1+l)) (table ! (i-1) + arr ! (i-1+l)) 

testarr = [31,-41,59,26,-53,58,97,-93,-23,84] :: [Int]
a = listArray (0, length testarr-1) testarr
        

{-- Making Change. 
You are given n types of coin denominations of values v(1) < v(2) < ... < v(n)
(all integers). Assume v(1) = 1, so you can always make change for any amount of money C. 
Give an algorithm which makes change for an amount of money C with as few coins as possible.

Mj - minimum number of coins required to make change for amount of money j
Mj = min {M(j-vi} + 1 for all i < j 
--}

change' :: [Int] -> Int -> Int
change' coins amount = table ! amount
  where table = array (0,amount) [ (i, f i) | i <- [0..amount]]
        f i = if i == 0 then 0
               else minimum [ table ! (i-k) + 1 | k <- coins, k <= i ]

change :: [Int] -> Int -> [Int]
change coins amount = values table amount 
  where table = array (0,amount) [ (i, f i) | i <- [0..amount]]
        f i = if i == 0 then (0,0)
               else minimum [ (fst (table ! (i-k)) + 1,k) | k <- coins, k <= i ]
        values t 0 = []
        values t n = let v = snd (t ! n) in v:values t (n-v)


{-- Longest Increasing Subsequence. 
Given a sequence of n real numbers A(1) ... A(n), determine a subsequence (not necessarily contiguous) 
of maximum length in which the values in the subsequence form a strictly increasing sequence.
[5,2,8,6,3,6,9,7]  -> 2,3,6,9

Mj = 1 + max (Mi) forall i < j and elem [j] > elem [i] 
--}

lsub' :: Array Int Int -> Int
lsub' arr = maximum $ elems table
  where table = array (0,h-l) [(i, f i) | i <- [0..h-l]]
        f i = if i == 0 then 1
               else let ts = [table ! k | k <- [0..i-1], arr ! i > arr ! k]
                    in if null ts then 1 else 1 + maximum ts
        (l,h) = bounds arr

lsub :: Array Int Int -> [(Int,Int)]
lsub arr = elems table
  where table = array (0,h-l) [(i, f i) | i <- [0..h-l]]
        f i = if i == 0 then (1,-1)
               else let ts = [(fst (table ! k), k) | k <- [0..i-1], arr ! i > arr ! k]
                    in if null ts then (1,-1) else let (l,p) = maximum ts in (1 + l, p)
        (l,h) = bounds arr


t0 = [9,5,2,8,7,3,1,6,4] :: [Int]
a0 = listArray (0,length t0 - 1) t0

t1 = [5,1,2,8,6,3,4,6,7,9,7,10] :: [Int]
a1 = listArray (0,length t1 - 1) t1

{-- Knapsack problem (duplicate allows and duplicate forbidden) 
Given items of different values and volumes, find the most valuable set of items that fit in a 
knapsack of fixed volume.
items = [(volume,value)] and capacity size C
items = [(5,3), (2,6), (4,7)] C = 6 => optimal C = 2+4=6, W = 6+7=13
Mj - optimal value for capacity j size
M0 = 0
Mj = {max (M(j - volume(i)) + value(i))}, where volume(i) <= j
--}

knapsack' :: [(Int,Int)] -> Int -> Int
knapsack' xs size = table ! size
  where table = array (0, size) [(i, f i) |  i <- [0..size]]
        f i = if i == 0 then 0
               else maximum $ [table ! (i - vol) + val | (vol,val) <- xs, vol <= i] ++ [0]

knapsack :: [(Int,Int)] -> Int -> (Int, [Int])
knapsack xs size = table ! size
  where table = array (0, size) [(i, f i) |  i <- [0..size]]
        f i = if i == 0 then (0,[])
               else maximum $ [let (v,c) = table ! (i - vol) in (v + val, c ++ [vol]) 
                                | (vol,val) <- xs, vol <= i] ++ [(0,[])]
        
it = [(6,30), (3,14), (4,16), (2,9)] :: [(Int,Int)]


{--
duplicates forbidden
M(i,j) - optimal value for filling exactly a capacity j knapsack with some subset of item 1..i,
  where i <- 1..n, j <- 1..C
M(i,j) = max {M(i-1,j), M(i-1,j-volume(i)) + value (i)}
  where M(i-1,j) - i-th item not used, 
        M(i-1,j-volume(i)) + value (i) - i-th item used 
--}


knapsack2 :: Array Int (Int,Int) -> Int -> Int
knapsack2 items capacity = table ! (n,capacity)
  where (l,h) = bounds items
        n = h-l+1
        table = array ((0,0),(n,capacity)) [((i,j), f i j) | i <- [0..n], j <- [0..capacity]]
        f i j = if i == 0 || j == 0 then 0
                 else if vol i > j then table ! (i-1,j)
                       else max (table ! (i-1,j)) (table ! (i-1, j - vol i) + val i)
        vol i = fst $ items ! (i-1+l)
        val i = snd $ items ! (i-1+l) 
-- it, cap = 10, answer 46   
ita = listArray (0,length it - 1) it   

{-- Edit Distance. 
Given two text strings A of length n and B of length m, you want to transform A into B with a 
minimum number of operations of the following types: delete a character from A, insert a character 
into A, or change some character in A into a new character. The minimal number of such operations 
required to transform A into B is called the edit distance between A and B.

Input: strings A[1..n], B[1..m]
Costs: Ci - insert, Cd - delete, Cu - update
Goal: Compute minimum cost of transforming A->B
T(i,j): minimum cost to transform A[1..i] into B[1..j]
recurrence:
 T(i,j) = min {Cd + T(i-1,j), T(i,j-1) + Ci, if Ai == Bj then T(i-1,j-1) else T(i-1,j-1) + Cr}
 T(0,j) = j
 T(i,0) = i
 Costs = 1
--}  
distance :: String -> String -> Int
distance xs ys = table ! (l1, l2)
  where (l1,l2) = (length xs, length ys)
        table = array ((0,0), (l1,l2)) [((i,j), f i j) | i <- [0..l1], j <- [0..l2]]
        f i j = if i == 0 then j 
                 else if j == 0 then i
                       else minimum [1 + table ! (i-1,j), table ! (i, j-1),
                                     if xs !! (i-1) == ys !! (j-1) then 0
                                      else table ! (i-1, j-1) + 1] 

{-- The Linear Partition Problem 
A given arrangement S of nonnegative numbers {s1,...,sn} and an integer k. 
Partition S into k ranges, so as to minimize the maximum sum over all the ranges.

M(j,k) minimum partition cost of all numbers into k ranges, where the cost
of partition is the largest sum of elements in one of its parts.
M(j,k) = min (i=1..j) max (M(i,k-1), sum all elems with indexes i+1..j)
M(1,k)=s1
M(j,1)= sum elems with indexes i=1..j 
--}

lp' :: [Int] -> Int -> Int
lp' xs k = table ! (l,k)
  where l = length xs
        table = array ((1,1), (l,k)) [((i,j), f i j) | i <- [1..l], j <- [1..k]]
        f i j = if i == 1 then head xs
                 else if j == 1 then pref ! i
                       else minimum [ max (table ! (i',j-1)) (pref ! i - pref ! i') | i' <- [1..i] ]
        pref = listArray (1,l) $ scanl1 (+) xs


