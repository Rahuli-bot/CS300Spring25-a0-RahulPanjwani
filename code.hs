module Code where
import Test.Hspec (describe, hspec, it, shouldBe)

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq)

data Direction = North | South | East | West deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- -----------------------------Category: Easy--------------------------------
-- ---------------------------------------------------------------------------

-- Question 1
myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose ([] : _) = []
myTranspose xss = map head xss : myTranspose (map tail xss)

matrixMultiplication :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiplication a b
  | null a || null b = []
  | length (head a) /= length b = []
  | otherwise =
      [ [ sum (zipWith (*) row col)
          | col <- myTranspose b ]
        | row <- a
      ]


-- Question 2
-- listStats :: LinkedList Int -> LinkedList Int
-- listStats = 

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq)

tolist :: LinkedList a -> [a]
tolist Null = []
tolist (ListNode x rest) = x : tolist rest

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    len = length xs
    (left, right) = splitAt (len `div` 2) xs


merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


median :: [Int] -> Int
median xs
    | null xs  = 0
    | even n   = (xs !! (n `div` 2 - 1) + xs !! (n `div` 2)) `div` 2
    | otherwise = xs !! (n `div` 2)
  where
    n = length xs


mean :: [Int] -> Int
mean xs = if null xs then 0 else sum xs `div` length xs

listStats :: LinkedList Int -> LinkedList Int
listStats lst = ListNode mmean (ListNode mem Null)
    where
        list = tolist lst
        sortedList = mergeSort list  -- Sort the list manually
        mmean = if null list then 0 else sum list `div` length list  -- Mean
        mem = median sortedList  -- Median




-- Question 3

triplets :: [a] -> [[a]]
triplets (a:b:c:xs) = [a, b, c] : triplets (b:c:xs)
triplets _ = []

concatSums :: Num a => [[a]] -> [a]
concatSums xs = [sum x | x <- xs]

indexOfMax :: (Ord a) => [a] -> Int
indexOfMax xs = indexOfMaxHelper xs 0 0 (head xs)
  where
    indexOfMaxHelper [] _ maxIndex _ = maxIndex
    indexOfMaxHelper (x:xs) currentIndex maxIndex maxValue
      | x > maxValue = indexOfMaxHelper xs (currentIndex + 1) currentIndex x
      | otherwise    = indexOfMaxHelper xs (currentIndex + 1) maxIndex maxValue


largestAdjacentSum :: [Int] -> (Int, Int, Int)
largestAdjacentSum xs = (maxTriplet !! 0, maxTriplet !! 1, maxTriplet !! 2)
  where
    t = triplets xs         
    sums = concatSums t     
    idx = indexOfMax sums   
    maxTriplet = t !! idx   


-- Question 4
helper :: Int -> [Int] -> [Int]
helper 1 l = 1 : l
helper x l
    | even x = helper (x `div` 2) (x : l) 
    | odd x && (x /= 1) = helper (x * 3 + 1) (x : l) 


collatzConjecture :: Int -> (Int, Int)
collatzConjecture x = (length(helper x []) - 1,maximum(helper x []))



-- Question 5
helper :: Int-> Int-> [Int]-> Int-> Int -> Int
helper index current xs end list
    | current == end+1 = list
    | current /= index  = helper index (current+1) xs end ((xs !! current) * list)
    | current == index = helper index (current+1) xs end list

productExceptSelf xs = [helper i 0 xs (length xs-1) 1 | i <- [0..length(xs)-1] ]




-- Question 6
-- Function to get the factors of a number
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Function to check if a number is prime
prime :: Int -> Bool
prime x = factors x == [1, x]

-- Helper function to recursively find the longest sequence of consecutive primes
helper :: [Int] -> Int -> Int -> Int
helper [] currentCount maxSeq = maxSeq
helper (x:xs) currentCount maxSeq
  | prime x && currentCount +1 > maxSeq = helper xs (currentCount + 1) (currentCount + 1)  -- Increase count if prime and update maxSeq
  | prime x = helper xs (currentCount + 1) maxSeq  -- Increase count if prime and update maxSeq
  | otherwise = helper xs 0 maxSeq  -- Reset count if not prime

-- Main function to find the longest prime sequence
longestPrimeSeq :: [Int] -> Int
longestPrimeSeq [] = 0
longestPrimeSeq xs = helper xs 0 0
-- ---------------------------------------------------------------------------
-- -----------------------------Category: Medium------------------------------
-- ---------------------------------------------------------------------------

-- Question 7
isLeaf :: Tree a -> Bool
isLeaf (TreeNode Nil _ Nil) = True
isLeaf _                    = False

prefixMatch :: String -> String -> Bool
prefixMatch _ []          = True       
prefixMatch [] _          = False
prefixMatch (x:xs) (y:ys) = (x == y) && prefixMatch xs ys

isSubstring :: String -> String -> Bool
isSubstring _ ""   = True   
isSubstring [] _   = False  
isSubstring s sub
  | prefixMatch s sub = True
  | otherwise         = isSubstring (tail s) sub

processChild :: String -> Tree String -> Tree String
processChild _ Nil = Nil
processChild parent t@(TreeNode _ x _) 
  | isLeaf t  = if isSubstring parent x then t else Nil
  | otherwise = leafDeletion t

leafDeletion :: Tree String -> Tree String
leafDeletion Nil = Nil
leafDeletion t@(TreeNode left v right)
  | isLeaf t  = Nil 
  | otherwise = TreeNode (processChild v left) v (processChild v right)





-- Question 8
textEditor :: String -> String
textEditor = undefined

-- Question 9
halkiOs :: String -> String
halkiOs = undefined

-- Question 10
palindromeSwaps :: [String] -> Int
palindromeSwaps = undefined

-- Question 11
maxStreak :: [Int] -> Int
maxStreak = undefined

-- ---------------------------------------------------------------------------
-- -----------------------------Category: Hard--------------------------------
-- ---------------------------------------------------------------------------

-- Question 12
treeDeduction :: (Eq a, Num a) => Tree a -> Tree a
treeDeduction = undefined

-- Question 13
poisonSpill :: [(Int, Int, Int)] -> Int
poisonSpill = undefined

-- Question 14
mazePathFinder :: [[Char]] -> [Direction]
mazePathFinder = undefined

-- Question 15
halloweenEscape :: [(String, String)] -> Int
halloweenEscape = undefined

-- Main Function
main :: IO ()
main =
  hspec $ do
    -- Test Matrix Multiplication
    describe "matrixMultiplication" $ do
      it "should return product of two matrices (lists)" $ do
        matrixMultiplication [[1, 2], [3, 4]] [[2, 0], [1, 2]] `shouldBe` [[4, 4], [10, 8]]
        matrixMultiplication [[1, 0], [0, 3]] [[4], [2]] `shouldBe` [[4], [6]]
        matrixMultiplication [[1, 2, 6], [0, 3, 4]] [[2, 0], [2, 7], [-5, 6]] `shouldBe` [[-24, 50], [-14, 45]]
        matrixMultiplication [[1, 2], [3, 4]] [[2, 0]] `shouldBe` []
        matrixMultiplication [[1, 7], [3, 1], [1, 5]] [[5, 6], [7, 8], [2, 5]] `shouldBe` []
        matrixMultiplication [[1, 7], [4, 3], [2, 5], [-3, 0]] [[5, -6, 6], [-7, 14, 8]] `shouldBe` [[-44, 92, 62], [-1, 18, 48], [-25, 58, 52], [-15, 18, -18]]
        matrixMultiplication [[1], [2], [3]] [[4]] `shouldBe` [[4], [8], [12]]
        matrixMultiplication [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]] [[1, 2, 3, 4, 5, 6, 7, 8], [9, 10, 11, 12, 13, 14, 15, 16]] `shouldBe` [[19, 22, 25, 28, 31, 34, 37, 40], [39, 46, 53, 60, 67, 74, 81, 88], [59, 70, 81, 92, 103, 114, 125, 136], [79, 94, 109, 124, 139, 154, 169, 184], [99, 118, 137, 156, 175, 194, 213, 232]]

    -- Test Stats of a LinkedList
    describe "listStats" $ do
      it "should return the mean and median of the linkedlist in linkedlist form" $ do
        listStats (ListNode 3 (ListNode 5 (ListNode 1 (ListNode 9 Null)))) `shouldBe` ListNode 4 (ListNode 4 Null)
        listStats (ListNode 5 (ListNode 1 (ListNode 9 Null))) `shouldBe` ListNode 5 (ListNode 5 Null)
        listStats (ListNode 3 Null) `shouldBe` ListNode 3 (ListNode 3 Null)

    -- Test Largest Adjacent Sum
    describe "largestAdjacentSum" $ do
      it "should return the three adjacent numbers with the largest sum" $ do
        largestAdjacentSum [2, 4, 1, 6, 4, 3, 7, 2] `shouldBe` (4, 3, 7)
        largestAdjacentSum [1, 2, 3, 4, 5, 6] `shouldBe` (4, 5, 6)
        largestAdjacentSum [1, -1, 2, 3, 5, -3] `shouldBe` (2, 3, 5)
        largestAdjacentSum [10, 1, 2, 3, 5, -3] `shouldBe` (10, 1, 2)
        largestAdjacentSum [-1, -2, -3, -4, -5, -6] `shouldBe` (-1, -2, -3)
        largestAdjacentSum [5, 1, 2, 10, 3, 4] `shouldBe` (10, 3, 4)
        largestAdjacentSum [5, 5, 5, 4, 5, 6] `shouldBe` (5, 5, 5)
        largestAdjacentSum [5, 5, 5, 4, 5, 6, 7, 7, 6] `shouldBe` (6, 7, 7)
        largestAdjacentSum [3, 3, 3, 3, 3, 3] `shouldBe` (3, 3, 3)

    -- Test Collatz Conjecture
    describe "collatzConjecture" $ do
      it "should return a tuple of step count and max value" $ do
        collatzConjecture 6 `shouldBe` (8, 16)
        collatzConjecture 12 `shouldBe` (9, 16)
        collatzConjecture 4 `shouldBe` (2, 4)
        collatzConjecture 5 `shouldBe` (5, 16)
        collatzConjecture 11 `shouldBe` (14, 52)
        collatzConjecture 17 `shouldBe` (12, 52)
        collatzConjecture 1 `shouldBe` (0, 1)

    -- Test Array Product Except Self
    describe "productExceptSelf" $ do
      it "should return the products list" $ do
        productExceptSelf [2, 3, 4, 5] `shouldBe` [60, 40, 30, 24]
        productExceptSelf [-1, 2, -3, 4] `shouldBe` [-24, 12, -8, 6]
        productExceptSelf [1, 2, 0, 4, 5] `shouldBe` [0, 0, 40, 0, 0]
        productExceptSelf [7, 3, 10, 2] `shouldBe` [60, 140, 42, 210]
        productExceptSelf [5, -2, 4, -3, 6] `shouldBe` [144, -360, 180, -240, 120]

    -- Test Longest Consecutive Sequence
    describe "longestPrimeSeq" $ do
      it "should return the count of longest consecutive seq of prime numbers" $ do
        longestPrimeSeq [2, 3, 5, 7, 6, 11, 4, 17, 6] `shouldBe` 4
        longestPrimeSeq [10, 15, 3, 7, 11, 6, 9, 11, 13] `shouldBe` 3
        longestPrimeSeq [4, 6, 8, 10] `shouldBe` 0
        longestPrimeSeq [2, 3, 5, 7, 11, 13, 17] `shouldBe` 7
        longestPrimeSeq [10, 2, 3, 4, 5, 6, 7, 8, 11] `shouldBe` 2
        longestPrimeSeq [8, 10, 15, 6, 11, 13, 17, 19, 23] `shouldBe` 5

    -- Test Leaf Deletion
    describe "leafDeletion" $ do
      it "should return the modified tree with the deleted non-substring leaves" $ do
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "act" Nil) "cat" (TreeNode Nil "ca" Nil)) "root" (TreeNode Nil "to" Nil)) `shouldBe` TreeNode (TreeNode Nil "cat" (TreeNode Nil "ca" Nil)) "root" Nil
        leafDeletion (TreeNode (TreeNode Nil "cats" Nil) "root" Nil) `shouldBe` TreeNode Nil "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "at" Nil) "cats" (TreeNode Nil "catts" Nil)) "root" (TreeNode Nil "oo" Nil)) `shouldBe` TreeNode (TreeNode (TreeNode Nil "at" Nil) "cats" Nil) "root" (TreeNode Nil "oo" Nil)
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "dog" Nil) "cats" (TreeNode Nil "elephant" Nil)) "root" (TreeNode Nil "apple" Nil)) `shouldBe` TreeNode (TreeNode Nil "cats" Nil) "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "dog" Nil) "Simba" (TreeNode Nil "Sim" Nil)) "root" (TreeNode Nil "apple" Nil)) `shouldBe` TreeNode (TreeNode Nil "Simba" (TreeNode Nil "Sim" Nil)) "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "abc" Nil) "abcdef" (TreeNode Nil "de" Nil)) "root" (TreeNode (TreeNode Nil "oof" Nil) "floof" (TreeNode Nil "floo" Nil))) `shouldBe` TreeNode (TreeNode (TreeNode Nil "abc" Nil) "abcdef" (TreeNode Nil "de" Nil)) "root" (TreeNode (TreeNode Nil "oof" Nil) "floof" (TreeNode Nil "floo" Nil))
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "ui" Nil) "fruits" (TreeNode Nil "banana" Nil)) "fruit basket" (TreeNode (TreeNode Nil "orange" Nil) "Tangerine" (TreeNode Nil "anger" Nil))) `shouldBe` TreeNode (TreeNode (TreeNode Nil "ui" Nil) "fruits" Nil) "fruit basket" (TreeNode Nil "Tangerine" (TreeNode Nil "anger" Nil))
        leafDeletion (TreeNode Nil "root" (TreeNode Nil "vegetable" (TreeNode Nil "potato" (TreeNode Nil "tomato" (TreeNode Nil "shakarkandi" (TreeNode Nil "shakar" Nil)))))) `shouldBe` TreeNode Nil "root" (TreeNode Nil "vegetable" (TreeNode Nil "potato" (TreeNode Nil "tomato" (TreeNode Nil "shakarkandi" (TreeNode Nil "shakar" Nil)))))

    -- Test Text Editor
    describe "textEditor" $ do
      it "should return the updated string" $ do
        textEditor "text#edi@tor" `shouldBe` "texedittor"
        textEditor "hello###wor@@ld" `shouldBe` "heworllld"
        textEditor "abcde#fg@h" `shouldBe` "abcdfgeh"

    -- Test Halki si OS
    describe "halkiOs" $ do
      it "should return the simplified path" $ do
        halkiOs "/users/" `shouldBe` "/users"
        halkiOs "/work//files/" `shouldBe` "/work/files"
        halkiOs "/documents///code/" `shouldBe` "/documents/code"
        halkiOs "/projects/reports/../images" `shouldBe` "/projects/images"
        halkiOs "/archive/files/./photos" `shouldBe` "/archive/files/photos"
        halkiOs "/home//user///downloads" `shouldBe` "/home/user/downloads"
        halkiOs "/root" `shouldBe` "/root"
        halkiOs "/" `shouldBe` "/"
        halkiOs "/videos/movies/../music" `shouldBe` "/videos/music"
        halkiOs "/abc/def/ghi" `shouldBe` "/abc/def/ghi"
        halkiOs "/abc/./def/.." `shouldBe` "/abc"
        halkiOs "/one/two/three/four/" `shouldBe` "/one/two/three/four"
        halkiOs "/one//two/.//three/" `shouldBe` "/one/two/three"
        halkiOs "/foo_bar/.." `shouldBe` "/"
        halkiOs "/hello123/./world456" `shouldBe` "/hello123/world456"

    -- Test Palindromic Paths with Minimum Swaps
    describe "palindromeSwaps" $ do
      it "should return the sum of swap counts to make each palindrome" $ do
        palindromeSwaps ["arcerac", "banana", "bbo"] `shouldBe` 2
        palindromeSwaps ["banana", "apple", "orange", "relrplepsee"] `shouldBe` 10
        palindromeSwaps ["aabbccd"] `shouldBe` 9

    -- Test Maximum Streak of Consecutive Numbers
    describe "maxStreak" $ do
      it "should return the length of the longest sequence of consecutive sequence" $ do
        maxStreak [100, 4, 200, 1, 3, 2] `shouldBe` 4
        maxStreak [9, 1, 3, 10, 2, 20] `shouldBe` 3
        maxStreak [10, 20, 30, 40] `shouldBe` 1
        maxStreak [5, 2, 99, 3, 1, 4, 100] `shouldBe` 5
        maxStreak [8, 7, 6, 5, 4, 3, 2, 1] `shouldBe` 8

    -- Test Tree Deduction
    describe "treeDeduction" $ do
      it "should return the tree after deductions" $ do
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 4 Nil) 2 (TreeNode Nil 5 Nil)) 1 (TreeNode (TreeNode Nil 6 Nil) 3 (TreeNode Nil 7 Nil))) `shouldBe` (TreeNode (TreeNode Nil 1 Nil) 5 (TreeNode Nil 1 Nil))
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 10 Nil) 5 Nil) 2 Nil) `shouldBe` (TreeNode (TreeNode Nil (-10) Nil) (-15) Nil)
        treeDeduction (TreeNode (TreeNode Nil 0 Nil) 0 Nil) `shouldBe` Nil
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 5 Nil)) 1 (TreeNode Nil 3 Nil)) `shouldBe` TreeNode Nil (-9) Nil

    -- Test Citywide Poison Spill
    describe "poisonSpill" $ do
      it "should return the max containers that can be affected" $ do
        poisonSpill [(0, 0, 3), (2, 0, 2), (4, 0, 2), (6, 0, 2), (8, 0, 1)] `shouldBe` 5
        poisonSpill [(2, 1, 3), (6, 1, 4)] `shouldBe` 2
        poisonSpill [(1, 1, 5), (10, 10, 5)] `shouldBe` 1

    -- Test Bilal Trapped in a Maze
    describe "mazePathFinder" $ do
      it "should return the shortest escape path" $ do
        mazePathFinder ["SOOOO", "OXXXO", "OOOXO", "XXOXO", "OGOOO"] `shouldBe` [South, South, East, East, South, South, West]
        mazePathFinder ["SOOO", "OOXO", "OXOX", "OXOX", "OOOG"] `shouldBe` [South, South, South, South, East, East, East]

    -- Test Halloween Escape
    describe "halloweenEscape" $ do
      it "should return the minimum number of moves to reach the end" $ do
        halloweenEscape [("House 1", "Closet"), ("House 1", "Basement"), ("House 2", "Closet"), ("House 3", "Attic"), ("House 3", "Bathroom"), ("House 2", "Basement"), ("House 2", "Attic")] `shouldBe` 3
        halloweenEscape [("House 2", "Dining Room"), ("House 1", "Attic"), ("House 1", "Kitchen"), ("House 1", "Dining Room")] `shouldBe` 1
