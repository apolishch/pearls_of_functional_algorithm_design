module Lib
    ( someFunc
    ) where

import Data.List
import Data.Array

readList :: IO [Int]
readList = do
  putStrLn "Please enter the list as a string"
  putStrLn "Example: input of '1 2 3 4 5' will map to [1,2,3,4,5]"
  line <- getLine
  return $ map read $ words line

readBound :: IO Int
readBound = do
  putStrLn "Please enter a bound"
  bound <- getLine
  return (read bound :: Int)

-- Take the first element of an array that contains none of the elements in xs
-- This works because Haskell is lazy. In effect, the first list is only expanded to the--- point such that every element in xs has been removed, the rest of the list is (lazily)-- not expanded.
minFreeArrayNaive :: [Int] -> Int
minFreeArrayNaive xs = head([0..] \\ xs)

printMinFreeArrayNaive :: [Int] -> IO ()
printMinFreeArrayNaive xs = putStrLn "The minimum free number is:" >> putStrLn (show $ minFreeArrayNaive xs)

-- Take the elements of an array as a list. Since all elements are booleans id returns
-- the Boolean itself (id a -> a). takeWhile takes elements from the front until an
  -- element is false. Since function composition (.) is right associative, parentheses are-- placed to the right so (length takeWhile) (id elems).
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- the type of accumArray is :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Arrayi e
-- in effect this function takes an accumulator function (much like fold/reduce/inject) that takes an element (think the fold memo value) of the type which the array values will contain, an element of the same type as the second elements of the tuples in the starting list and somehow converts the latter to the former. In our case they are the same type
-- It then takes a starting memo, a range of indices, and a list of tuples of type (index, starging type)
-- It returns an array, indexed by index type, of the memo type
-- In our specific case, we accumulate using logical or (so from Bool to Bool), and inject False to start with. Our index range is 0.. length of the starting list
-- Our starting list is a zip between the integer range [0..length] and an infinite list of Trues. again we rely on Lazy evaluation.
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
               where n = length xs


prosaicChecklist = putStrLn "prosaic goes here"                     
                     
-- similiar to checklist but generates an indexed list of occurence counts.
-- the zip zips a list of array members as indices with a list of 1s.
-- the fact that this is an associative array ensures that repetition is just reapplied addition, thus generating a count of occurences.
-- in effect the way the array is stored by index is the sort.
-- the reason this does not violate the well known O(nln(n)) result, is that the sort is not, in fact, in place
-- Additionally this is only really useful if we have some kind of upper bound known on the dataset
-- TODO: More generic function that takes bounds
countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n)(zip xs (repeat 1))
               where n = length xs

-- the replicate concat does what it says on the tin. Assocs reads a list of tuples, and the replicate reconstructs the right number of items based on their count. The ones that were absent the original array are "replicated" 0 times
sortLinearTime :: [Int] -> [Int]
sortLinearTime xs = concat [replicate k x | (x, k) <- assocs $ countlist xs] 

printSortedArray :: [Int] -> IO ()
printSortedArray xs = putStrLn "The sorted array is:" >> putStrLn (show $ sortLinearTime xs)

printMinFreeBetterArray :: [Int] -> IO ()
printMinFreeBetterArray xs = putStrLn "foo"

countListGeneric :: [Int] -> Int -> Int -> Array Int Int
countListGeneric xs x y | (x < y) = accumArray (+) 0 (x, y)(zip xs (repeat 1))
                        | otherwise = accumArray (+) 0 (y, x)(zip xs (repeat 1))

sortGeneric :: [Int] -> Int -> Int -> [Int]
sortGeneric xs x y = concat [replicate k x | (x, k) <- assocs $ countListGeneric xs x y]

printGenericallySortedArray :: [Int] -> IO ()
printGenericallySortedArray xs = let sortGenericList = sortGeneric xs
                                 in readBound >>= \bound1 -> let sortGenericBounded = sortGenericList bound1
                                                                 in readBound >>= \bound2 -> putStrLn "The sorted array is:" >> putStrLn (show $ sortGenericBounded bound2)
printMinFreeProsaicArray :: [Int] -> IO()
printMinFreeProsaicArray xs = putStrLn "TODO"

someFunc :: IO ()
someFunc = putStrLn "Please select an implementation. Please enter an integer:" >>
  putStrLn "1. Naive array based implementation" >>
  putStrLn "2. Better array based implementation" >>
  putStrLn "3. An unrelated aside: Linear time sorting" >>
  putStrLn "4. An even more unrelated aside: General sorting on bounded input" >>
  putStrLn "5. Back on track. An array based implementation with a 'prosaic' checklist">>
    getLine >>= \implementation ->
      let li = length implementation
      in case(li) of
        0 -> putStrLn "You did not select an implementation"
        _ -> let impl = read implementation :: Int
             in case(impl) of
               1 -> putStrLn "You selected the naive array implementation" >>
                 Lib.readList >>= printMinFreeArrayNaive
               2 -> putStrLn "You selected the array based implementation" >>
                 Lib.readList >>= printMinFreeBetterArray
               3 -> putStrLn "You wish to sort an array" >>
                 Lib.readList >>= printSortedArray
               4 -> putStrLn "You wish to more generally sort an array" >>
                 Lib.readList >>= printGenericallySortedArray
               5 -> putStrLn "You selected the array based implementation with prosaic checklist">>
                 Lib.readList >>= printMinFreeProsaicArray
               _ -> putStrLn "This implementation not yet implemented"
