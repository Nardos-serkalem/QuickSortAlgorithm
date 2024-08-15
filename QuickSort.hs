import Data.Array

quickSortArray :: forall a. Ord a => Array Int a -> Array Int a
quickSortArray arr = array (l, u) [(i, sorted !! (i - l)) | i <- [l..u]]
  where
    (l, u) = bounds arr
    sorted = quickSort' (elems arr) --i used to elems to get the list of numbers from the input array
   

    quickSort' [] = []
    quickSort' [x] = [x]  --this line shows the Base case for single element array
    quickSort' xs =
        let pivot = median xs  -- 
            lesser = [y | y <- xs, y < pivot]
            greater = [y | y <- xs, y > pivot]
            equal = [y | y <- xs, y == pivot]    
        in quickSort' lesser ++ equal ++ quickSort' greater

    median ys = case take 3 ys of
        [a, b, c] -> if a > b then if a < c then a else max b c else if b < c then b else max a c
        [a, b] -> if a > b then a else b
        [a] -> a
        [] -> error "Empty list in median function"


main :: IO ()
main = do
    let unsortedArray = array (0, 9) (zip [0..9] [7, 2, 5, 9, 1, 6, 3, 8, 4, 0])
    let sortedArray = quickSortArray unsortedArray
    print sortedArray  
