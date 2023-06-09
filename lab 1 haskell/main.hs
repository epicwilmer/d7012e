module Main where

--returns list--
getList:: (Int, Int, [Int]) -> [Int] 
getList (sum, i, list) = list

--sum of a provided list--
getListSum :: (Int, Int, [Int]) -> Int 
getListSum (sum, i, list) = sum

--first index of list--
startIndex :: (Int, Int, [Int]) -> Int
startIndex (sum, i, list) = i

--last index of list--
endIndex :: (Int, Int, [Int]) -> Int
endIndex (sum, i, list) = i + length list -1

--Sums all elements from [i,...j], sums and repeates this process for [i,...j-1] and so forth until j=i--
subSum :: [Int] -> Int -> [(Int, Int, [Int])]
subSum [] _ = []
subSum subL i = (sum subL, i, subL) : subSum (init subL) i

--used to create all sublists of [i,...j], and then [i+1,...j] until i=j, where j is the last element of the list--
subLists :: [Int] -> Int -> [(Int, Int, [Int])]
subLists [] _ = []
subLists subL i = subSum subL i ++ subLists (tail subL) (i+1)

--Returns lists sorted by smallest to biggest sum of all elements inside each list.--
sortSub :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
sortSub [] = []
sortSub (x:xs) =    let small = sortSub[i | i <- xs, getListSum i <= getListSum x]
                        big = sortSub [i | i <- xs, getListSum i > getListSum x]
                    in  small ++ [x] ++ big

-- takes a list and returns the k first sublists, since they are ordered from smallest to biggest sumwise, the smallest will be selected before the bigger ones--
smallestKsets :: [Int] -> Int -> [(Int, Int, [Int])]
smallestKsets xs k =    let sublist = subLists xs 1
                            sortedSubs = sortSub sublist
                        in take k sortedSubs

-- turns a list into a string--
listToString :: [Int] -> String
listToString [] = "\n"
listToString (x:xs) = show x ++ " " ++ listToString xs

-- format for the string printout --
stringFormat :: [(Int, Int, [Int])] -> String
stringFormat [] = ""
stringFormat (x:xs) = show (getListSum x) ++ " " ++ show(startIndex x) ++ " " ++ show(endIndex x) ++ " " ++ listToString (getList x) ++ stringFormat xs


-- header for clearer output of the program --
stringHeader :: String
stringHeader = "size i j sublist \n"

-- uses the tests provided in the lab instructions--
test1 :: [Int]
test1 = [x * (-1) ^x | x <- [1..100]]

test2 :: [Int]
test2 = [24,-11,-34,42,-24,7,-19,21]

test3 :: [Int]
test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]


--outputs the results--
main = do
    putStr(stringHeader ++ stringFormat (smallestKsets test1 15))
    putStr "\n \n"
    putStr(stringHeader ++ stringFormat (smallestKsets test2 6))
    putStr "\n \n"
    putStr(stringHeader ++ stringFormat (smallestKsets test3 8))
    putStr "\n \n"
    putStr(stringHeader ++ stringFormat (subLists test2 1))