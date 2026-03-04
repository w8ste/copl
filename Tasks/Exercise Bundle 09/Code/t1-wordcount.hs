module Assignment where

import Data.List

-- **** Task 1.1 ****
-- Implement a function "countWords" counting the number of words in a text (a string).
--
-- (Hint: use function "words" to split a string into words)

countWords :: String -> Int
countWords = undefined

-- **** Task 1.2 ****
-- Implement a function "differentWords" that takes a text and returns a list of different words
-- in the text in lexicographic order.

differentWords :: String -> [String]
differentWords = undefined

-- **** Task 1.3 ****
-- Implement a function "wordFrequencies" taking a text and building a table of word frequencies
-- in that text. The table should contain a list of pairs, consisting of a word
-- and the number of the occurrences of that word in the text.
--
-- The table should be sorted alphabetically.

wordFrequencies :: String -> [(String, Int)]
wordFrequencies = undefined

-- **** Task 1.4 ****
-- Implement a function "normFrequencies" computing an normalized frequencies of words in a text.
--
-- A normalized frequency of a word is computed by dividing the frequency of
-- that word in the text by the total number of words in the text.
--
-- (Hint: Use the function "intToFloat" to convert from Int to Float.)

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

normFrequencies :: String -> [(String, Float)]
normFrequencies txt = undefined


-- **** Tests ****
-- run the following tests:

test1a = (countWords "aa cc bb dd aa dd cc cc" == 8)
test2a = (differentWords "aa cc bb dd aa dd cc cc" == ["aa", "bb", "cc", "dd"])
test3a = (wordFrequencies "aa cc bb dd aa dd cc cc" ==
          [("aa", 2), ("bb", 1), ("cc", 3), ("dd", 2)])
test4a = (normFrequencies "aa cc bb dd aa dd cc cc" ==
          [("aa", 0.25), ("bb", 0.125), ("cc", 0.375), ("dd", 0.25)])

main = do
    print test1a
    print test2a
    print test3a
    print test4a










-- Note, below are extra tasks that were considered “too much work” at some point.
-- These are included if you feel like you need more excercises :-)



-- **** Task 1.5 ****

-- Implement a function that merges a list of sorted lists into a consolidated
-- sorted list. The elements should be compared using the the function given as
-- a parameter.

mergeLists :: (a -> a -> Bool) -> [[a]] -> [a]
mergeLists comp ll = undefined

test5a = (mergeLists (<)
              [["aa", "bb", "cc"], ["ab", "bb", "ca"], ["aa", "ab", "ac", "dd"]]
            == ["aa", "aa", "ab", "ab", "ac", "bb", "bb", "ca", "cc", "dd"])
test5b = (mergeLists (\(k1, v1) (k2, v2) -> k1 < k2)
             [[(2, 20), (5, 50), (7, 70)], [(2, 25), (3, 35)]]
           == [(2, 20), (2, 25), (3, 35), (5, 50), (7, 70)])

-- **** Task 1.6 ****

-- Implement a function that takes a list of texts (strings) and builds a search
-- index of words in these texts.
--
-- The index should be represented by a table from words to lists of texts
-- containing the word. The table should be sorted alphabetically. The texts
-- associated to a word in the table must be sorted by the normalized frequency
-- of the word in the text (in decreasing order).

buildIndex :: [String] -> [(String, [String])]
buildIndex texts = undefined

txt1 = "aa aa aa dd aa bb"
txt2 = "aa aa aa dd dd bb bb"
txt3 = "aa aa bb dd dd dd bb bb"
txt4 = "aa bb bb dd dd bb bb"
txt5 = "bb bb bb dd bb bb"

testIdx = [("aa", [txt1, txt2, txt3, txt4]),
           ("bb", [txt5, txt4, txt3, txt2, txt1]),
           ("dd", [txt3, txt2, txt4, txt1, txt5])]

test6a = (buildIndex [txt1, txt2, txt3, txt4, txt5] == testIdx)

-- **** Task 1.7 ****

-- Implement a function that takes an index build by function buildIndex, number
-- n and a word, and returns the n documents in which the word appears with the
-- highest frequency.
--
-- If there are less than n documents in which the word appears, then the
-- function should return all these documents.
--
-- The returned list should be sorted by the frequency of the word in the
-- documents (in decreasing order).

searchWord :: [(String, [String])] -> Int -> String -> [String]
searchWord idx n w = undefined

test7a = (searchWord testIdx 2 "bb" == [txt5, txt4])
test7b = (searchWord testIdx 7 "aa" == [txt1, txt2, txt3, txt4])
test7c = (searchWord testIdx 2 "cc" == [])
