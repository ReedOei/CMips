module Util
    (
        findSplit,
        maybePred
    ) where

findSplit :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findSplit f = findSplit' []
    where
        findSplit' _ [] = Nothing
        findSplit' before (x:xs)
            | f x = Just (before, x, xs)
            | otherwise = findSplit' (before ++ [x]) xs

maybePred :: (a -> Bool) -> Maybe a -> Bool
maybePred _ Nothing = False
maybePred f (Just x) = f x

