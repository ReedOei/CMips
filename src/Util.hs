module Util where

import Control.Applicative

import Data.Maybe

class PrettyPrint a where
    prettyPrint :: a -> String

    prettyPrintLong :: a -> String
    prettyPrintLong = prettyPrint

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

untilM :: Monad m => (m a -> m a -> m Bool) -> (a -> m a) -> m a -> m a
untilM pred f cur = do
    let res = f =<< cur
    b <- pred cur res
    if b then
        res
    else
        untilM pred f res

noChange :: (Monad m, Eq a) => m [a] -> m [a] -> m Bool
noChange compM newM = do
    comp <- compM
    new <- newM
    pure $ comp == new

-- | If the function applied to the first value is true, bind to the first function, otherwise, bind to the second
condM :: Monad m => (a -> Bool) -> m a -> (a -> m b) -> (a -> m b) -> m b
condM f v thenF elseF = do
    b <- f <$> v
    if b then thenF =<< v else elseF =<< v

prependA :: Applicative f => a -> f [a] -> f [a]
prependA x xs = (:) <$> pure x <*> xs

-- | Match each function in the list with at most one element in the input list.
-- If all functions match with at least one distinct element (elements cannot be matched twice),
-- then will return Just matches, otherwise, returns Nothing
matchSet :: Eq b => [a -> Maybe b] -> [a] -> Maybe [b]
matchSet fs xs = selectNoOverlap $ map (($ xs) . mapMaybe) fs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Select one element from each list so that no value is selected twice, if possible.
-- Otherwise, returns Nothing.
selectNoOverlap :: Eq a => [[a]] -> Maybe [a]
selectNoOverlap = selectNoOverlap' []
    where
        selectNoOverlap' :: Eq a => [a] -> [[a]] -> Maybe [a]
        selectNoOverlap' _ [] = Just []
        selectNoOverlap' ignore (xs:xss) =
            safeHead [x : selection | x <- xs, x `notElem` ignore,
                                      let selected = selectNoOverlap' (x : ignore) xss,
                                      isJust selected, let selection = fromJust selected]

