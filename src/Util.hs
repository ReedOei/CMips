module Util where

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

