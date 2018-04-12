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

