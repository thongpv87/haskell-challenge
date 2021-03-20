foldL :: (a->b->a) -> a -> [b] -> a
foldL f a (x:xs) = foldL f (f a x) xs
foldL _ a [] = a

foldR :: (b->a->a) -> a -> [b] -> a
foldR f a (x:xs) = f x (foldR f a xs)
foldR _ a [] = a
