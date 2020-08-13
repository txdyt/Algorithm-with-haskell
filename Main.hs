module Algorithm where

type Nat = Int

label :: [a] -> [(Nat, a)]
label = zip [0..]

perms1 :: [a] -> [[a]]
perms1 [] = [[]]
perms1 (x:xs)  = [zs | ys <- perms1  xs, zs <- inserts x ys]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys):map (y:) (inserts x ys)

perms1' :: [a] -> [[a]]
perms1' = foldr step [[]]
  where
    step x = concatMap (inserts x)

perms1'' :: [a] -> [[a]]
perms1'' = foldr (concatMap . inserts) [[]]

perms2 [] = [[]]
perms2 xs = [x:zs | (x, ys) <- picks xs, zs <- perms2 ys]

picks :: [a] -> [(a, [a])]
picks [] = []
picks (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- picks xs]

perms2' [] = [[]]
perms2' xs = concatMap subperms (picks xs)
    where
        subperms (x, ys) = map (x:) (perms2' ys)

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p x then x else until p f (f x)

while :: (a -> Bool) -> (a -> a) -> a -> a
while p = until (not . p)

-- map f . map g = map (f . g)
-- concatMap f . map g = concatMap (f . g)
-- foldr f e . map g = foldr (f .  g) e 
--
-- foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
--
-- concat = foldr (++) []
-- h (foldr (++) [] xss) = foldr g (h []) xss   provided h (xs ++ ys) = g xs (h ys)
-- h = foldr f e => take g = flip (foldr f)
-- =>
-- foldr f e . concat = foldr (flip (foldr f)) e
--
-- h (foldr f e xs) = foldr g (h e) xs          provided h (f x y) = g x (h y)

wrap :: a -> [a]
wrap x = [x]

unwrap :: [a] -> a
unwrap [x] = x

single :: [a] -> Bool
single [x] = True
signle _ = False

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []
