{-# LANGUAGE FlexibleContexts #-}

module PatTypeTest where

data T a b = T1 a a
           | T2 a b

f :: [a] -> ()
f [] = ()
f (x:xs) = ()

-- doesn't preserve the context
g :: (Eq b, Num b) => Maybe b -> ()
g Nothing = ()
g (Just 5) = ()

-- doesn't preserve the Num a context
h (3,4) Nothing (y:ys) [1,2,3] = ()

j (T1 "asd" "qwe") (T2 (Just 4) ('c', True)) = ()

k x = case x of
        (Just xs) | null xs -> ()
        _ -> ()

l x
  | 4 <- x = ()
  | otherwise = ()

-- this is perfectly legal
m 5 (x:xs) = ()
m [] 3 = ()

-- can't get type of patterns in let bindings
-- z1 xs = let y = length xs in y

-- can't get type of "longer lists"
-- z2 (x:y:z:[])
