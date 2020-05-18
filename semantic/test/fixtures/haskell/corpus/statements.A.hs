f = if a then 1 else 2
f = if a; then b; else c
f = if (if a then b else c) then d else e
f = if if a then b else c then d else e

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
