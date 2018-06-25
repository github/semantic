g = if b then 1 else 2
g = if b; then c; else d
g = if (if b then c else d) then e else f
g = if if b then c else d then e else f

h' :: [a] -> a
h' [] = error "No head for empty lists!"
h' (x:_) = x
