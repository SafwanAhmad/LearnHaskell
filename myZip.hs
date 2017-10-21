-- myZip.hs 
-- |The zipTogether function binds two lists together
zipTogether [] _ = []
zipTogether _ [] = [] 
zipTogether xs ys = (head xs, head ys) : zipTogether (tail xs) (tail ys)