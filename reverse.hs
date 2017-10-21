rev [] = []
rev x = rev (tail x) ++ [(head x)]
