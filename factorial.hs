facA 0 = 1
facA n = n * (facA (n-1))


facB n = if n > 0 then n * (facB (n -1))  else 1