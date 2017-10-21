triple :: Int -> Int
triple x = x * 3


l2norm (x1,y1) (x2,y2) = sqrt ( sqrsum (x1-x2) (y1-y2) )
    where sqrsum a b = a*a + b*b

l2norm' (x1,y1) (x2,y2) = let sqrsum a b = a*a + b*b in sqrt (sqrsum (x1-x2) (y1-y2) )


mul = x * y
   where x = 4
         y = 5

expr1 = z / x + y
    where x = 7
          y = negate x
          z = y * 10


mymin :: Ord a => (a,a) -> a
mymin (x,y) = z 
    where z = min x y


solve :: Double -> Double -> Double -> [(Double,Double)]
solve a b c = let (re,im) = if des >= 0
                                then ( (-b + sqrt des)/2/a, 0)
                                else ( (-b/2/a), (sqrt (-des))/2/a) 
                 in [(re,im),(re,(-im))]
                 where  des = (b*b - 4*a*c)
