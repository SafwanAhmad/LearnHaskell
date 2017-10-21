import MyData

isPal :: Eq a => [a] -> Bool

isPal [] = True
isPal (x:[]) = True
isPal xs = if (head xs) == (last xs)  then  isPal (init (tail xs)) else False


myHead :: [a] -> a
myHead (x:xs) = x
myHead [] = error "empty list!"

headTailRemoved :: [a] -> [a]
headTailRemoved [x] = []
headTailRemoved (x:xs) = init xs
headTailRemoved [] = error "empty list"

-- Find head with guards
-- |headWithGuards
headWithGuards :: (Eq x) => [x] -> x
headWithGuards xs | xs == [] = error "Empty list!"
                  | otherwise = xs !! 0

-- Change metic to imperial
metricToImperial :: (Double, [Char]) -> (Double, [Char])
metricToImperial (value, measure) | measure == "m"  = (1.09361 * value, "yd")
                                 | measure == "L"  = (0.264172 * value, "gal")
                                 | measure == "kg" = (2.20462 * value, "lb")
                                 | measure == "yd" = (value/1.09361, "m")
                                 | measure == "gal" = (value/0.264172, "L")
                                 | measure == "lb" = (value/2.20462, "kg")
                                 | otherwise = error "Please provide correct measure unit <m, L, kg>"


-- Use our module MyData to convert value
testConversion value = let result = convert value 
                       in print result