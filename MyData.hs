-- We can export useful code to a module
-- so that other projects can reuse them.
-- Create this to a module MyData
-- Note: The module name and file name should be same.
module MyData
 (MetricUnit(..),
  ImperialUnit(..),
  Measurement(..),
  convert)
  where



-- MetricUnit is called the type constructor
-- Everything after the equal sign is called the value constructor
data MetricUnit = Meter | Litre | Kilogram 
                deriving (Show, Eq)

data ImperialUnit = Yard | Gallon | Pound
                deriving (Show, Eq)

-- Create a Measurement Data Type
data Measurement = MetricMeasurement Double MetricUnit 
                 | ImperialMeasurement Double ImperialUnit 
                deriving (Show)

-- Create a Point data type
-- With record syntax we get two getters
-- xvar and yvar
-- if b is a Point then b = Point 2.0 3.0 then
-- xvar b = 2.0
-- yvar b = 3.0
data Point = Point 
            {xvar:: Double, yvar:: Double}
            deriving (Show)

-- Write a simple function that takes in a MetricUnit, and returns its symbol
-- Meter → "m"
-- Liter → "L"
-- KiloGram → "kg"

returnMetricSymbol :: MetricUnit -> String
returnMetricSymbol x | x == Meter = "m"
                   | x == Litre = "L"
                   | x == Kilogram = "kg"

-- Write a simple function that takes in a ImperialUnit, and returns its symbol
-- Yard → "yd"
-- Gallon → "gal"
-- Pound → "lb"

returnImperialSymbol :: ImperialUnit -> String
returnImperialSymbol x | x == Pound = "lb"
                   | x == Gallon = "gal"
                   | x == Yard = "yd"

-- Change metric to/from imperial
convert :: Measurement -> Measurement
convert (MetricMeasurement value unit) | unit == Meter = (ImperialMeasurement (1.09361 * value) Yard)
                                       | unit == Litre = (ImperialMeasurement (0.264172 * value) Gallon)
                                       | unit == Kilogram = (ImperialMeasurement (2.20462 * value) Pound)


convert (ImperialMeasurement value unit) | unit == Yard = (MetricMeasurement (value/1.09361) Meter)
                                         | unit == Gallon = (MetricMeasurement (value/0.264172) Litre)
                                         | unit == Pound = (MetricMeasurement (value/2.20462) Kilogram)