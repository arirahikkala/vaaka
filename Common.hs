module Common where

newtype Seller = Seller { sellerName :: String } deriving (Eq, Ord)
data Car = Car { 
      carId :: Int,
      name :: String,
      numAxles :: Int } deriving (Eq)

data Grain = Ohra | Kaura | Vehna | Seka deriving Eq