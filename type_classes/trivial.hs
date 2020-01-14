module Trivial where

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where 
    (==) Mon Mon = True 
    (==) Tue Tue = True 
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False 

instance Eq Date where
    (==) (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') = 
            weekday == weekday'
            && dayOfMonth == dayOfMonth'




data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a' 

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString x) (TisAString x') = x == x'
    (==) (TisAnInt _) (TisAString _) = False
    (==) (TisAString _) (TisAnInt _) = False


data Pair a = Pair a a 
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') =  x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y ==y'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) (ThisOne _) (ThatOne _) = False
    (==) (ThatOne _) (ThisOne _) = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye x) (Goodbye x') = x == x'
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False