module Interface where

class SchemeData t where
    symbol :: String -> t
    number :: Integer -> t
    boolean :: Bool -> t
    cons :: t -> t -> t
    nil :: t
    proper :: [t] -> t
    improper :: [t] -> t -> t

