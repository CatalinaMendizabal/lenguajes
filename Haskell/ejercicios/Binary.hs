module Binary where

data Bit = One | Zero deriving (Show, Eq)
type Binary = [Bit]

data SumResult = SumResult{carry::Bit, result::Bit} deriving Show

bitAdd :: Bit -> Bit -> SumResult
bitAdd Zero Zero = SumResult Zero Zero
bitAdd One One = SumResult One Zero
bitAdd _ _ = SumResult Zero One

fullBitAdd :: Bit -> Bit -> Bit -> Binary
fullBitAdd x y z = [result carryAdd, result secondAdd]
    where 
        firstAdd = bitAdd x y
        secondAdd = bitAdd (result firstAdd) z
        carryAdd = bitAdd (carry firstAdd) (carry secondAdd)