import qualified Data.Vector.Unboxed as V

binaryList n = bs n []
    where
        bs n s | n > 1 = let (n', r) = divMod n 2 
                         in bs n' (r:s)
               | otherwise = n:s

crule (s:ngb) | s == 1 = if cn == 2 || cn == 3 then 1 else 0
              | s == 0 = if cn == 3 then 1 else 0
    where
        cn = sum ngb

-- Vector representing Conway Life's Game rules
-- The index represent a state S_ij(t - dt), N_ij(t - dt) in binary format, 
-- the value at the index position is the S_ij(t) value 
conway :: V.Vector Int 
conway = V.fromList $ map (crule.binaryList) [0..512]
