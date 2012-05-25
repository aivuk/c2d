binaryString n = bs n []
    where
        bs 1 s = 1:s
        bs 0 _ = [0]
        bs n s = bs n' (r:s)
            where (n', r) = divMod n 2

rule (s:ngb) | s == 1 = if cn == 2 || cn == 3 then 1 else 0
             | s == 0 = if cn == 3 then 1 else 0
    where
        cn = sum ngb

