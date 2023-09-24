--Fast exponentiation by squaring mod p
--Returns x^n mod p
expSquaring :: (Integral t, Integral a) => a -> t -> a -> a
expSquaring x n p
    | x == 0 = 0
    | n <= 0 = 1
    | (mod n 2) == 0 =  mod (expSquaring (mod (x*x) p) (div n 2) p) p
    | otherwise = mod (x * (expSquaring (mod (x*x) p) (div (n-1) 2) p)) p

--Returns a pair (x,j) where x is a 2-adic primitive root of unity mod p where n is the 2-adicity of Fp
--Also returns the value j where x^(2^j)=1 mod p
twoAdicPrimitiveRoot :: (Integral t, Integral b) => b -> t -> (t, b)
twoAdicPrimitiveRoot n p = helper p $ cartProd  [2..(p-1)] [2..n-1]
    where
        helper p [] = (0,0)
        helper p (x:xs)
            | (expSquaring (fst x) (2^(snd (x))) p) == 1 = x
            | otherwise = helper p xs
        cartProd xs ys = [(x,y) | x <- xs, y <- ys]

--Returns true if w^(x) !=1 mod p for all x<2^j, otherwise return false
checkPrimitiveRoot :: (Integral t1, Integral t2) => t2 -> t1 -> t1 -> t2 -> Bool
checkPrimitiveRoot w j x p
    | 2^j < x = True
    | (expSquaring w x p) == 1 = False
    | otherwise = checkPrimitiveRoot w (x+1) j p