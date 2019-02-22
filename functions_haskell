parsoma:: [(Integer, Integer)] -> Integer
parsoma [] = 0
parsoma ((a,b):as) = b + parsoma as

pertence:: Integer -> [Integer] -> Bool
pertence x [] = False
pertence x (a:as) = if(a == x)
                    then True 
                    else pertence x as
                    
inter:: [Integer]->[Integer]->[Integer]
inter [] _ = []
inter _ [] = []
inter(a:as) b = if(pertence a b) then [a] ++ inter as b else inter as b

diff:: [Integer]->[Integer]->[Integer]
diff [] _ = []
diff _ [] = []
diff(a:as) b = if(pertence a b) then diff as b  else [a] ++ diff as b
