somalista:: [Integer] -> Integer
somalista [] = 0
somalista (a:as) = a + somalista (as)

soma2lista:: [[Integer]] -> [Integer]
soma2lista [] = []
soma2lista (a:as) = [somalista a] ++ soma2lista as

lista1par:: [(Integer, Integer)] -> [Integer]
lista1par [] = []
lista1par((a, b):as) = [a] ++ lista1par as

somapares:: [(Integer, Integer)] -> [Integer]
somapares [] = []
somapares ((a,b):as) = [a + b] ++ somapares as 

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

uniao_lista:: [Integer] -> [Integer] -> [Integer]
uniao_lista a b = a ++ b

listapar:: [Integer] -> [Integer] -> [(Integer, Integer)]
listapar [] b = []
listapar (a:as) b = formapar a b ++ listapar as b

formapar:: Integer -> [Integer] -> [(Integer, Integer)]
formapar x [] = []
formapar x (a:as) = [(x, a)] ++ formapar x as
