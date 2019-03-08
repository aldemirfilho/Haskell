pertence:: Integer -> [Integer] -> Bool
pertence x [] = False
pertence x (a:as) = if(a == x)
                    then True 
                    else pertence x as

isGrafo:: [Integer] -> [(Integer, Integer)] -> Bool
isGrafo [] _ = False
isGrafo v [] = True
isGrafo v ((o, d):as) = (pertence o v) &&  
                        (pertence d v) && 
                        isGrafo v as

grau:: Integer -> [(Integer,Integer)] -> Integer
grau v [] = 0
grau v ((x,y):as) = if (v == x)
                    then if (v == y)
                          then 2 + (grau v as)
                         else 1 + (grau v as)
                    else 
                       if (v == y)
                        then 1 + (grau v as)
                       else (grau v as)
                     
grau_emissao:: Integer -> [(Integer,Integer)] -> Integer
grau_emissao v [] = 0
grau_emissao v ((x,y):as) = if(v == x)
                              then 1 + (grau_emissao v as)
                            else (grau_emissao v as)

grau_recepcao:: Integer -> [(Integer,Integer)] -> Integer
grau_recepcao v [] = 0
grau_recepcao v ((x,y):as) = if(v == y)
                              then 1 + (grau_recepcao v as)
                             else (grau_recepcao v as)

tipo_vertice:: Integer -> [(Integer, Integer)] -> Char
tipo_vertice v ((x,y):as) = if((grau_emissao v ((x,y):as) == 0 && 
                                  grau_recepcao v ((x,y):as) == 0) || 
                                  (grau_emissao v ((x,y):as) /= 0 && 
                                  grau_recepcao v ((x,y):as) /= 0))
                            then 'n'
                            else if(grau_emissao v ((x,y):as) /= 0 && 
                                  grau_recepcao v ((x,y):as) == 0)
                            then 'f'
                            else if(grau_emissao v ((x,y):as) == 0 && 
                                  grau_recepcao v ((x,y):as)  /= 0)
                            then 's'
