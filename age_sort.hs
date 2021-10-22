main :: IO ()
main = do
   let result = sortList [("Helga", 90), ("Emil", 6), ("Julia", 23), ("Otto", 1), ("Eduard", 55), ("Elfriede", 34), ("Emma", 24), ("Anton", 31), ("Uta", 10), ("Hannah", 64), ("Alfred", 15), ("Toni", 17), ("Jakob", 42), ("Maria", 101), ("Jonathan", 28), ("Selina", 44), ("Georg", 6), ("Ulf", 73), ("Tanja", 4), ("Ina", 52)]
   print result

sortList :: [(String, Int)] -> [String]
sortList x = kleinKinder x ++ kinderUndJugendliche x ++ studierende x ++ erwachsene x ++ rentnerInnen x

kleinKinder :: [(String, Int)] -> [String]
kleinKinder ((x,y):xs)
   | y>=0 && y<=5 = x : kleinKinder xs
   | otherwise = kleinKinder xs
kleinKinder [] = []

kinderUndJugendliche :: [(String, Int)] -> [String]
kinderUndJugendliche ((x,y):xs)
   | y>=6 && y<=17 = x : kinderUndJugendliche xs
   | otherwise = kinderUndJugendliche xs
kinderUndJugendliche [] = []

studierende :: [(String, Int)] -> [String]
studierende ((x,y):xs)
   | y>=18 && y<=29 = x : studierende xs
   | otherwise = studierende xs
studierende [] = []

erwachsene :: [(String, Int)] -> [String]
erwachsene ((x,y):xs)
   | y>=30 && y<=64 = x : erwachsene xs
   | otherwise = erwachsene xs
erwachsene [] = []

rentnerInnen :: [(String, Int)] -> [String]
rentnerInnen ((x,y):xs)
   | y>=65 = x : rentnerInnen xs
   | otherwise = rentnerInnen xs
rentnerInnen [] = []
