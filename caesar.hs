import Data.String ()
import Data.Char ( ord, chr )

shift :: Char -> Maybe Int -> Char
shift c (Just n)
   | ord c >= 65 && ord c <= 90 = chr ((((ord c + n)-65) `mod` 26)+65)
   | ord c >= 97 && ord c <= 122 = chr ((((ord c + n)-97) `mod` 26)+97)
   | otherwise = chr (ord c)

caesar :: String -> Maybe Int -> String
caesar ws Nothing = caesar ws (Just 42)
caesar ws (Just s) = [shift w (Just s) | w <- ws]