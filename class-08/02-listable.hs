{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import Data.Char

{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
  toList :: a -> [a]
  fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
  toList = words
  fromList = unwords

instance Listable Integer where
  toList n = map toInteger $ map digitToInt $ show n
  fromList l = fst $ foldl (\(acc, mul) x -> (acc + x*mul, mul * 10)) (0, 1) (reverse l)