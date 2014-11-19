{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import Data.Ord
import Data.List

data Student = Student {name :: String, age :: Int, group :: String} deriving (Eq)

instance Show Student where
  show (Student n a g) = n ++ " " ++ (show a) ++ " " ++ g ++ "\n"
  
instance Ord Student where
  compare (Student n1 a1 g1) (Student n2 a2 g2) = compare n1 n2
  
readData [] = []
readData xs = (take 3 xs) : readData (drop 3 xs)

intoStudent xs = foldl (\acc [n, a, g] -> (Student n (read a) g) : acc ) [] xs

readF fname = (return . intoStudent . readData . words) `liftM` (readFile fname)

writeF :: [String] -> IO ()
writeF st = return (show st) >>= writeFile "st12.txt"

main = ((++) `liftM` (readF "st1.txt") `ap` (readF "st2.txt")) >>= (writeF . concat . sort)
