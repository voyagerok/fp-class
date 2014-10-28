{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set
import System.IO
import Data.List

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
	contents <- readFile fname
	return $ map read $ concatMap words $ lines contents

get_intersect cont = Set.toList $ foldl1 Set.intersection $ map Set.fromList cont

solve :: [[Int]] -> (Int, [Int])
solve cont = (length $ get_intersect cont, get_intersect cont)

main = getArgs >>= mapM readNumFile >>= print.solve
