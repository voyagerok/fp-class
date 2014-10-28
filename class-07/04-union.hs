{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.IntSet as Set
import System.IO
import Data.List


readF fname = do
	contents <- readFile fname
	return $ map read $ concatMap words $ lines contents

get_union cont = Set.toList $ Set.unions $ map Set.fromList cont

solve :: [[Int]] -> (Int, Int)
solve cont = (length $ get_union cont, sum $ get_union cont)

main = getArgs >>= mapM readF >>= print . solve
