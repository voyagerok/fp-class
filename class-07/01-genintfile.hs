{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.IO
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import System.Directory
import System.Random

appF fname n m minN maxN =
	if (m > 0) then do
		gen1 <- newStdGen
		let list = take n $  (randomRs (minN, maxN) gen1 :: [Int])
		appendFile fname $ (foldl (\acc x -> acc++(show x)++" ") [] list) ++ "\n"
		appF fname n (m - 1) minN maxN
	else
		return ()

genFile (fname : minN : maxN : maxL : maxS : []) = do
	exists <- doesFileExist fname
	when (exists) $ removeFile fname
	appF fname (read maxS) (read maxL) (read minN) (read maxN)

main = do
args <- getArgs
genFile args
