{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.IO
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import System.Directory
import System.Random

countLines (fname : []) = do
	contents <- readFile fname
	print $ length (lines contents)

toUp (fname : []) = do
	contents <- readFile fname
	putStrLn (map toUpper contents)

addStr (fname : s : flag : []) = do
	hdl <- openFile "foo.txt" ReadWriteMode
	hdl1 <- openFile fname ReadMode
	contents <- hGetContents hdl1
	if (flag == "begin") then
		hPutStr hdl (s++"\n"++contents)
	else
		hPutStr hdl (contents++"\n"++s)
	hClose hdl
	hClose hdl1
	removeFile fname	
	renameFile "foo.txt" fname

unionFiles (fname1 : fname2 : []) = do
	cnt1 <- readFile fname1
	cnt2 <- readFile fname2
	writeFile "file3" $ unlines $ zipWith (++) (lines cnt1) $ map (" "++) (lines cnt2)

appF fname n m = 
	if (m > 0) then do
		gen1 <- newStdGen
		gen2 <- newStdGen
		let symb = head (randomRs (1,n) gen2 :: [Int])
		appendFile fname $ (take symb $ (randomRs ('a','z') gen1 :: [Char])) ++ "\n"
		appF fname n (m - 1)
	else
		return ()

genFile (fname : maxL : maxS : []) = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	gen3 <- newStdGen
	--let maxSymb = head (randomRs (5,20) gen1 :: [Int])
	--let maxStrs = head (randomRs (5, 20) gen2 ::[Int])
	let strs = head (randomRs (5, (read maxL)) gen2 :: [Int])
	appF fname (read maxS) strs
			

main  = do
	(action : args) <- getArgs
	if (action == "1") then
		countLines args
	else if (action == "2") then
		addStr args
	else if (action == "3") then
		toUp args
	else if (action == "4") then
		unionFiles args
	else if (action == "5") then
		genFile args
	else
		putStrLn "Нераспознанное действие"
	
