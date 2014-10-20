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
import Control.Monad
import Data.List
import Data.Char
import System.Directory

countLines :: FilePath -> IO ()
countLines fname = do
	contents <- readFile fname
	print $ length (lines contents)

toUp fname = do
	contents <- readFile fname
	putStrLn (map toUpper contents)

addStr fname s flag = do
	hdl <- openFile "foo.txt" ReadWriteMode
	hdl1 <- openFile fname ReadMode
	contents <- hGetContents hdl1
	if (flag == 0) then
		hPutStr hdl (s++"\n"++contents)
	else
		hPutStr hdl (contents++"\n"++s)
	hClose hdl
	hClose hdl1
	removeFile fname	
	renameFile "foo.txt" fname

unionFiles fname1 fname2 = do
	cnt1 <- readFile fname1
	cnt2 <- readFile fname2
	writeFile "file3" $ unlines $ (zipWith (++) (lines $ foldl (\acc x -> if x == '\n' then acc++[' ']++[x] else acc++[x]) [] cnt1) (lines cnt2))

main  = do
	rs <- sequence [getLine, getLine, getLine]
	print rs
