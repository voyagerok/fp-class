{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.Environment
import Control.Monad

genFile (fname:[]) = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	gen3 <- newStdGen	
	let strs = head $ (randomRs (5, 15) gen1 :: [Int])
	writeFile fname $ unlines $ map show $ take strs (zipWith (\x y -> (x,y))  (randomRs (-100, 100) gen2 ::[Int] ) (randomRs (-100,100) gen3::[Int]))

getQuarter (x,y)
	| x > 0 && y > 0 = 1
	| x < 0 && y > 0 = 2
	| x < 0 && y < 0 = 3
	| otherwise = 4
	

countQuarters (fname:[]) = do
	contents <- readFile fname
	let list = lines contents
	let quartList = foldl (\acc x -> acc++[show $ getQuarter $ read x]) [] list
	mapM_ print $ foldl (\[q1, q2, q3, q4] x -> if (read x) == 1 then [q1 + 1, q2, q3, q4] 
												else if (read x) == 2 then [q1, q2 + 1, q3, q4]
												else if (read x) == 3 then [q1, q2, q3 + 1, q4]
												else [q1, q2, q3, q4 + 1]) [0,0,0,0] quartList

getDistance (x,y) = sqrt $ x^2 + y^2

getMostRemotedPnt (fname:[]) = do
	contents <- readFile fname
	let list = lines contents
	print $ fst $ foldl (\(acc, max) x -> if ((getDistance $ read x) > max) then (read x, getDistance $ read x) else (acc, max)) ((0,0), 0) list

main = do
	(action : args) <- getArgs
	if (read action) == 1 then
		genFile args
	else if (read action) == 2 then
		countQuarters args
	else if (read action) == 3 then
		getMostRemotedPnt args
	else
		putStrLn "Нераспознанное действие"
	
