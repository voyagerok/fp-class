{-1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.-}

import System.Environment
import Control.Monad.Reader

parse_config_str :: String -> (String, Int)
parse_config_str s = (takeWhile (/= '=') s, read $ dropWhile (== '=') $ snd $ span (/= '=') s) 

get_op :: (String, Int) -> Int -> Int
get_op (s, n) x
  | s == "sumand" = (x+n)
  | s == "multiplier" = (x*n)
  | s == "divisor" && n /= 0 = x `div` n
  | otherwise =  x

make_op :: [Int] -> Reader [(String, Int)] [Int]
make_op numbers = do
  conf <- ask
  return $ fmap (\x -> (foldl (flip get_op) x conf)) numbers

readF :: FilePath -> IO [(String, Int)]
readF fname = readFile fname >>= return . (map parse_config_str) . lines

main = do
  (confName : numName : xs) <- getArgs
  nums <- readFile numName >>= return . map (read) . concat . map words . lines
  conf <- readF confName
  print $ runReader (make_op nums) conf
  