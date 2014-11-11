import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	| a `mod` 3 == 0 = 0
	| a `mod` 2 /= 0 = a ^ 2
	| otherwise = a ^ 3 

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n f = foldl (\acc x -> fmap reduce acc) f [1..n] 

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldl (\acc x -> acc ++ [fst x + snd x]) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe l = Just (sum $ map fst l)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty"
toEither l = Right (sum $ map fst l)

-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO l = do
  gen <- newStdGen
  let n = fst $ randomR (1, 20) gen
  return $ (n * sum $ map snd l)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs l = (head l, read $ last l)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
  contents <- readFile fname
  return $ foldl (\acc x -> acc ++ [(read $ head x, read $ last x)]) [] (map words $ lines contents)

test fname = do
	contents <- readFile fname
	print $ foldl (\acc x -> acc ++ [(head x, last x)]) [] (map words $ lines contents)

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
:main "temp1.txt" 1
[8,64,0,512]
Just 1000
Right 1000
0

:main "temp2.txt" 1
[0,0,25,512,512]
Just 361
Right 361
32768
-}
