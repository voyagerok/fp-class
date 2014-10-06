import Data.Char
import Data.List
{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

f11a :: Integral a => [a] -> [a]
f11a = map (*2)

f11b :: Integral a => [a] -> [a]
f11b = map (\x -> if even x then x*2 else x)

f11c :: Integral a => [a] -> [a]
f11c = map (\x -> if odd x then 0  else x)

f11d :: Integral a =>a ->  [a] -> [a]
f11d k = filter (<= k)

f11e :: Integral a => [a] -> [a]
f11e = filter (< 0)

f11f :: Integral a => [a] -> [a]
f11f = filter (\x -> (x > 0) && even x )

{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной 
b) преобразовать декартовы координаты в полярные.
-}
setQuarter :: (Eq a, Num a, Num a1, Num a2, Ord a1, Ord a2) => a -> [(a1,a2)] -> [(a1,a2)] 
setQuarter n = filter (\(x,y) -> if n == 1 then (x > 0) && (y > 0)
									else if n == 2 then (x < 0) && (y > 0)
									else if n == 3 then (x < 0) && (y < 0)
									else if n == 4 then (x >0) && (y < 0)
									else error "Wrong quarter")

{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}

f13a :: [String] -> [String]
f13a = map $ map toUpper

f13b :: Int -> [[a]] -> [[a]]
f13b n = filter (\x -> length x == n)

f13c :: Eq a => a -> [[a]] -> [[a]]
f13c c = filter (\x -> head x == c)

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a_0=1, a_n=(1+a_{n-1})/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

f2a :: [Integer]
f2a = iterate (+1) 0

f2b :: [Integer]
f2b = iterate (+2) 0

f2c :: [Double]
f2c = iterate ((/2).(+1)) 1

f2d :: [Char]
f2d = take 26 (iterate succ 'a')

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}

f3a :: [Char] -> [[Char]]
f3a = groupBy(\x y -> isDigit x == isDigit y)

f3b :: [(Double, Double)] -> [[(Double, Double)]]
f3b = groupBy(\(x1,x2) (y1,y2) -> ((x1 > 0) == (y1 > 0)) && ((x2 > 0) == (y2 > 0)))

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = undefined

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}
