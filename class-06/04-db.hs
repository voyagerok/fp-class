{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}

import Data.List
import Data.Char
import System.Environment
import Control.Monad
import System.IO

avgAge fname = do
	contents <- readFile fname
	let list = lines contents
	let list1 = foldl (\acc x -> if x == ";" then acc++[";"] else acc++[x]) [] list
	let list2 = map lines list1
	mapM_ print list2
	

main = undefined
