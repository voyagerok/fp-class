{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Heart | Diamond | Club | Spade deriving(Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
					deriving (Show, Eq, Ord)

data Card = Card Value Suit deriving (Show, Eq, Ord)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2 

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) = compare v1 v2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round ((x:xs), (y:ys)) 
	| x `beats` y == GT = (xs ++ [x] ++ [y], ys)
	| x `beats` y == LT = (xs, ys ++ [x] ++ [y])
	| otherwise = game_round ((xs++[x]), (ys++[y]))

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game (set1, set2) = func (set1, set2) 0
	where 
		func ([], _) c = (Second, c)
		func (_, []) c = (First, c)
		func ((x:xs), (y:ys)) c = func (game_round((x:xs), (y:ys))) (c + 1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}


test_game1 = game ([Card Two Heart, Card Six Diamond, Card Nine Club, Card Three Diamond, Card Ten Club, Card Four Diamond, Card Jack Club, Card Six Heart, Card Five Spade, Card Eight Diamond],[Card Jack Club, Card Seven Spade, Card Queen Heart, Card Ace Spade, Card King Spade, Card Eight Heart, Card Ace Diamond, Card Nine Spade, Card Seven Heart, Card King Diamond]) == (Second,10)

test_game2 = game ([Card Six Diamond, Card King Diamond, Card Ten Club, Card Jack Diamond, Card Ten Club, Card Four Diamond, Card Jack Club, Card Six Heart, Card Five Spade, Card Eight Diamond],[Card Two Club, Card Queen Spade, Card Five Heart, Card Eight Spade, Card Four Spade, Card Three Heart, Card Ten Diamond, Card Two Spade, Card Four Heart, Card Seven Diamond]) == (First, 10)

test_game3 = game ([Card Six Diamond, Card King Diamond, Card Ten Club, Card Jack Diamond, Card Ten Club, Card Four Diamond, Card Jack Club, Card Six Heart, Card Five Spade, Card Eight Diamond, Card Nine Spade],[Card Two Club, Card Queen Spade, Card Five Heart, Card Eight Spade, Card Four Spade, Card Three Heart, Card Ten Diamond, Card Two Spade, Card Four Heart, Card Seven Diamond, Card Ten Diamond]) == (First,31)

test_game4 = game ([ Card Six Diamond, Card King Diamond, Card Ten Club, Card Jack Diamond, Card Ten Club, Card Four Diamond, Card Jack Club, Card Six Heart, Card Five Spade, Card Eight Diamond, Card King Heart, Card Nine Spade],[Card Two Club, Card Queen Spade, Card Five Heart, Card Eight Spade, Card Four Spade, Card Three Heart, Card Ten Diamond, Card Two Spade, Card Four Heart, Card Seven Diamond, Card Ace Spade, Card Ten Diamond]) == (Second,130)

test_game5 = game ([ Card Six Diamond, Card King Diamond, Card Ten Club, Card Jack Diamond, Card Ten Club, Card Four Diamond, Card Jack Club, Card Six Heart, Card Five Spade, Card Eight Diamond, Card King Heart, Card Nine Spade, Card Seven Spade],[Card Two Club, Card Queen Spade, Card Five Heart, Card Eight Spade, Card Four Spade, Card Three Heart, Card Ten Diamond, Card Two Spade, Card Four Heart, Card Seven Diamond, Card Ace Spade, Card Ten Diamond,  Card Jack Diamond]) == (Second,1649)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

game1 :: ([Card], [Card]) -> (Winner, Int, [(Card, Card)])
game1 (set1, set2) = func1 (set1, set2) 0 []
	where 
		func1 ([], _) c l = (Second, c, l)
		func1 (_, []) c l = (First, c, l)
		func1 ((x:xs), (y:ys)) c l = func1 (game_round((x:xs), (y:ys))) (c + 1) (l++[(x,y)])

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
