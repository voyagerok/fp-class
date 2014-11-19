import Control.Monad
import Control.Applicative 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if (unbalanced p) > balance then Left "Unbalanced left" else if (unbalanced p) < balance * (-1) then Left "unbalanced right" else Right p
  where
    unbalanced (l, r) = l - r

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = updatePole (left + n, right + n)

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0, 0))

banana :: Pole -> Either String Pole
banana = const (Left "Banana")

test1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
       >>= landLeft (-1) >>= landRight (-2)) == Left "unbalanced right"
test2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
test3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left ("Banana")
test4 = (return (0, 0) >>= landLeft 1 >>= unlandAll >>= landRight 1) == Right (0, 1)
test5 = (return (0, 0) >>= landLeft 2 >>= landBoth 3 >>= landLeft 3) == Left ("Unbalanced left")
