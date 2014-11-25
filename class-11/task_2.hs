{- 2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell. -}

import Control.Monad.Writer

eps = 0.000001
 
calc_next res x n = -1 * res * (x ^ 2) / ((n + 1) * (n + 2))

taylor res prev x n = tell [prev] >> if abs(res - (calc_next res x n)) < eps then return prev
  	else taylor x (prev + (calc_next res x n)) (calc_next res x n) (n + 2)

sinn x = runWriter $ taylor x x x 1
coss x = runWriter $ taylor 1 1 x 0