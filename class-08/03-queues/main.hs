import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import System.IO
import System.Random
import System.Environment

--checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
--checkQueue q = lastElem (enqueue q 5) == 5
-- where
 -- lastElem q = let (x, q') = dequeue q in
    --           if isEmpty q' then x else lastElem q'

rand_list n = do
	gen <- newStdGen
   	return $ take n $ (randomRs (1, 1000) gen :: [Int])

fromListToQueue q (x:xs) 0 = q
fromListToQueue q (x : xs) n = fromListToQueue (enqueue q x) xs (n - 1)

deq q n
	| n == 0 = q
	| otherwise = deq (snd $ dequeue q) (n - 1)

add_remove q l 0 = q
add_remove q l n = deq (fromListToQueue q l n) (n - 1)

checkQueue q l k n
	| k <= n = checkQueue (add_remove q l k) (drop k l) (k + 1) n
	| otherwise = q

fromQueueToList q
	| isEmpty q = []
	| otherwise = [x] : fromQueueToList queue
		where
			(x, queue) = dequeue q  


--main = print $
--         checkQueue (enqueue empty 10 :: Q.Queue Int)
--         &&  checkQueue (enqueue empty 10 :: FQ.Queue Int)

main = do
	[args] <- getArgs
	gen <- newStdGen
	let l = randomRs (-1000, 1000) gen :: [Int]
	let n = read args :: Int
	let q1 = checkQueue (enqueue empty 10 :: Q.Queue Int) l 1 n
	let q2 = checkQueue (enqueue empty 10 :: FQ.Queue Int) l 1 n
	print $ (fromQueueToList q1) == (fromQueueToList q2)
