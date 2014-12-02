{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer 

import Data.Char
import System.Environment

isValid :: String -> [String]-> Bool
isValid s (len : digits : letters : punct : [])= (length s >= read len) && 
  (if (read digits) then (any isNumber s) else True) && 
  (if (read letters) then (any isAlpha s) else True) &&
  (if (read punct) then (any isPunctuation s) else True)

getValidPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) String
getValidPassword = do
  str <- ask
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  guard (isValid s str)
  return s
 
askPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."

main = do
  args <- getArgs
  pair <- runWriterT (runReaderT (runMaybeT askPassword) args)
  print $ snd pair