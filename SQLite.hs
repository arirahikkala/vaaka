module SQLite where

import Data.List
import Database.SQLite hiding (insertRow)
import Data.Char

insertRow h tab cs = do
   let stmt = ("INSERT INTO " ++ tab ++
               tupled (toVals fst) ++ " VALUES " ++
               tupled (toVals (quote.snd)) ++ ";")
   execStatement_ h stmt
  where
   toVals f = map (toVal f) cs
   toVal f p = f p -- ($ f)

   quote "" = "''"
   quote nm@(x:_)
--    | isDigit x = nm
    | otherwise = '\'':toSQLString nm ++ "'"


tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"
