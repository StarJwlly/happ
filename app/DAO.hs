{-# LANGUAGE LambdaCase #-}
module DAO
  where

import           Control.Monad         (forever)
import Database.MySQL.Base
import qualified System.IO.Streams     as Streams
import Data.Unique
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.Lazy                  as TL
import Data.Text.Lazy.Encoding         as TL

import Data.Binary.Put

mainA :: IO ()
mainA = do
    conn <- connect
        defaultConnectInfo {ciUser = T.encodeUtf8 $ T.pack "",
        ciPassword = T.encodeUtf8 $ T.pack "",
        ciDatabase = T.encodeUtf8 $ T.pack ""}
    s <- prepareStmt conn $ Query $ TL.encodeUtf8 $ TL.pack "SELECT * FROM usuario"
    (defs, is) <- queryStmt conn s [MySQLInt32U 18]
    print =<< Streams.toList is



getDatabaseConnection :: IO MySQLConn
getDatabaseConnection = do
   connect
    defaultConnectInfo {ciUser = T.encodeUtf8 $ T.pack "",
    ciPassword = T.encodeUtf8 $ T.pack "",
    ciDatabase = T.encodeUtf8 $ T.pack ""}

--receives the user email address
--  registers the temporary creation in the database
--  sends an email to the address with a link to the password definition webpage
createUser :: String -> IO()
createUser email = do
  conn <- getDatabaseConnection
  s <- prepareStmt conn $ Query $ TL.encodeUtf8 $ TL.pack
    ("insert into TemporaryUser (email) values (\"" ++ email ++ "\")")
  --(defs, is) <- queryStmt conn s [MySQLInt32U 18]
  --s1 <- prepareStmt conn $ Query $ TL.encodeUtf8 $ TL.pack 
  --  ("select id from TemporaryUser where email = \"" ++ email ++ "\"")
  --(defs1, is1) <- queryStmt conn s1 [MySQLInt32U 18]
  s1 <- prepareStmt conn $ Query $ TL.encodeUtf8 $ TL.pack "select * from CTemporaryUser where id = ?"
  (defs1, is1) <- queryStmt conn s1 [MySQLText $ T.pack ""]
  result <- Streams.toList is1
  print $ Prelude.map (Prelude.map (TL.unpack . TL.decodeUtf8 . runPut . putBinaryField)) result
  close conn
  return ()
--receives a string with the password redefining code
--  checks if the code exists in the database 
--  and if the current time is within 20 minutes from the creation of the code  
checkPasswordCode :: String -> IO Bool
checkPasswordCode code = do
  conn <- getDatabaseConnection
  s <- prepareStmt conn $ Query $ TL.encodeUtf8 $ TL.pack "select * from CTemporaryUser where id = ?"
  (defs, is) <- queryStmt conn s [MySQLText $ T.pack code]
  queryResults <- Streams.toList is
  print $ Prelude.map (\x -> (TL.unpack . TL.decodeUtf8 . runPut . putBinaryField) (x !! 1)) queryResults
  close conn
  return True
  --if Prelude.null queryResults
  --  then return False
  --  else if head queryResults
    
  

  
