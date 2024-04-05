{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text.IO (hGetContents)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO (openFile, IOMode (ReadMode))
import qualified Diff
import Data.List (transpose)

import Util
import qualified DAO

main :: IO ()
main = do 
  a <- DAO.checkPasswordCode "da4b9237bacccdf19c0760cab7aec4a8359010b0"
  putStrLn $ show a
  return ()
  --serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
  [ dir "echo"    $ echo
  , dir "query"   $ queryParams
  , dir "form"    $ formPage
  , dir "fortune" $ fortune
  , dir "files"   $ fileServing
  , dir "upload"  $ upload
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
           H.h1 "Hello!"
           H.p "Writing applications with happstack-lite is fast and simple!"
           H.p "Check out these killer apps."
           H.p $ a ! href "/echo/secret%20message"  $ "echo"
           H.p $ a ! href "/query?foo=bar" $ "query parameters"
           H.p $ a ! href "/form"          $ "form processing"
           H.p $ a ! href "/fortune"       $ "(fortune) cookies"
           H.p $ a ! href "/files"         $ "file serving"
           H.p $ a ! href "/upload"        $ "file uploads"

echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "echo" $ do
          p $ "echo says: " >> toHtml msg
          p "Change the url to echo something else."

queryParams :: ServerPart Response
queryParams =
    do mFoo <- optional $ lookText "foo"
       ok $ template "query params" $ do
         p $ "foo is set to: " >> toHtml (show mFoo)
         p $ "change the url to set it to something else."

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ template "form" $
              form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
                label ! A.for "msg" $ "Say something clever"
                input ! type_ "text" ! A.id "msg" ! name "msg"
                input ! type_ "submit" ! value "Say it!"

    processForm :: ServerPart Response
    processForm =
        do method POST
           msg <- lookText "msg"
           ok $ template "form" $ do
             H.p "You said:"
             H.p (toHtml msg)


fortune :: ServerPart Response
fortune = msum [ viewFortune, updateFortune ]
    where
      viewFortune :: ServerPart Response
      viewFortune =
          do method GET
             mMemory <- optional $ lookCookieValue "fortune"
             let memory = fromMaybe "Your future will be filled with web programming." mMemory
             ok $ template "fortune" $ do
                    H.p "The message in your (fortune) cookie says:"
                    H.p (toHtml memory)
                    form ! action "/fortune" ! enctype "multipart/form-data" ! A.method "POST" $ do
                      label ! A.for "fortune" $ "Change your fortune: "
                      input ! type_ "text" ! A.id "fortune" ! name "new_fortune"
                      input ! type_ "submit" ! value "Say it!"

      updateFortune :: ServerPart Response
      updateFortune =
          do method POST
             fortune <- lookText "new_fortune"
             addCookies [(Session, mkCookie "fortune" (Data.Text.Lazy.unpack fortune))]
             seeOther ("/fortune" :: String) (toResponse ())


fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."

upload :: ServerPart Response
upload =
       msum [ uploadForm
            , handleUpload
            ]
    where
    uploadForm :: ServerPart Response
    uploadForm =
        do method GET
           ok $ template "upload form" $ do
             form ! enctype "multipart/form-data" ! A.method "POST" ! action "/upload" $ do
               input ! type_ "file" ! name "osu_file"
               input ! type_ "file" ! name "diff_file"
               input ! type_ "submit" ! value "upload"

    handleUpload :: ServerPart Response
    handleUpload = do
      (osuTmpFile, osuUploadName, osuContentType) <- lookFile "osu_file"
      (diffTmpFile, diffUploadName, diffContentType) <- lookFile "diff_file"

      let diffFile = if diffUploadName == ""
          then "./app/diff.txt"
          else diffTmpFile

      liftIO $ putStrLn "asdad"
      --result <- liftIO $  Diff.caclulate osuTmpFile diffFile
      (artistName, songName, creatorName, diffName, keymode, diffDataL, diffDataR) <- liftIO $ Diff.getData osuTmpFile diffFile
      
      --liftIO $ putStrLn $ show (artistName, songName, creatorName, diffName, keymode, diffData)

      let diffDataPerColTotalL = map (map sum) (snd diffDataL)
      let diffDataPerColTotalR = map (map sum) (snd diffDataR)

      liftIO $ putStrLn "mmkmkmkmk"
      liftIO $ putStrLn $ show (fst diffDataL)
      liftIO $ putStrLn $ show (fst diffDataR)
      liftIO $ putStrLn $ show diffDataPerColTotalL
      liftIO $ putStrLn $ show diffDataPerColTotalR



      --liftIO $ putStrLn $ show result

      --TODO show more interesting data
      


      ok $ template "file uploaded" $ do
           H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.js" $ ""
           H.canvas ! A.id "chartL" ! A.style "width:100%;max-width:50%" $ ""
           H.script $ toHtml $ (scatterChartMaker "chartL" (map fromIntegral (fst diffDataL)) $ transpose diffDataPerColTotalL) 1
           H.canvas ! A.id "chartR" ! A.style "width:100%;max-width:50%" $ ""
           H.script $ toHtml $ (scatterChartMaker "chartR" (map fromIntegral (fst diffDataR)) $ transpose diffDataPerColTotalR) (1 + keymode `div` 2)
           H.canvas ! A.id "chartT" ! A.style "width:100%;max-width:50%" $ ""
           H.script $ toHtml $ (scatterChartMaker "chartT" (map fromIntegral (fst diffDataR)) $ transpose (zipWith (++) diffDataPerColTotalL diffDataPerColTotalR)) 1
           p (toHtml $ show (artistName, songName, creatorName, diffName, keymode, diffDataL, diffDataR))

           p (toHtml $ "uploaded name:  " ++ diffUploadName)
           p (toHtml $ "content-type:   " ++ show diffContentType)


scatterChartMaker :: String -> [Double] -> [[Double]] -> Int -> String
scatterChartMaker name xValues yValues colourStart = "new Chart('" ++ name ++ "', {\
\  type: 'scatter',\
\  data: {\
\    datasets: [\
\    " ++ fst (foldl (\(x, i) y -> (x ++ scatterChartData (zip xValues y) colourStart i, i + 1)) ("", 1) yValues) ++ "]\
\  }\
\});"

scatterChartData :: [(Double, Double)] -> Int -> Int -> String
scatterChartData points colourStart column = "{\
\      label: 'column " ++ show column ++ "',\
\      backgroundColor: '" ++ kellyColours !! (colourStart + column) ++ "',\
\      data: [" ++ finalPoints ++ "] },"
  where finalPoints = foldl (\x y -> x ++ scatterPointToJson y ++ ",") "" $ filter (\x -> snd x > 0) points

scatterPointToJson :: (Double, Double) -> String
scatterPointToJson (x, y) = "{ x: " ++ show x ++ ", y: " ++ show y ++ "}"

--from https://gist.github.com/ollieglass/f6ddd781eeae1d24e391265432297538?permalink_comment_id=2580138#gistcomment-2580138
--original https://eleanormaclure.files.wordpress.com/2011/03/colour-coding.pdf




--TODO aplicar desing pattern Adapter
--  https://refactoring.guru/design-patterns/adapter

--TODO database mySQL
--  https://hackage.haskell.org/package/mysql-haskell

--TODO criptografia
--  https://hackage.haskell.org/package/Crypto
--  eac conexao com com a database
--  SHA para converter o id para a api key e codigo de definicao de senha


--TODO transmitir os dados por json
--  https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html#g:1

--TODO email
--  https://hackage.haskell.org/package/smtp-mail

--TODO UI
--  usar bootstrap
--  fazer outras paginas 