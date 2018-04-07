{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy                 as BL
import           Data.Maybe
import           Database.Mbtiles
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Environment
import           System.Exit

type TileServer =
  "tiles" :>
    Capture "zoom" Int :>
      Capture "x" Int :>
        Capture "y" Int :>
          Get '[OctetStream] (Headers '[Header "Content-Encoding" String] BL.ByteString)

mbtilesToHandler :: MbtilesPool -> MbtilesT (ExceptT ServantErr IO) a -> Handler a
mbtilesToHandler pool mbt = do
  e <- runMbtilesPoolT pool mbt
  return e

transformToHandler :: MbtilesPool -> MbtilesT (ExceptT ServantErr IO) :~> Handler
transformToHandler p = Nat (mbtilesToHandler p)

tileAPI :: ServerT TileServer (MbtilesT (ExceptT ServantErr IO))
tileAPI = getTileFromDB
  where getTileFromDB z x y = do
          mbt <- getTileBS (Z z) (X x) (Y y)
          case mbt of
            Nothing -> lift $ throwError err404
            Just b  -> return $ addHeader "gzip" b
        getTileBS :: Z -> X -> Y -> MbtilesT (ExceptT ServantErr IO) (Maybe BL.ByteString)
        getTileBS = getTile

api :: MbtilesPool -> Server TileServer
api pool = enter (transformToHandler pool) tileAPI

service :: MbtilesPool -> Server TileServer
service = api

apiProxy :: Proxy TileServer
apiProxy = Proxy

middleware :: Application -> Application
middleware = logStdoutDev . simpleCors

app :: MbtilesPool -> Application
app = serve apiProxy . service

startAPI :: Int -> MbtilesPool -> IO ()
startAPI p = (run p . middleware) . app

main = do
  tilesFile <- fromMaybe "" . listToMaybe <$> getArgs
  tilesPort <- maybe 9000 read . listToMaybe . drop 1 <$> getArgs
  me <- getMbtilesPool tilesFile
  either (\e -> print e >> exitFailure) (startAPI tilesPort) me
