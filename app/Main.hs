{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy                 as BL
import           Data.Maybe
import           Database.Mbtiles
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Exit

type TileServer =
  "tiles" :>
    Capture "zoom" Int :>
      Capture "x" Int :>
        Capture "y" Int :>
          Get '[OctetStream] BL.ByteString

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
            Just b  -> return b
        getTileBS :: Z -> X -> Y -> MbtilesT (ExceptT ServantErr IO) (Maybe BL.ByteString)
        getTileBS = getTile

mbtilesFileHere = "/path/to/mbtiles/file.mbtiles"

api :: MbtilesPool -> Server TileServer
api pool = enter (transformToHandler pool) tileAPI

service :: MbtilesPool -> Server TileServer
service = api

apiProxy :: Proxy TileServer
apiProxy = Proxy

middleware :: Application -> Application
middleware = logStdoutDev

app :: MbtilesPool -> Application
app = serve apiProxy . service

startAPI :: MbtilesPool -> IO ()
startAPI = (run 3000 . middleware) . app

main = do
  me <- getMbtilesPool mbtilesFileHere
  either (\e -> print e >> exitFailure) startAPI me
