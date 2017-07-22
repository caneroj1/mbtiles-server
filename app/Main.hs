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

type TileServer =
  "tiles" :>
    Capture "zoom" Int :>
      Capture "x" Int :>
        Capture "y" Int :>
          Get '[OctetStream] BL.ByteString

mbtilesToHandler :: FilePath -> MbtilesT (ExceptT ServantErr IO) a -> Handler a
mbtilesToHandler fp mbt = do
  e <- runMbtilesT fp mbt
  either (const (fail "MBTiles Error")) return e

transformToHandler :: FilePath -> MbtilesT (ExceptT ServantErr IO) :~> Handler
transformToHandler fp = Nat (mbtilesToHandler fp)

tileAPI :: ServerT TileServer (MbtilesT (ExceptT ServantErr IO))
tileAPI = getTileFromDB
  where getTileFromDB z x y = do
          mbt <- getTileBS (Z z) (X x) (Y y)
          case mbt of
            Nothing -> lift $ throwError err404
            Just b  -> return b
        getTileBS :: Z -> X -> Y -> MbtilesT (ExceptT ServantErr IO) (Maybe BL.ByteString)
        getTileBS = getTile

api :: Server TileServer
api = enter (transformToHandler "/Users/jcanero/Desktop/other.mbtiles") tileAPI

service :: Server TileServer
service = api

apiProxy :: Proxy TileServer
apiProxy = Proxy

middleware :: Application -> Application
middleware = logStdoutDev

app :: Application
app = serve apiProxy service

startAPI :: IO ()
startAPI = (run 3000 . middleware) app

main = startAPI
