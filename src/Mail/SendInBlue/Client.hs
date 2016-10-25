{-# LANGUAGE OverloadedStrings #-}

module Mail.SendInBlue.Client
    ( send
    ) where

import Data.ByteString.Conversion    (toByteString')
import Data.CaseInsensitive          (mk)
import Data.Text                     (Text)
import Control.Exception.Safe
import Control.Monad.Reader
import Lens.Micro
import Network.HTTP.Simple
import Mail.SendInBlue.Types.App
import Mail.SendInBlue.Types.Message
import Network.URI                   (uriToString)


-- | The 'send' function sends an email message and returns the
-- response text upon success.
send :: EmailMessage -> Client Text
send e = do
    env <- ask
    rsp <- liftIO $ httpJSON =<< mkReq env
    case getResponseBody rsp of
        s@Success{} -> return        $ s^.rsMessage
        s@Failure{} -> throw . Error $ s^.rfMessage
  where
    mkReq e = do
      initReq <- parseRequest $ uriToString id (e^.apiEndpoint) ""
      return
          . setRequestMethod "POST"
          . addRequestHeader (mk "api-key") (toByteString' $ e^.apiKey)
          $ initReq
