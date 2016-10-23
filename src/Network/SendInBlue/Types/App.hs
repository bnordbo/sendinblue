{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.SendInBlue.Types.App
    ( -- * API key for v2 authentication
      ApiKey (..)
      -- * Client type
    , Client
    , runClient
      -- * Environment setup
    , Env
    , mkEnv
    , apiKey
    , apiEndpoint
      -- * Error type
    , Error  (..)
    ) where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.ByteString.Builder    (byteString)
import Data.ByteString.Conversion (ToByteString(..))
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8)
import Lens.Micro.TH              (makeLenses)
import Network.URI                (URI)
import Network.URI.Static         (uri)


-- | The SendInBlue API key used for v2 authentication.
newtype ApiKey = ApiKey Text

instance ToByteString ApiKey where
  builder (ApiKey text) = byteString $ encodeUtf8 text

-- | The environment of a SendInBlue client.
data Env = Env
    { _apiKey      :: !ApiKey  -- ^ API key for authenticating requests.
    , _apiEndpoint :: !URI     -- ^ HTTP end-point to contact.
    }

-- | Initialise the client environment using the specified API key and
-- the default API end-point.
mkEnv :: ApiKey -> Env
mkEnv key = Env
    { _apiKey      = key
    , _apiEndpoint = [uri|https://api.sendinblue.com/v2.0/email|]
    }

newtype Error = Error Text deriving Show

instance Exception Error

-- | The Client monad.
--
-- This is a newtype around some internal environment. Prior to
-- running via 'runClient', it must be initialized using 'mkEnv'.
newtype Client a = Client (ReaderT Env IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
             , MonadThrow
             )

-- | Execute the 'Client' monad using the environment in 'Env'.
runClient :: MonadIO m => Env -> Client a -> m a
runClient e (Client c) = liftIO $ runReaderT c e

makeLenses ''Env
