{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.Client
-- Description: HTTP REST client
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.Client where

import BasicPrelude
import Data.Default (Default (def))
import Data.Text (append, pack, unpack)
import Kipu.Config
-- import Network.Connection (TLSSettings (TLSSettings))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import Network.TLS
  ( clientHooks,
    clientSupported,
    credentialLoadX509,
    defaultParamsClient,
    onCertificateRequest,
    onServerCertificate,
    supportedCiphers,
  )
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)

-- Certificate handling

-- mkMngr :: Text -> Text -> Text -> IO Manager
-- mkMngr hostName crtFile keyFile = do
--   creds <-
--     either error Just
--       `fmap` credentialLoadX509
--         (unpack crtFile)
--         (unpack keyFile)
--   let hooks =
--         def
--           { onCertificateRequest = \_ -> return creds,
--             -- FIXME: it bypass server validation
--             onServerCertificate = \_ _ _ _ -> return []
--           }
--       clientParams =
--         (defaultParamsClient (unpack hostName) "")
--           { clientHooks = hooks,
--             clientSupported =
--               def
--                 { supportedCiphers = ciphersuite_strong
--                 }
--           }
--       tlsSettings = TLSSettings clientParams
--   newManager $ mkManagerSettings tlsSettings Nothing

run :: (Config -> ClientM a) -> IO (Either Text a)
run f = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e -> return $ Left $ append "Error: " $ pack $ show e
    Right cfg -> do
      manager' <- newTlsManager
      --      manager' <- case crtPath cfg of
      --        Just cert -> case keyPath cfg of
      --          Just ckey -> mkMngr (url cfg) cert ckey
      --          Nothing -> newTlsManager
      --        Nothing -> newTlsManager
      u <- parseBaseUrl $ unpack (url cfg)
      let g = f cfg
      res <- runClientM g (mkClientEnv manager' u)
      case res of
        Left err -> return $ Left $ append "Error: " $ pack $ show err
        Right r -> return $ Right r
