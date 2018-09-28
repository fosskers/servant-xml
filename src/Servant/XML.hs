{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Servant.XML
-- Copyright : (c) Colin Woodbury, 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Servant support for XML.
--
-- Types with a `ToXml` instance will be automatically marshalled into XML
-- and successfully returned by Servant endpoints.
-- Types with a `FromXml` instance can be decoded from request bodies.
--
-- In implementing these typeclass instances, you can use the primitives
-- found in the /xmlbf/ library.

module Servant.XML where

import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import qualified Network.HTTP.Media as M
import           Servant.API
import           Xmlbf
import           Xmlbf.Xeno

---

-- | The /application\/xml/ Content-Type. To be used in Servant endpoints like:
--
-- @
-- data Foo = ...
--
-- instance ToXml Foo where
--   toXml foo = ...
--
-- type API = ...
--   :\<|\> "foo" :> Get '[XML] Foo
--   :\<|\> "foo" :> "update" :> ReqBody '[XML] Foo :> PostAccepted '[JSON] ()
-- @
data XML

instance Accept XML where
  contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance ToXml a => MimeRender XML a where
  mimeRender _ = toLazyByteString . encode . toXml

instance FromXml a => MimeUnrender XML a where
  mimeUnrender _ bs = nodes (toStrict bs) >>= runParser fromXml
