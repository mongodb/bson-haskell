-- | UTF-8 String

{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving #-}

module Data.UString (
	UString, u,
	module Data.CompactString.UTF8
) where

import Data.CompactString.UTF8
import qualified Data.CompactString as S
import qualified Data.CompactString.Encodings as E
import Text.Read (Read(..))
import Data.Typeable
import Control.Applicative ((<$>))

deriving instance Typeable1 S.CompactString

deriving instance Typeable E.UTF8

instance Read CompactString where
	readPrec = pack <$> readPrec

type UString = CompactString
-- ^ UTF-8 String

u :: String -> UString
u = pack
