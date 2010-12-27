-- | UTF-8 String

{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable #-}

module Data.UString (
	UString, u,
	module Data.CompactString.UTF8,
	module Data.CompactString
) where

import Data.CompactString ()  -- Show and IsString instances
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


{- Authors: Tony Hannan <tony@10gen.com>
   Copyright 2010 10gen Inc.
   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License. -}
