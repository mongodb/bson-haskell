-- | A BSON document is a JSON-like object with a standard binary encoding defined at bsonspec.org. This implements version 1.0 of that spec.
-- Use the GHC language extension "OverloadedStrings" to automatically convert String literals to UString (UTF8)

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Data.Bson (
	UString,
	Document, Element(..), Value(..),
	label, value,
	lookup
) where

import Prelude hiding (lookup)
import Data.Bson.ObjectId (ObjectId)
import Data.Int
import qualified Data.CompactString.UTF8 as U
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()  -- for Show and Read instances of UTCTime
import Data.ByteString.Char8 (ByteString)
import Data.List (find)
import Text.Read (Read(..))
import Control.Applicative ((<$>), (<*>))

instance Read U.CompactString where
	readPrec = U.pack <$> readPrec

type UString = U.CompactString
-- ^ UTF-8 String

-- * Document structure

type Document = [Element]

data Element = Label := Value  deriving (Show, Read, Eq)

type Label = UString

data Value =
	  Float Double
	| String UString
	| Document Document
	| Array [Value]
	| Function ByteString
	| Binary ByteString
	| UUID ByteString
	| MD5 ByteString
	| UserDefined ByteString
	| ObjectId ObjectId
	| Bool Bool
	| UTC UTCTime
	| Null
	| Regex UString UString
	| JavascriptCode UString
	| Symbol UString
	| JavascriptClosure UString Document
	| Int32 Int32
	| MongoStamp Int64
	| Int64 Int64
	| MinKey
	| MaxKey
	deriving (Show, Read, Eq)

-- ** Document access

lookup :: Label -> Document -> Maybe Value
-- ^ Lookup value of named element in document
lookup k o = value <$> find ((k ==) . label) o

label :: Element -> Label
label (k := _) = k

value :: Element -> Value
value (_ := v) = v
