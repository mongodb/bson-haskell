-- | A BSON document is a JSON-like object with a standard binary encoding defined at bsonspec.org. This implements version 1.0 of that spec.
-- Use the GHC language extension "OverloadedStrings" to automatically convert String literals to UString (UTF8)

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveDataTypeable, RankNTypes, OverlappingInstances, IncoherentInstances #-}

module Data.Bson (
	UString,
	Document, Element(..), (=:), look, lookup,
	Label,
	Value(..), Val(..), fval,
	Array,
	Function(..), Binary(..), UUID(..), MD5(..), UserDefined(..),
	Regex(..), Javascript(..), Symbol(..), MongoStamp(..), MinMaxKey(..),
	ObjectId(..), timestamp, genObjectId
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Typeable
import Data.Int
import Data.Word
import Data.UString (UString, u, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Data.Time.Format ()  -- for Show and Read instances of UTCTime
import Data.List (find)
import Data.Bits (shift, (.|.))
import Data.ByteString.Char8 (ByteString, pack)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Numeric (readHex)
import Network.BSD (getHostName)
import System.Posix.Process (getProcessID)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

roundTo :: (RealFrac a) => a -> a -> a
-- ^ Round second number to nearest multiple of first number. Eg: roundTo (1/1000) 0.12345 = 0.123
roundTo mult n = fromIntegral (round (n / mult)) * mult

-- * Document

type Document = [Element]

look :: Label -> Document -> Maybe Value
-- ^ Value of field in document, or Nothing if field not found
look k doc = value <$> find ((k ==) . label) doc

lookup :: (Val v) => Label -> Document -> Maybe v
-- ^ Lookup field in document and cast to expected type. Nothing if field not found or value not of expected type.
lookup k doc = typed =<< look k doc

infix 0 :=, =:

data Element = (:=) {label :: Label, value :: Value}  deriving (Typeable, Eq)

(=:) :: (Val v) => Label -> v -> Element
k =: v = k := val v

instance Show Element where
	showsPrec d (k := v) = showParen (d > 0) $ showString (' ' : unpack k) . showString ": " . showsPrec 1 v

type Label = UString

-- * Value

data Value =
	Float Double |
	String UString |
	Doc Document |
	Array Array |
	Fun Function |
	Bin Binary |
	Uuid UUID |
	Md5 MD5 |
	UserDef UserDefined |
	ObjId ObjectId |
	Bool Bool |
	UTC UTCTime |
	Null |
	RegEx Regex |
	JavaScr Javascript |
	Sym Symbol |
	Int32 Int32 |
	Int64 Int64 |
	Stamp MongoStamp |
	MinMax MinMaxKey
	deriving (Typeable, Eq)

fval :: (forall a . (Val a) => a -> b) -> Value -> b
fval f v = case v of
	Float x -> f x
	String x -> f x
	Doc x -> f x
	Array x -> f x
	Fun x -> f x
	Bin x -> f x
	Uuid x -> f x
	Md5 x -> f x
	UserDef x -> f x
	ObjId x -> f x
	Bool x -> f x
	UTC x -> f x
	Null -> f ()
	RegEx x -> f x
	JavaScr x -> f x
	Sym x -> f x
	Int32 x -> f x
	Int64 x -> f x
	Stamp x -> f x
	MinMax x -> f x

instance Show Value where
	showsPrec d v = fval (showsPrec d) v

type Array = [Value]

-- | Haskell types of this class correspond to BSON value types
class (Typeable a, Show a, Eq a) => Val a where
	val :: a -> Value
	typed :: Value -> Maybe a

instance Val Double where
	val = Float
	typed (Float x) = Just x
	typed (Int32 x) = Just (fromIntegral x)
	typed (Int64 x) = Just (fromIntegral x)
	typed _ = Nothing

instance Val Float where
	val = Float . realToFrac
	typed (Float x) = Just (realToFrac x)
	typed (Int32 x) = Just (fromIntegral x)
	typed (Int64 x) = Just (fromIntegral x)
	typed _ = Nothing

instance Val UString where
	val = String
	typed (String x) = Just x
	typed (Sym (Symbol x)) = Just x
	typed _ = Nothing

instance Val String where
	val = String . u
	typed (String x) = Just (unpack x)
	typed (Sym (Symbol x)) = Just (unpack x)
	typed _ = Nothing

instance Val Document where
	val = Doc
	typed (Doc x) = Just x
	typed _ = Nothing

instance Val Array where
	val = Array
	typed (Array x) = Just x
	typed _ = Nothing

instance (Val a) => Val [a] where
	val = Array . map val
	typed (Array x) = mapM typed x
	typed _ = Nothing

instance Val Function where
	val = Fun
	typed (Fun x) = Just x
	typed _ = Nothing

instance Val Binary where
	val = Bin
	typed (Bin x) = Just x
	typed _ = Nothing

instance Val UUID where
	val = Uuid
	typed (Uuid x) = Just x
	typed _ = Nothing

instance Val MD5 where
	val = Md5
	typed (Md5 x) = Just x
	typed _ = Nothing

instance Val UserDefined where
	val = UserDef
	typed (UserDef x) = Just x
	typed _ = Nothing

instance Val ObjectId where
	val = ObjId
	typed (ObjId x) = Just x
	typed _ = Nothing

instance Val Bool where
	val = Bool
	typed (Bool x) = Just x
	typed _ = Nothing

instance Val UTCTime where
	val = UTC . posixSecondsToUTCTime . roundTo (1/1000) . utcTimeToPOSIXSeconds
	typed (UTC x) = Just x
	typed _ = Nothing

instance Val POSIXTime where
	val = UTC . posixSecondsToUTCTime . roundTo (1/1000)
	typed (UTC x) = Just (utcTimeToPOSIXSeconds x)
	typed _ = Nothing

instance Val () where
	val () = Null
	typed Null = Just ()
	typed _ = Nothing

instance Val Regex where
	val = RegEx
	typed (RegEx x) = Just x
	typed _ = Nothing

instance Val Javascript where
	val = JavaScr
	typed (JavaScr x) = Just x
	typed _ = Nothing

instance Val Symbol where
	val = Sym
	typed (Sym x) = Just x
	typed (String x) = Just (Symbol x)
	typed _ = Nothing

instance Val Int32 where
	val = Int32
	typed (Int32 x) = Just x
	typed (Int64 x) = Just (fromIntegral x)
	typed (Float x) = Just (round x)
	typed _ = Nothing

instance Val Int64 where
	val = Int64
	typed (Int64 x) = Just x
	typed (Int32 x) = Just (fromIntegral x)
	typed (Float x) = Just (round x)
	typed _ = Nothing

instance Val Int where
	val n = if fromIntegral (minBound :: Int32) <= n && n <= fromIntegral (maxBound :: Int32)
		then Int32 (fromIntegral n)
		else Int64 (fromIntegral n)
	typed (Int32 x) = Just (fromIntegral x)
	typed (Int64 x) = Just (fromIntegral x)
	typed (Float x) = Just (round x)
	typed _ = Nothing

instance Val Integer where
	val n = if fromIntegral (minBound :: Int32) <= n && n <= fromIntegral (maxBound :: Int32)
		then Int32 (fromIntegral n)
		else Int64 (fromIntegral n)
	typed (Int32 x) = Just (fromIntegral x)
	typed (Int64 x) = Just (fromIntegral x)
	typed (Float x) = Just (round x)
	typed _ = Nothing

instance Val MongoStamp where
	val = Stamp
	typed (Stamp x) = Just x
	typed _ = Nothing

instance Val MinMaxKey where
	val = MinMax
	typed (MinMax x) = Just x
	typed _ = Nothing

-- * Binary types

newtype Function = Function ByteString  deriving (Typeable, Show, Read, Eq)

newtype Binary = Binary ByteString  deriving (Typeable, Show, Read, Eq)

newtype UUID = UUID ByteString  deriving (Typeable, Show, Read, Eq)

newtype MD5 = MD5 ByteString  deriving (Typeable, Show, Read, Eq)

newtype UserDefined = UserDefined ByteString  deriving (Typeable, Show, Read, Eq)

-- * Regex

data Regex = Regex UString UString  deriving (Typeable, Show, Read, Eq)

-- * Javascript

data Javascript = Javascript UString (Maybe Document)  deriving (Typeable, Show, Eq)

-- * Symbol

newtype Symbol = Symbol UString  deriving (Typeable, Show, Read, Eq)

-- * MongoStamp

newtype MongoStamp = MongoStamp Int64  deriving (Typeable, Show, Read, Eq)

-- * MinMax

data MinMaxKey = MinKey | MaxKey  deriving (Typeable, Show, Read, Eq)

-- * ObjectId

data ObjectId = Oid Word32 Word64  deriving (Typeable, Show, Read, Eq, Ord)
-- ^ A BSON ObjectID is a 12-byte value consisting of a 4-byte timestamp (seconds since epoch), a 3-byte machine id, a 2-byte process id, and a 3-byte counter. Note that the timestamp and counter fields must be stored big endian unlike the rest of BSON. This is because they are compared byte-by-byte and we want to ensure a mostly increasing order.

timestamp :: ObjectId -> UTCTime
timestamp (Oid time _) = posixSecondsToUTCTime (fromIntegral time)

genObjectId :: IO ObjectId
-- ^ Create a fresh ObjectId
genObjectId = do
	time <- truncate <$> getPOSIXTime
	pid <- fromIntegral <$> getProcessID
	inc <- nextCount
	return $ Oid time (composite machineId pid inc)
 where
	machineId :: Word24
	machineId = unsafePerformIO (fst . head . readHex . take 6 . md5sum . pack <$> getHostName)
 	{-# NOINLINE machineId #-}
 	counter :: IORef Word24
 	counter = unsafePerformIO (newIORef 0)
 	{-# NOINLINE counter #-}
 	nextCount :: IO Word24
 	nextCount = atomicModifyIORef counter $ \n -> (wrap24 (n + 1), n)
 	composite :: Word24 -> Word16 -> Word24 -> Word64
 	composite mid pid inc = fromIntegral mid `shift` 40 .|. fromIntegral pid `shift` 24 .|. fromIntegral inc

type Word24 = Word32
-- ^ low 3 bytes only, high byte must be zero

wrap24 :: Word24 -> Word24
wrap24 n = n `mod` 0x1000000
