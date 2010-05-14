-- | Standard binary encoding of BSON documents, version 1.0. See bsonspec.org

module Data.Bson.Binary (
	encode, decode
) where

import Prelude hiding (length, concat)
import Data.Bson
import Data.Bson.ObjectId
import Data.Int
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.Char8 (ByteString, pack, length, concat)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, toChunks, length)
import qualified Data.CompactString.UTF8 as U
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Control.Applicative ((<$>), (<*>))

encode :: Document -> L.ByteString
-- ^ Binary representation of document
encode doc = runPut (putDocument doc)

decode :: L.ByteString -> Document
-- ^ Haskell representation of binary BSON document
decode bytes = runGet getDocument bytes

putElement :: Element -> Put
-- ^ Write binary representation of element
putElement (k := v) = case v of
	Float x				-> putTL 0x01 >> putDouble x
	String x				-> putTL 0x02 >> putString x
	Document x		-> putTL 0x03 >> putDocument x
	Array x				-> putTL 0x04 >> putArray x
	Function x			-> putTL 0x05 >> putBinary 0x01 x
	Binary x			-> putTL 0x05 >> putBinary 0x02 x
	UUID x				-> putTL 0x05 >> putBinary 0x03 x
	MD5 x				-> putTL 0x05 >> putBinary 0x05 x
	UserDefined x	-> putTL 0x05 >> putBinary 0x80 x
	ObjectId x			-> putTL 0x07 >> putObjectId x
	Bool x				-> putTL 0x08 >> putBool x
	UTC x				-> putTL 0x09 >> putUTC x
	Null					-> putTL 0x0A
	Regex x y			-> putTL 0x0B >> putCString x >> putCString y
	JavascriptCode x	-> putTL 0x0D >> putString x
	Symbol x			-> putTL 0x0E >> putString x
	JavascriptClosure x y	-> putTL 0x0F >> putClosure x y
	Int32 x				-> putTL 0x10 >> putInt32 x
	MongoStamp x	-> putTL 0x11 >> putInt64 x
	Int64 x				-> putTL 0x12 >> putInt64 x
	MinKey				-> putTL 0xFF
	MaxKey			-> putTL 0x7F
 where
	putTL t = putTag t >> putLabel k

getElement :: Get Element
-- ^ Read binary representation of Element
getElement = do
	t <- getTag
	k <- getLabel
	v <- case t of
		0x01 -> Float <$> getDouble
		0x02 -> String <$> getString
		0x03 -> Document <$> getDocument
		0x04 -> Array <$> getArray
		0x05 -> getBinary >>= \(s, x) -> case s of
			0x01 -> return (Function x)
			0x02 -> return (Binary x)
			0x03 -> return (UUID x)
			0x05 -> return (MD5 x)
			0x80 -> return (UserDefined x)
			_ -> fail $ "unknown Bson binary subtype " ++ show s
		0x07 -> ObjectId <$> getObjectId
		0x08 -> Bool <$> getBool
		0x09 -> UTC <$> getUTC
		0x0A -> return Null
		0x0B -> Regex <$> getCString <*> getCString
		0x0D -> JavascriptCode <$> getString
		0x0E -> Symbol <$> getString
		0x0F -> uncurry JavascriptClosure <$> getClosure
		0x10 -> Int32 <$> getInt32
		0x11 -> MongoStamp <$> getInt64
		0x12 -> Int64 <$> getInt64
		0xFF -> return MinKey
		0x7F -> return MaxKey
		_ -> fail $ "unknown Bson element type " ++ show t
	return (k := v)

putTag = putWord8
getTag = getWord8

putLabel = putCString
getLabel = getCString

putDouble = putFloat64le
getDouble = getFloat64le

putInt32 :: Int32 -> Put
putInt32 = putWord32le . fromIntegral

getInt32 :: Get Int32
getInt32 = fromIntegral <$> getWord32le

putInt64 :: Int64 -> Put
putInt64 = putWord64le . fromIntegral

getInt64 :: Get Int64
getInt64 = fromIntegral <$> getWord64le

putCString :: UString -> Put
putCString x = do
	putByteString (U.toByteString x)
	putWord8 0

getCString :: Get UString
getCString = U.fromByteString_ . concat . L.toChunks <$> getLazyByteStringNul

putString :: UString -> Put
putString x = let b = U.toByteString x in do
	putInt32 $ toEnum (length b + 1)
	putByteString b
	putWord8 0

getString :: Get UString
getString = do
	len <- subtract 1 <$> getInt32
	b <- getByteString (fromIntegral len)
	getWord8
	return (U.fromByteString_ b)

putDocument :: Document -> Put
putDocument es = let b = runPut (mapM_ putElement es) in do
	putInt32 $ (toEnum . fromEnum) (L.length b + 1)
	putLazyByteString b
	putWord8 0

getDocument :: Get Document
getDocument = do
	len <- subtract 1 <$> getInt32
	b <- getLazyByteString (fromIntegral len)
	getWord8
	return (runGet getElements b)
 where
	getElements = isEmpty >>= \done -> if done
		then return []
		else (:) <$> getElement <*> getElements

putArray :: [Value] -> Put
putArray vs = putDocument (zipWith f [0..] vs)
	where f i v = U.pack (show i) := v

getArray :: Get [Value]
getArray = map value <$> getDocument

type Subtype = Word8

putBinary :: Subtype -> ByteString -> Put
putBinary t x = do
	putInt32 $ toEnum (length x)
	putTag t
	putByteString x

getBinary :: Get (Subtype, ByteString)
getBinary = do
	len <- getInt32
	t <- getTag
	x <- getByteString (fromIntegral len)
	return (t, x)

putObjectId :: ObjectId -> Put
putObjectId (Oid x y) = putWord32be x >> putWord64be y

getObjectId :: Get ObjectId
getObjectId = Oid <$> getWord32be <*> getWord64be

putBool :: Bool -> Put
putBool x = putWord8 (if x then 1 else 0)

getBool :: Get Bool
getBool = (> 0) <$> getWord8

putUTC :: UTCTime -> Put
-- store milliseconds since Unix epoch
putUTC x = putInt64 $ truncate (utcTimeToPOSIXSeconds x * 1000)

getUTC :: Get UTCTime
-- stored as millisecinds since Unix epoch
getUTC = posixSecondsToUTCTime . (/ 1000) . realToFrac <$> getInt64

putClosure :: UString -> Document -> Put
putClosure x y = let b = runPut (putString x >> putDocument y) in do
	putInt32 $ (toEnum . fromEnum) (L.length b)
	putLazyByteString b

getClosure :: Get (UString, Document)
getClosure = do
	getInt32
	x <- getString
	y <- getDocument
	return (x, y)
