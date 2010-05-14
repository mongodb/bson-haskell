
module Data.Bson.ObjectId (
	ObjectId(..), genObjectId,
	timestamp
) where

import Control.Applicative ((<$>))
import Data.Word
import Data.Bits (shift, (.|.))
import Data.ByteString.Char8 (pack)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Numeric (readHex)
import Network.BSD (getHostName)
import System.Posix.Process (getProcessID)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

data ObjectId = Oid Word32 Word64  deriving (Show, Read, Eq, Ord)
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
