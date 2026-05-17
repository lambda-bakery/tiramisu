module ClassReader where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (lift)
import Data.Binary (Get, Word16, Word32, Word8)
import Data.Binary.Get (runGet)
import Data.Binary.Get qualified as BG
import Data.ByteString.Lazy qualified as BS

type ReadResult = ExceptT ClassReadError Get

data ClassFile

data ClassReadError

getU1 :: ReadResult Word8
getU1 = lift BG.getWord8

getU2 :: ReadResult Word16
getU2 = lift BG.getWord16be

getU4 :: ReadResult Word32
getU4 = lift BG.getWord32be

runClassReader :: BS.ByteString -> Either ClassReadError ClassFile
runClassReader = (runGet . runExceptT) readClassFile

readClassFile :: ReadResult ClassFile
readClassFile = undefined
