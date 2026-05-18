{-# LANGUAGE ViewPatterns #-}

module ClassReader where

import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.Trans (lift)
import Data.Binary (Get, Word16, Word32, Word8)
import Data.Binary.Get (runGet)
import Data.Binary.Get qualified as BG
import Data.ByteString (unpack)
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

type ReadResult = ExceptT ClassReadError Get

data ClassFile = ClassFile
  { classFileMinorVersion :: Word16,
    classFileMajorVersion :: Word16,
    classFileConstantPool :: ConstantPool,
    -- TODO: properly interpret access flags
    classFileAccessFlags :: Word16,
    classFileThisClass :: String,
    classFileSuperClass :: String,
    classFileInterfaces :: [String],
    classFileFields :: [FieldInfo],
    classFileMethods :: [MethodInfo],
    classFileAttributes :: [AttributeInfo]
  }
  deriving (Show)

type ConstantPool = [PoolEntry]

data PoolEntry
  = InvalidEntry
  | Utf8Info String
  | IntegerInfo Word32
  | FloatInfo Word32
  | LongInfo Word32 Word32
  | DoubleInfo Word32 Word32
  | ClassInfo Word16
  | StringInfo Word16
  | FieldRefInfo Word16 Word16
  | MethodRefInfo Word16 Word16
  | InterfaceMethodInfo Word16 Word16
  | NameAndTypeInfo Word16 Word16
  deriving (Show)

data FieldInfo = FieldInfo
  { fieldInfoAccessFlags :: Word16,
    fieldInfoName :: String,
    fieldInfoDescriptor :: String,
    fieldInfoAttributes :: [AttributeInfo]
  }
  deriving (Show)

data MethodInfo = MethodInfo
  { methodInfoAccessFlags :: Word16,
    methodInfoName :: String,
    methodInfoDescriptor :: String,
    methodInfoAttributes :: [AttributeInfo]
  }
  deriving (Show)

data AttributeInfo
  = AttributeInfoConstantValue Word16
  | AttributeInfoCode Word16 Word16 [Word8] [(Word16, Word16, Word16, Word16)] [AttributeInfo]
  | AttributeInfoExceptions [Word16]
  | AttributeInfoLineNumberTable [(Word16, Word16)]
  | AttributeInfoSourceFile String
  deriving (Show)

data ClassReadError
  = InvalidMagic Word32
  | InvalidPoolEntryTag Word8
  | InvalidUtf8PoolIndex Word16
  | InvalidClassInfoPoolIdx Word16
  | UnsupportedAttribute String
  deriving (Show)

getU1 :: ReadResult Word8
getU1 = lift BG.getWord8

getU2 :: ReadResult Word16
getU2 = lift BG.getWord16be

getU4 :: ReadResult Word32
getU4 = lift BG.getWord32be

runClassReader :: BS.ByteString -> Either ClassReadError ClassFile
runClassReader = (runGet . runExceptT) readClassFile

readClassFile :: ReadResult ClassFile
readClassFile = do
  (minorVersion, majorVersion, poolSize) <- readClassHeader =<< getU4
  constantPool <- readConstantPool (poolSize - 1)
  accessFlags <- getU2
  thisClass <- getClassInfo constantPool =<< getU2
  superClass <- getClassInfo constantPool =<< getU2
  interfaces <- readInterfaces constantPool =<< getU2
  fields <- readFields constantPool =<< getU2
  methods <- readMethods constantPool =<< getU2
  attributes <- readAttributes constantPool =<< getU2
  pure $
    ClassFile
      minorVersion
      majorVersion
      constantPool
      accessFlags
      thisClass
      superClass
      interfaces
      fields
      methods
      attributes

readClassHeader :: Word32 -> ReadResult (Word16, Word16, Word16)
readClassHeader 0xCAFEBABE = (,,) <$> getU2 <*> getU2 <*> getU2
readClassHeader magic = throwError $ InvalidMagic magic

-- TODO: must have a better way to do this
readConstantPool :: Word16 -> ReadResult [PoolEntry]
readConstantPool 0 = pure []
readConstantPool n = do
  entry <- readPoolEntry =<< getU1
  resolveEntry entry n
  where
    resolveEntry :: PoolEntry -> Word16 -> ReadResult [PoolEntry]
    resolveEntry e@DoubleInfo {} n' = ((:) e) <$> ((:) InvalidEntry) <$> readConstantPool (n' - 1)
    resolveEntry e@LongInfo {} n' = ((:) e) <$> ((:) InvalidEntry) <$> readConstantPool (n' - 1)
    resolveEntry e n' = ((:) e) <$> readConstantPool (n' - 1)

    readPoolEntry :: Word8 -> ReadResult PoolEntry
    readPoolEntry 1 = Utf8Info . T.unpack . TE.decodeUtf8 <$> (lift . BG.getByteString . fromIntegral =<< getU2)
    readPoolEntry 3 = IntegerInfo <$> getU4
    readPoolEntry 4 = FloatInfo <$> getU4
    readPoolEntry 5 = LongInfo <$> getU4 <*> getU4
    readPoolEntry 6 = DoubleInfo <$> getU4 <*> getU4
    readPoolEntry 7 = ClassInfo <$> getU2
    readPoolEntry 8 = StringInfo <$> getU2
    readPoolEntry 9 = FieldRefInfo <$> getU2 <*> getU2
    readPoolEntry 10 = MethodRefInfo <$> getU2 <*> getU2
    readPoolEntry 11 = InterfaceMethodInfo <$> getU2 <*> getU2
    readPoolEntry 12 = NameAndTypeInfo <$> getU2 <*> getU2
    readPoolEntry tag = throwError $ InvalidPoolEntryTag tag

readInterfaces :: ConstantPool -> Word16 -> ReadResult [String]
readInterfaces pool n = replicateM (fromIntegral n) (getClassInfo pool =<< getU2)

readFields :: ConstantPool -> Word16 -> ReadResult [FieldInfo]
readFields pool n = replicateM (fromIntegral n) readFieldInfo
  where
    readFieldInfo :: ReadResult FieldInfo
    readFieldInfo = do
      accessFlags <- getU2
      name <- getUtf8Info pool =<< getU2
      descriptor <- getUtf8Info pool =<< getU2
      attributes <- readAttributes pool =<< getU2
      pure $ FieldInfo accessFlags name descriptor attributes

readMethods :: ConstantPool -> Word16 -> ReadResult [MethodInfo]
readMethods pool n = replicateM (fromIntegral n) readMethodInfo
  where
    readMethodInfo :: ReadResult MethodInfo
    readMethodInfo = do
      accessFlags <- getU2
      methodName <- getUtf8Info pool =<< getU2
      methodDescriptor <- getUtf8Info pool =<< getU2
      methodAttributes <- readAttributes pool =<< getU2
      pure $ MethodInfo accessFlags methodName methodDescriptor methodAttributes

resolveIdx :: Word16 -> Word16
resolveIdx 0 = error "what"
resolveIdx n = n - 1

getUtf8Info :: ConstantPool -> Word16 -> ReadResult String
getUtf8Info pool (resolveIdx -> idx) = case pool !! fromIntegral idx of
  Utf8Info s -> pure s
  _ -> throwError $ InvalidUtf8PoolIndex idx

getClassInfo :: ConstantPool -> Word16 -> ReadResult String
getClassInfo pool (resolveIdx -> idx) = case pool !! fromIntegral idx of
  ClassInfo idx' -> getUtf8Info pool idx'
  _ -> throwError $ InvalidClassInfoPoolIdx idx

readAttributes :: ConstantPool -> Word16 -> ReadResult [AttributeInfo]
readAttributes pool n = replicateM (fromIntegral n) $ readAttributeInfo pool

readAttributeInfo :: ConstantPool -> ReadResult AttributeInfo
readAttributeInfo pool = do
  attributeName <- getUtf8Info pool =<< getU2
  attributeLen <- fromIntegral <$> getU4
  attributeInfo <- lift $ BG.isolate attributeLen $ runExceptT (matchAttribute attributeName)
  liftEither attributeInfo
  where
    matchAttribute :: String -> ReadResult AttributeInfo
    matchAttribute "Code" = do
      maxStack <- getU2
      maxLocals <- getU2
      codeLen <- fromIntegral <$> getU4
      code <- unpack <$> (lift . BG.getByteString $ codeLen)
      exnTable <- readExnTable =<< getU2
      attributes <- readAttributes pool =<< getU2
      pure $ AttributeInfoCode maxStack maxLocals code exnTable attributes
      where
        readExnTable :: Word16 -> ReadResult [(Word16, Word16, Word16, Word16)]
        readExnTable n = replicateM (fromIntegral n) exn
        exn :: ReadResult (Word16, Word16, Word16, Word16)
        exn = (,,,) <$> getU2 <*> getU2 <*> getU2 <*> getU2
    matchAttribute "ConstantValue" = AttributeInfoConstantValue <$> getU2
    matchAttribute "Exceptions" = AttributeInfoExceptions <$> ((\n -> replicateM (fromIntegral n) getU2) =<< getU2)
    matchAttribute "LineNumberTable" = AttributeInfoLineNumberTable <$> ((\n -> replicateM (fromIntegral n) readLineNumberTable) =<< getU2)
      where
        readLineNumberTable :: ReadResult (Word16, Word16)
        readLineNumberTable = (,) <$> getU2 <*> getU2
    matchAttribute "SourceFile" = AttributeInfoSourceFile <$> (getUtf8Info pool =<< getU2)
    -- TODO: returning UnsupportedAttribute here gives a weird behavior with BG.isolate lol
    matchAttribute name = error name
