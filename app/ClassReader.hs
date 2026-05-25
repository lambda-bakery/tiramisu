{-# LANGUAGE ViewPatterns #-}

module ClassReader where

import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.Trans (lift)
import Data.Binary (Get, Word16, Word32, Word64, Word8)
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
  | FloatInfo Float
  | LongInfo Word64
  | DoubleInfo Double
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
  | AttributeInfoStackMapTable [StackMapFrame]
  | AttributeInfoExceptions [Word16]
  | AttributeInfoInnerClasses [(Word16, Word16, Word16, Word16)]
  | AttributeInfoEnclosingMethod Word16 Word16
  | AttributeInfoSynthetic
  | AttributeInfoSignature Word16
  | AttributeInfoLineNumberTable [(Word16, Word16)]
  | AttributeInfoSourceFile String
  | AttributeInfoSourceDebugExtension [Word8]
  | AttributeInfoLocalVariableTable [(Word16, Word16, Word16, Word16, Word16)]
  | AttributeInfoLocalVariableTypeTable [(Word16, Word16, Word16, Word16, Word16)]
  | AttributeInfoDeprecated
  deriving (Show)

data StackMapFrame
  = SameFrame Word8
  | SameLocals1StackItemFrame Word8 VerificationTypeInfo
  | SameLocals1StackItemFrameExtended Word16 VerificationTypeInfo
  | ChopFrame Word16 Word8
  | SameFrameExtended Word16
  deriving (Show)

data VerificationTypeInfo
  = TopVariable
  | IntegerVariable
  | FloatVariable
  | DoubleVariable
  | LongVariable
  | NullVariable
  | UninitializedThisVariable
  | ObjectVariable Word16
  | UninitializedVariable Word16
  deriving (Show)

data ClassReadError
  = InvalidMagic Word32
  | InvalidPoolEntryTag Word8
  | InvalidUtf8PoolIndex Word16
  | InvalidClassInfoPoolIdx Word16
  | InvalidVerificationTypeTag Word8
  | UnsupportedAttribute String
  deriving (Show)

getU1 :: ReadResult Word8
getU1 = lift BG.getWord8

getU2 :: ReadResult Word16
getU2 = lift BG.getWord16be

getU4 :: ReadResult Word32
getU4 = lift BG.getWord32be

getU8 :: ReadResult Word64
getU8 = lift BG.getWord64be

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

readConstantPool :: Word16 -> ReadResult [PoolEntry]
readConstantPool 0 = pure []
readConstantPool n = do
  (padded, wide) <- resolveEntry <$> (readPoolEntry =<< getU1)
  ((++) padded) <$> readConstantPool (if wide then n - 2 else n - 1)
  where
    resolveEntry :: PoolEntry -> ([PoolEntry], Bool)
    resolveEntry e@LongInfo {} = ([e, InvalidEntry], True)
    resolveEntry e@DoubleInfo {} = ([e, InvalidEntry], True)
    resolveEntry e = ([e], False)
    readPoolEntry :: Word8 -> ReadResult PoolEntry
    readPoolEntry = \case
      1 -> Utf8Info . T.unpack . TE.decodeUtf8 <$> (lift . BG.getByteString . fromIntegral =<< getU2)
      3 -> IntegerInfo <$> getU4
      4 -> FloatInfo . fromIntegral <$> getU4
      5 -> LongInfo . fromIntegral <$> getU8
      6 -> DoubleInfo . fromIntegral <$> getU8
      7 -> ClassInfo <$> getU2
      8 -> StringInfo <$> getU2
      9 -> FieldRefInfo <$> getU2 <*> getU2
      10 -> MethodRefInfo <$> getU2 <*> getU2
      11 -> InterfaceMethodInfo <$> getU2 <*> getU2
      12 -> NameAndTypeInfo <$> getU2 <*> getU2
      tag -> throwError $ InvalidPoolEntryTag tag

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
    matchAttribute = \case
      "ConstantValue" -> AttributeInfoConstantValue <$> getU2
      "Code" -> uncurry5 AttributeInfoCode <$> readCode
        where
          readCode :: ReadResult (Word16, Word16, [Word8], [(Word16, Word16, Word16, Word16)], [AttributeInfo])
          readCode = do
            maxStack <- getU2
            maxLocals <- getU2
            codeLen <- fromIntegral <$> getU4
            code <- unpack <$> (lift . BG.getByteString $ codeLen)
            exnTable <- readExceptionTable =<< getU2
            attributes <- readAttributes pool =<< getU2
            pure (maxStack, maxLocals, code, exnTable, attributes)
          readExceptionTable :: Word16 -> ReadResult [(Word16, Word16, Word16, Word16)]
          readExceptionTable n = replicateM (fromIntegral n) readException
          readException :: ReadResult (Word16, Word16, Word16, Word16)
          readException = (,,,) <$> getU2 <*> getU2 <*> getU2 <*> getU2
          uncurry5 f (a, b, c, d, e) = f a b c d e
      "StackMapTable" -> AttributeInfoStackMapTable <$> (readStackMapFrames =<< getU2)
        where
          readStackMapFrames :: Word16 -> ReadResult [StackMapFrame]
          readStackMapFrames n = replicateM (fromIntegral n) (readStackMapFrame =<< getU1)
          readStackMapFrame :: Word8 -> ReadResult StackMapFrame
          readStackMapFrame frameType
            | frameType <= 63 = pure $ SameFrame frameType
            | frameType <= 127 = SameLocals1StackItemFrame frameType <$> (readVerificationTypeInfo =<< getU1)
            | frameType == 247 = SameLocals1StackItemFrameExtended <$> getU2 <*> (readVerificationTypeInfo =<< getU1)
            | frameType <= 250 = ChopFrame <$> getU2 <*> pure (251 - frameType)
            | frameType == 251 = SameFrameExtended <$> getU2
            | otherwise = error "TODO"
          readVerificationTypeInfo :: Word8 -> ReadResult VerificationTypeInfo
          readVerificationTypeInfo = \case
            0 -> pure TopVariable
            1 -> pure IntegerVariable
            2 -> pure FloatVariable
            3 -> pure DoubleVariable
            4 -> pure LongVariable
            5 -> pure NullVariable
            6 -> pure UninitializedThisVariable
            7 -> ObjectVariable <$> getU2
            8 -> UninitializedVariable <$> getU2
            tag -> throwError $ InvalidVerificationTypeTag tag
      "Exceptions" -> AttributeInfoExceptions <$> (readExceptions =<< getU2)
        where
          readExceptions :: Word16 -> ReadResult [Word16]
          readExceptions n = replicateM (fromIntegral n) getU2
      "InnerClasses" -> AttributeInfoInnerClasses <$> (readInnerClasses =<< getU2)
        where
          readInnerClasses :: Word16 -> ReadResult [(Word16, Word16, Word16, Word16)]
          readInnerClasses n = replicateM (fromIntegral n) readInnerClass
          readInnerClass :: ReadResult (Word16, Word16, Word16, Word16)
          readInnerClass = (,,,) <$> getU2 <*> getU2 <*> getU2 <*> getU2
      "EnclosingMethod" -> AttributeInfoEnclosingMethod <$> getU2 <*> getU2
      "Synthetic" -> pure AttributeInfoSynthetic
      "Signature" -> AttributeInfoSignature <$> getU2
      "SourceFile" -> AttributeInfoSourceFile <$> (getUtf8Info pool =<< getU2)
      "SourceDebugExtension" -> AttributeInfoSourceDebugExtension <$> BS.unpack <$> lift BG.getRemainingLazyByteString
      "LineNumberTable" -> AttributeInfoLineNumberTable <$> (readLineNumberTable =<< getU2)
        where
          readLineNumberTable :: Word16 -> ReadResult [(Word16, Word16)]
          readLineNumberTable n = replicateM (fromIntegral n) readLineNumber
          readLineNumber :: ReadResult (Word16, Word16)
          readLineNumber = (,) <$> getU2 <*> getU2
      "LocalVariableTable" -> AttributeInfoLocalVariableTable <$> (readLocalVariableTable =<< getU2)
        where
          readLocalVariableTable :: Word16 -> ReadResult [(Word16, Word16, Word16, Word16, Word16)]
          readLocalVariableTable n = replicateM (fromIntegral n) readLocalVariable
          readLocalVariable :: ReadResult (Word16, Word16, Word16, Word16, Word16)
          readLocalVariable = (,,,,) <$> getU2 <*> getU2 <*> getU2 <*> getU2 <*> getU2
      "LocalVariableTypeTable" -> AttributeInfoLocalVariableTypeTable <$> (readLocalVariableTypeTable =<< getU2)
        where
          readLocalVariableTypeTable :: Word16 -> ReadResult [(Word16, Word16, Word16, Word16, Word16)]
          readLocalVariableTypeTable n = replicateM (fromIntegral n) readLocalVariableType
          readLocalVariableType :: ReadResult (Word16, Word16, Word16, Word16, Word16)
          readLocalVariableType = (,,,,) <$> getU2 <*> getU2 <*> getU2 <*> getU2 <*> getU2
      "Deprecated" -> pure AttributeInfoDeprecated
      "RuntimeVisibleAnnotations" -> undefined
      "RuntimeInvisibleAnnotations" -> undefined
      "RuntimeVisibleParameterAnnotations" -> undefined
      "RuntimeInvisibleParameterAnnotations" -> undefined
      "AnnotationDefault" -> undefined
      "BootstrapMethods" -> undefined
      -- TODO: returning UnsupportedAttribute here gives a weird behavior with BG.isolate lol
      name -> error name
