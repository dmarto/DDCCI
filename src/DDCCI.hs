--
-- Copyright (c) 2018, Martin Dimov <martin <at> dmarto <dot> com>
--    Licensed under the BSD-2-Clause "Simplified BSD License".
--

module DDCCI (
    openDevice
  , ddcciRead
  , ddcciReadCtrl
  , ddcciRawReadCtrl
  , ddcciWrite
  , ddcciWriteCtrl
  , ddcciSendCommand
  -- ~ , ddcciFindDevice  -- TODO
) where

import Data.Word

import Foreign
import Foreign.C

import System.Posix

import Control.Concurrent

foreign import ccall unsafe "i2c_read" c_i2c_read :: Fd -> CUInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "i2c_write" c_i2c_write :: Fd -> CUInt -> Ptr Word8 -> CInt -> IO CInt

openDevice :: String -> IO Fd
openDevice dev = openFd dev ReadWrite Nothing defaultFileFlags
-- error handling? closing?

-- magic values
ddcciAddr         = 0x37
ddcciFirstByte    = 0x51
ddcciSecondByte   = 0x80
ddcciRecieveXOR   = 0x50
ddcciWriteXOR     = (ddcciAddr `shiftL` 1)
ddcciWriteDelay   = 45000 -- TODO: figure out
-- TODO: find a way for control over the magic values

-- |
checksum :: Word8 -> [Word8] -> Word8
checksum start = foldl (xor) start

-- |
ddcciRead :: Fd -> Int -> IO [Word8]
ddcciRead device len = do
  ptr  <- mallocBytes 127 -- 127 is max message length
  err  <- c_i2c_read device (fromIntegral ddcciAddr) ptr (fromIntegral $ len + 3)
  -- TODO: error handle
  list <- peekArray (len + 2) ptr
  let (controlBytes, payload) = splitAt 2 list
  -- TODO: validate payload, what on fail
    -- ~ if ((list !! 0) == (ddcciAddr * 2))
    -- ~ && (((list !! 1) .&. ddcciSecondByte) \= 0)
    -- ~ && (((list !! 1) .&. (complementBit ddcciSecondByte)) <= len)
    -- ~ && (checksum list) == 0
    -- ~ then list
  return payload

-- |
ddcciReadCtrl :: Fd -> Word8 -> IO (Word8, Word8)
ddcciReadCtrl device ctrl = do
  payload <- ddcciRawReadCtrl device ctrl
  if -- TODO: refactor in more haskell acceptable way
    (payload !! 0 /= 0x02) || -- TODO: doc values
    (payload !! 2 /= ctrl)
  then return (255, 255)
    else do
      let cur = ((payload !! 6) * 256 + payload !! 7)
      let max = ((payload !! 4) * 256 + payload !! 5)
      return (cur, max)

-- |
ddcciRawReadCtrl :: Fd -> Word8 -> IO [Word8]
ddcciRawReadCtrl device ctrl =
  ddcciWrite device [0x01, ctrl] >> ddcciRead device 8 -- TODO: doc values

-- |
ddcciWrite :: Fd -> [Word8] -> IO CInt
ddcciWrite device bytes = do
  let buf = buildMessage bytes
  ptr <- newArray0 (0x00 :: Word8) buf -- null terminated buffer
  err <- c_i2c_write device (fromIntegral ddcciAddr) ptr (fromIntegral $ length buf)
  -- TODO: error handle
  threadDelay ddcciWriteDelay
  return err
  where
    buildMessage :: [Word8] -> [Word8]
    buildMessage bytes = bytes' ++ [checksum ddcciWriteXOR bytes'] where
      bytes' = ddcciFirstByte : (ddcciSecondByte .|. fromIntegral (length bytes)) : bytes

-- |
ddcciWriteCtrl :: Fd -> Word8 -> Word8 -> IO CInt
ddcciWriteCtrl device ctrl value =
  ddcciWrite device [0x03, ctrl, value `shiftR` 8, value .&. 255] -- TODO: doc values

-- |
ddcciSendCommand :: Fd -> Word8 -> IO CInt
ddcciSendCommand device byte = ddcciWrite device [byte]
