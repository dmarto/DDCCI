module DDCCI (
    openDevice
  , ddcci_read
  , ddcci_write
  , ddcci_command
  , ddcci_raw_readctrl
  , ddcci_readctrl
  , ddcci_writectrl
  -- ~ , find_ddcci_device  -- TODO
) where

import Foreign
import Foreign.C

import System.Posix

import Control.Concurrent

import Data.Char -- can drop probably?

foreign import ccall unsafe "i2c_read" c_i2c_read :: Fd -> CUInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall unsafe "i2c_write" c_i2c_write :: Fd -> CUInt -> Ptr CUChar -> CInt -> IO CInt

openDevice :: String -> IO Fd
openDevice dev = openFd dev ReadWrite Nothing defaultFileFlags
-- error handling? closing?

-- magic values
ddcci_addr          = 0x37
ddcci_first_byte    = 0x51
ddcci_second_byte   = 0x80
ddcci_recieve_xor   = 0x50
ddcci_write_xor     = (ddcci_addr `shiftL` 1)
ddcci_write_delay   = 45000 -- 40k works as well, check the speck again
-- TODO: find a way for control over the magic values

-- |
checksum :: Int -> [Int] -> Int
checksum start = foldl (xor) start

-- |
ddcci_write :: Fd -> [Int] -> IO CInt
ddcci_write device bytes = do
  let buf = build_message bytes
  ptr <- newArray0 (castCharToCUChar '\0') buf
  err <- c_i2c_write device (fromIntegral ddcci_addr) ptr (fromIntegral (length buf))
  -- TODO: error handle
  threadDelay ddcci_write_delay
  return err
  where
    build_message :: [Int] -> [CUChar]
    build_message bytes = map castCharToCUChar (map chr (bytes' ++ [checksum ddcci_write_xor bytes'])) where
      bytes' = (ddcci_first_byte : ((ddcci_second_byte .|. (length bytes)) : bytes))

-- |
ddcci_read :: Fd -> Int -> IO [CUChar]
ddcci_read device len = do
  ptr  <- mallocBytes 127 -- max message length
  err  <- c_i2c_read device (fromIntegral ddcci_addr) ptr (fromIntegral (len + 3))
  -- TODO: error handle
  list <- peekArray (len + 2) ptr
  let (control_bytes, payload) = splitAt 2 list
  -- TODO: validate payload, what on fail
    -- ~ if ((list !! 0) == (ddcci_addr * 2))
    -- ~ && (((list !! 1) .&. ddcci_second_byte) \= 0)
    -- ~ && (((list !! 1) .&. (complementBit ddcci_second_byte)) <= len)
    -- ~ && (checksum list) == 0
    -- ~ then list
  return payload

-- |
ddcci_command :: Fd -> Int -> IO CInt
ddcci_command device byte = ddcci_write device [byte]

-- |
ddcci_raw_readctrl :: Fd -> Int -> IO [CUChar]
ddcci_raw_readctrl device ctrl =
  ddcci_write device [0x01, ctrl] >> ddcci_read device 8

-- |
ddcci_readctrl :: Fd -> Int -> IO (CUChar, CUChar)
ddcci_readctrl device ctrl = do
  payload <- ddcci_raw_readctrl device ctrl
  if
    (payload !! 0 /= 0x02) ||
    (payload !! 2 /= castCharToCUChar (chr ctrl))
  then return (255, 255)
    else do
      let cur = ((payload !! 6) * 256 + payload !! 7)
      let max = ((payload !! 4) * 256 + payload !! 5)
      return (cur, max)

-- |
ddcci_writectrl :: Fd -> Int -> Int -> IO CInt
ddcci_writectrl device ctrl value =
  ddcci_write device [0x03, ctrl, value `shiftR` 8, value .&. 255]

