{-# LANGUAGE OverloadedStrings #-}

module L2Crypt where

import Data.ByteString (ByteString)
import Data.ByteString.Base16
-- import Data.ByteString.Char8
import Data.Bits
import Data.ByteArray (Bytes)
import qualified Data.ByteArray as BArr
import Data.Word


data RecvPacket = RecvPacket {
    _head :: Int,       -- 2 bytes
    _data :: ByteString -- the remaining bytes
} deriving (Show)

{- Packet Structure

----------------------
| Header | Data |
----------------------
| 06 00  | 64 61 74 61

first byte indicates the length of packet
2nd   byte is a flag byte: 00

-}

{- Cryptic Protocol

1. Login Crypt

Login Server use more complex crypts than game server, but still base on Blowfish Cipher

2. Game Crypt

Server will pick a Blowfish key from a pool (usually 20keys from mobius/acis source)

Crypt Key has a length of 16 bytes (128bit)

-}

-- Blowfish key with the last 8 bytes are static
-- The first 8 bytes are random-ed from 0 - 255 Int
keystatic :: Bytes
keystatic =  BArr.pack [0xc8, 0x27, 0x93, 0x01, 0xa1, 0x6c, 0x31, 0x97]

key :: Bytes
key = BArr.pack [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77] <>  keystatic

zbits = 0x00 :: Word8

-- Checksum function for incoming/outgoing packets
-- Assume that chksum func works with zero offset by default
chksum :: Bytes -> Bool
chksum bytes
    | len     <    5 = False -- check if array size is greater than 4?
    | len .&. 3 /= 0 = False -- check if array size is multiple of 4?
    | otherwise = (xor_fold . fmap chkw $ split_every 4 arr1) == (chkw arr2)
    where (arr1, arr2) = splitAt (len - 4) words 
          words        = BArr.unpack bytes
          len          = BArr.length bytes

-- Check a block of 4 bytes data. If length of input isn't eq to 4, return False
-- Interested: xor 0 x == x
chkw :: [Word8] -> Word8
chkw bs@(x:y:z:w:s)
    | length bs /= 4 = 0x00
    | otherwise      = cx .|. cy .|. cz .|. cw where
      cx = x .&. 0xff
      cy = y `shiftL` (0x08 .&. 0xff00)
      cz = z `shiftL` (0x10 .&. 0xff0000)
      cw = w `shiftL` (0x18 .&. 0xff000000)

-- folding with xor return the only non-repeat element in a list
-- ex: xor_fold [1,2,3,4,3,2,1] should return 4
-- not sure it is applied in this case
xor_fold xs = foldr (xor) 0 xs 

-- split a list every n elems
split_every _ [] = []
split_every n xs = fs : split_every n rs where (fs, rs) = splitAt n xs

nameVerifyReq1 :: ByteString
-- Request Name: weaverzero (10chars)
-- 56456427bb5af848: 8 same bytes from 2 requests
nameVerifyReq1 = "1b0056456427bb5af848de801377d6c8f91492210046da319342d4"
nameVerifyReq2 = "1b0056456427bb5af8486439aace6f7140ad2b98b9ff63882afbd7"

hexResponse :: ByteString
hexResponse = "090078a5854e2d560b"
