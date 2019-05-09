{-# LANGUAGE OverloadedStrings #-}

module L2Crypt where

import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Char8
import qualified Data.ByteArray as ByteArray
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
keystatic :: ByteArray.Bytes
keystatic =  ByteArray.pack [0xc8, 0x27, 0x93, 0x01, 0xa1, 0x6c, 0x31, 0x97]

key :: ByteArray.Bytes
key = ByteArray.pack [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77] <>  keystatic

nameVerifyReq1 :: ByteString
-- Request Name: weaverzero (10chars)
-- 56456427bb5af848: 8 same bytes from 2 requests
nameVerifyReq1 = "1b0056456427bb5af848de801377d6c8f91492210046da319342d4"
nameVerifyReq2 = "1b0056456427bb5af8486439aace6f7140ad2b98b9ff63882afbd7"

hexResponse :: ByteString
hexResponse = "090078a5854e2d560b"
