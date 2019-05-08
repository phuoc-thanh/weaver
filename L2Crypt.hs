{-# LANGUAGE OverloadedStrings #-}

module L2Crypt where

import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Char8

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


nameVerifyReq1 :: ByteString
-- Request Name: weaverzero (10chars)
-- 56456427bb5af848: 8 same bytes from 2 requests
nameVerifyReq1 = "1b0056456427bb5af848de801377d6c8f91492210046da319342d4"
nameVerifyReq2 = "1b0056456427bb5af8486439aace6f7140ad2b98b9ff63882afbd7"

hexResponse :: ByteString
hexResponse = "090078a5854e2d560b"
