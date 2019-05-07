{-# LANGUAGE OverloadedStrings #-}

module L2Crypt where

import Data.ByteString

data RecvPacket = RecvPacket {
    _head :: Int,       -- 2 bytes
    _data :: ByteString -- the remaining bytes
} deriving (Show)

bs_to_byte = undefined

byte_to_bs = undefined

nameVerifyReq1 = "1b0056456427bb5af848de801377d6c8f91492210046da319342d4"
nameVerifyReq2 = "1b0056456427bb5af8486439aace6f7140ad2b98b9ff63882afbd7"

hexResponse = "090078a5854e2d560b"
