{-# LANGUAGE  OverloadedStrings #-}

module WhoIsKingCrypt  where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Data.List.Split
import Numeric


-- "Who is King" - a well-know mobile game in Asia market back to 2016. 
-- It uses TCP as base communication channel, the packet structure and Encrypt protocol described below

{- Packet structure

--------------------------------------
|   6 bytes header  |  n bytes data  |
--------------------------------------
| 09 00 02 00 05 00 | 64 61 74 61 00 |    
----------------- --------------------

Header
    first  byte indicates length of packet
    second byte is a flag byte: 00
    third  byte indicates order/index of packet
    fourth byte is a flag byte: ff
    fifth  byte indicates length of data
    sixth  byte is a flag byte: 00

Data
    n-1 bytes of data
    last byte is a flag byte: 00    
-}

{- Encrypt Procedure

Most packet use  plain base16 (hexString) encode, so decode is super easy.
Some special packets was sent as reversed-order-bytes after encode with base16. 
So if I use bytestring, just calculate header then append it to data.

somePacket = append (hexSerialize $ length m) m 
   where m = append "sample_msg" "\NUL"

-}

decToHex :: (Show a, Integral a) => a -> C.ByteString
decToHex c
    | c < 16 = C.append "0" . C.pack $ showHex c ""
    | otherwise = C.pack $ showHex c ""

-- For some special packets  which have reversed-order form of bytes
hexDeserialize :: C.ByteString -> Integer
hexDeserialize "" = 0
hexDeserialize b  = read . concat . ("0x":) . reverse . chunksOf 2 $ C.unpack b

-- Non-decode version of hexSerialize
hexSerialize' :: (Show a, Integral a) => a -> C.ByteString
hexSerialize' d = decToHex (d + 4) <> "0003ff" <> decToHex d <> "00"

hexSerialize :: (Show a, Integral a) => a -> C.ByteString
hexSerialize d = fst . decode $ decToHex (d + 4) <> "0003ff" <> decToHex d <> "00"

-- Login and Enter world is special case where the packet header must match first and second index in the packet stream

hexLoginSerialize :: (Show a, Integral a) => a -> C.ByteString                           
hexLoginSerialize d = fst . decode $ decToHex (d + 4) <> "0001ff" <> decToHex d <> "00"

hexEnterSerialize :: (Show a, Integral a) => a -> C.ByteString                             
hexEnterSerialize d = fst . decode $ decToHex (d + 4) <> "0002ff" <> decToHex d <>"00"                                   

