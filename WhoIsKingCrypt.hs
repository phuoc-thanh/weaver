{-# LANGUAGE  OverloadedStrings #-}

module Serializer where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Data.List.Split
import Numeric


-- Who is King, a famous mobile game in Asia market back to 2016, and it has several re-skin games.
-- This module is a complete reverve-bytes of tcp packet of Who is King and it's re-skin.

-- Packet structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- data        = x bytes (x-1 byte data + 1 flag byte) contains the data

decToHex :: (Show a, Integral a) => a -> C.ByteString
decToHex c
    | c < 16 = C.append "0" . C.pack $ showHex c ""
    | otherwise = C.pack $ showHex c ""

-- For some special data which is reverse order of bytes
hexDeserialize :: C.ByteString -> Integer
hexDeserialize "" = 0
hexDeserialize b  = read . concat . ("0x":) . reverse . chunksOf 2 $ C.unpack b

-- Non-decode version
hexSerialize' :: (Show a, Integral a) => a -> C.ByteString
hexSerialize' d = C.append (decToHex (d + 4))
                $ C.append "0003ff"
                $ C.append (decToHex d) "00"

hexSerialize :: (Show a, Integral a) => a -> C.ByteString
hexSerialize d = fst . decode $ C.append (decToHex (d + 4))
                              $ C.append "0003ff"
                              $ C.append (decToHex d) "00"

hexLoginSerialize :: (Show a, Integral a) => a -> C.ByteString                           
hexLoginSerialize d = fst . decode $ C.append (decToHex (d + 4))
                                   $ C.append "0001ff"
                                   $ C.append (decToHex d) "00"

hexEnterSerialize :: (Show a, Integral a) => a -> C.ByteString                             
hexEnterSerialize d = fst . decode $ C.append (decToHex (d + 4))
                                   $ C.append "0002ff"
                                   $ C.append (decToHex d) "00"                                   

