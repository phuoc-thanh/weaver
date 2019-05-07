{-# LANGUAGE  OverloadedStrings #-}

module WhoIsKingCrypt  where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Data.List.Split
import Numeric


-- "Who is King" - a well-know mobile game in Asia market back to 2016, and it has several re-skin games.
-- I've done some work on it and have many good feeling when I've succesful "crack" the communication protocol between the server and client of that game.
-- This modudle is the crypt part of a bigger botting system written by me, maybe it help someone want to know how online game work.

-- Who's King use TCP as base communication channel, just sniff it and see. So analyze packet is the first step to "win" the game.
-- If you know Packet Structure and Packet Encrypt Protocol, you got the gold mine.

-- Packet structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- data        = x bytes (x-1 byte data + 1 flag byte) contains the data

-- Encrypt Protocol
-- Most packet use base16 (hexString) encode, so decode is super easy
-- Some special packets was sent as reversed-order-bytes after encode 
-- So if I use bytestring, just calculate header then append it to data

-- Ex: somePacket = append (hexSerialize $ length m) m 
--        where m = append "sample_msg" "\NUL"

decToHex :: (Show a, Integral a) => a -> C.ByteString
decToHex c
    | c < 16 = C.append "0" . C.pack $ showHex c ""
    | otherwise = C.pack $ showHex c ""

-- Some special packets  which have reversed-order form of bytes
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

-- Login and Enter world is special case where the packet header must match first and second index in the packet stream

hexLoginSerialize :: (Show a, Integral a) => a -> C.ByteString                           
hexLoginSerialize d = fst . decode $ C.append (decToHex (d + 4))
                                   $ C.append "0001ff"
                                   $ C.append (decToHex d) "00"

hexEnterSerialize :: (Show a, Integral a) => a -> C.ByteString                             
hexEnterSerialize d = fst . decode $ C.append (decToHex (d + 4))
                                   $ C.append "0002ff"
                                   $ C.append (decToHex d) "00"                                   

