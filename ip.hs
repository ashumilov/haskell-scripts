#!/usr/bin/env stack
-- stack script --resolver lts-22.11 --package ip --package text
{-# LANGUAGE OverloadedStrings #-}

import           Data.Bits
import           Data.Maybe
import           Data.Word
import qualified Net.IPv4   as IPv4
import qualified Net.IPv6   as IPv6

convert ip =
    let ipv6 = fromMaybe IPv6.any (IPv6.decode ip)
        (word32,_,_,_) = IPv6.toWord32s ipv6
        [o1,o2,o3,o4] = octets word32
        ipv4 = IPv4.fromOctets o1 o2 o3 o4
    in show $ IPv4.encode ipv4

main = do
    let ipv6 = "2601:447:c980:2c21:857d:b9c3:bde1:102"
    putStrLn $ convert ipv6

octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
