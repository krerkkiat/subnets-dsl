module Subnet
    ( Subnet (..)
    , splitOn
    , inetNtoA
    , inetAtoN
    , netMaskBitsToA
    , firstUsableAddress
    , lastUsableAddress
    , possibleAddresses
    , broadcastAddress
    , availableHosts
    ) where

import Data.Bits
import Data.Word

data Subnet = Subnet { networkAddress :: String
                     , netMaskBits :: Int
                     , hosts :: Int
                     , note :: String
                     } deriving (Show, Read, Eq)

-- Taken from SO
-- See https://stackoverflow.com/a/4981265/10163723
splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s''
        where (w, s'') = break p s'

inetOp :: Word32 -> (Word32, Int) -> Word32
inetOp old pair = uncurry shiftL pair .|. old

inetAtoN :: String -> Word32
inetAtoN addr = foldl inetOp 0 $ zip (map read parts) [24, 16, 8, 0]
                where parts = splitOn (=='.') addr

inetNtoA :: Word32 -> String
inetNtoA addr = part1 ++ "." ++ part2 ++ "." ++ part3 ++ "." ++ part4
                where part1 = show $ shiftR addr 24 .&. 255
                      part2 = show $ shiftR addr 16 .&. 255
                      part3 = show $ shiftR addr 8 .&. 255
                      part4 = show $ addr .&. 255

-- 0xffffff << (32 - n)
netMaskOp :: Int -> Int
netMaskOp n = shiftL 4294967295 (32 - n)

netMaskBitsToA :: Int -> String
netMaskBitsToA n = part1 ++ "." ++ part2 ++ "." ++ part3 ++ "." ++ part4
                where part1 = show $ shiftR (netMaskOp n) 24 .&. 255
                      part2 = show $ shiftR (netMaskOp n) 16 .&. 255
                      part3 = show $ shiftR (netMaskOp n) 8 .&. 255
                      part4 = show $ netMaskOp n .&. 255

firstUsableAddress :: Subnet -> Word32
firstUsableAddress subnet = inetAtoN (networkAddress subnet) + 1

lastUsableAddress :: Subnet -> Word32
lastUsableAddress subnet = broadcastAddress subnet - 1

-- Total possible addresses (ignoring broadcast and network).
possibleAddresses :: Int -> Word32
possibleAddresses mask = shiftL 1 (32 - mask)

broadcastAddress :: Subnet -> Word32
broadcastAddress subnet = addr + possibleAddresses (netMaskBits subnet) - 1
                        where addr =  inetAtoN $ networkAddress subnet

availableHosts :: Subnet -> Word32
availableHosts subnet = 1 + (lastUsableAddress subnet - firstUsableAddress subnet)


