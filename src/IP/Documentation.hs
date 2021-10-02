module IP.Documentation
  ( csvPrintSubnet
  , csvPrintSubnets
  ) where

import Subnet

-- 1. Print network line with: network address, string "Network,
--    number of host expected, number of hosts available, netmask (dot notation),
--    net mask (slash notation), note
-- 2. Print first usable address
-- 3. Print a pipe with string "Usable Rage"
-- 4. Print last usable address
-- 5. print broadcast address
csvPrintSubnet :: Subnet -> IO ()
csvPrintSubnet subnet = do
  putStrLn $ part1 ++ ",Network," ++ part2 ++ "," ++ part3 ++ "," ++ part4 ++ ",/" ++ part5 ++ "," ++ note subnet
  putStrLn $ inetNtoA $ firstUsableAddress subnet
  putStrLn "|,Usable Range"
  putStrLn $ inetNtoA (lastUsableAddress subnet - 1)
  putStrLn $ inetNtoA (lastUsableAddress subnet) ++ ",Router"
  putStrLn $ inetNtoA (broadcastAddress subnet) ++ ",Broadcast"
  where part1 = networkAddress subnet
        part2 = show $ hosts subnet
        part3 = show $ availableHosts subnet
        part4 = netMaskBitsToA $ netMaskBits subnet
        part5 = show $ netMaskBits subnet

csvPrintSubnets :: [Subnet] -> IO ()
csvPrintSubnets = mapM_ csvPrintSubnet
