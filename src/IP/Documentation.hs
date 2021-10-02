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
csvPrintSubnet (SubnetSlash addrS hosts note) = csvPrintSubnet (Subnet addr bitCount hosts note)
  where addr = slashAddressToAddress addrS
        bitCount = slashAddressToBitCount addrS
csvPrintSubnet subnet@(Subnet addr bitCount hosts note) = do
  putStrLn $ addr ++ ",Network," ++ part2 ++ "," ++ part3 ++ "," ++ part4 ++ ",/" ++ part5 ++ "," ++ note
  putStrLn $ inetNtoA $ firstUsableAddress subnet
  putStrLn "|,Usable Range"
  putStrLn $ inetNtoA (lastUsableAddress subnet - 1)
  putStrLn $ inetNtoA (lastUsableAddress subnet) ++ ",Router"
  putStrLn $ inetNtoA (broadcastAddress subnet) ++ ",Broadcast"
  where part2 = show $ hosts
        part3 = show $ availableHosts subnet
        part4 = netMaskBitsToA $ bitCount
        part5 = show $ bitCount

csvPrintSubnets :: [Subnet] -> IO ()
csvPrintSubnets = mapM_ csvPrintSubnet
