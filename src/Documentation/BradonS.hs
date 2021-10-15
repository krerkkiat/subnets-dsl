module Documentation.BradonS
  ( printCsvSubnet
  , printCsvSubnets
  ) where

import Data.Subnet

-- 1. Print network line with: network address, string "Network,
--    number of host expected, number of hosts available, netmask (dot notation),
--    net mask (slash notation), note
-- 2. Print first usable address
-- 3. Print a pipe with string "Usable Rage"
-- 4. Print last usable address
-- 5. print broadcast address
printCsvSubnet :: Subnet -> IO ()
printCsvSubnet (SubnetSlash addrS hosts note) = printCsvSubnet (Subnet addr bitCount hosts note)
  where addr = slashAddressToAddress addrS
        bitCount = slashAddressToBitCount addrS
printCsvSubnet subnet@(Subnet addr bitCount hosts note) = do
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

-- TODO Double check that the subnets cover a specific range.
printCsvSubnets :: [Subnet] -> IO ()
printCsvSubnets = mapM_ printCsvSubnet
