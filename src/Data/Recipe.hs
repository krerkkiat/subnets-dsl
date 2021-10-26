module Data.Recipe where

import Data.String
import Data.Bits
import Data.Word
import Text.Printf

import Data.Subnet

data Network = Network { netName :: String
                       , address :: String
                       }

data LinkType = Gateway | Offset Word32

data Interface = Interface { intName :: String
                           , network :: Network
                           , linkType :: LinkType
                           }

data Machine = Machine { name :: String
                       , interfaces :: [Interface]
                       }

instance Show Machine where
  show (Machine name interfaces) = (printf "set system host-name %s\n" name) ++ (unlines $ map show interfaces)

instance Show Interface where
  show (Interface name network Gateway) = unlines [ printf "set interfaces ethernet %s address %s/%d" name (inetNtoA $ lastUsableAddress subnet) (slashAddressToBitCount addr) :: String
                                                  , printf "set interfaces ethernet %s description %s" name (netName network) :: String
                                                  ]
                                          where subnet = SubnetSlash (address network) 0 ""
                                                addr = address network
  show (Interface name network (Offset n)) = unlines [ printf "set interfaces ethernet %s address %s/%d" name (inetNtoA $ firstUsableAddress subnet + n) (slashAddressToBitCount addr) :: String
                                                     , printf "set interfaces ethernet %s description %s" name (netName network) :: String
                                                     ]
                                             where subnet = SubnetSlash (address network) 0 ""
                                                   addr = address network

data Route = Route { toNet :: Network
                   , nextHop :: String
                   , distance :: Int
                   }

instance Show Route where
  show (Route net next d) = printf "set protocol static route %s next-hop %s distance '%s'" (address net) next (show d)

gatewayOf :: Network -> String
gatewayOf network = inetNtoA $ lastUsableAddress (SubnetSlash (address network) 0 "")

firstAddrOf :: Network -> String
firstAddrOf network = inetNtoA $ firstUsableAddress (SubnetSlash (address network) 0 "")
