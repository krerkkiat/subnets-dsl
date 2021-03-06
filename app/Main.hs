module Main where

import Data.Subnet
import Documentation.BradonS
import Diagram.Drawio

subnets = [ Subnet "172.18.128.0" 20 2000 "Wireless"
          , SubnetSlash "172.18.144.0/22" 600 "Technical"
          ]

main :: IO ()
main = do
  printCsvSubnets subnets
  writeDrawio subnets
