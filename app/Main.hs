module Main where

import Subnet
import IP.Documentation

subnet = Subnet "192.168.0.0" 24 0 "Home network"

main :: IO ()
main = csvPrintSubnet subnet
