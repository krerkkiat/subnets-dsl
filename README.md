# subnets-dsl

## Example

``` haskell
import Subnet
import IP.Documentation

subnets = [ Subnet "172.18.128.0" 20 2000 "Wireless"
          , Subnet "172.18.144.0" 22 600 "Technical"
          ]
          
main :: IO ()
main = printCsvSubnets subnets
```

The source code above should generate a CSV content that can be imported into
Google's Sheets or Microsoft's Excel for the following result.

![imported result](./.github/imported-result.png)
