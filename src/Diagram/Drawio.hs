module Diagram.Drawio ( writeDrawio
                      ) where

import Text.XML
import Text.Hamlet.XML
import Prelude hiding (writeFile)
import Data.Text (Text, pack)
import qualified Data.Map as M

import Data.Subnet

-- This can be accomphished in many ways
-- draw.io native format is just a XML file that contains
-- jGraph's mxGraphModel.
--
-- We can also create svg and import that file into draw.io.
-- This implementation is attempting to output directly
-- to the native XML format.

mxGraphModelAttributes :: M.Map Name Text
mxGraphModelAttributes = M.fromList
  [ ("dx", "1884")
  , ("dy", "2389")
  , ("grid", "1")
  , ("gridSize", "10")
  , ("guides", "1")
  , ("tooltips", "1")
  , ("connect", "1")
  , ("arrows", "1")
  , ("fold", "1")
  , ("page", "1")
  , ("pageScale", "1.5")
  , ("pageWidth", "1169")
  , ("pageHeight", "826")
  , ("background", "none")
  , ("math", "0")
  , ("shadow", "0")
  ]

mxCellDefaultStyle :: String
mxCellDefaultStyle = "text;html=1;strokeColor=none;fillColor=none;align=center;verticalAlign=middle;whiteSpace=wrap;rounded=0;"

-- TODO Actually randomly generate the string.
genPrefix = "S7n_AQlkfD6dSjdseuGR"

data Cell = Cell { prefix :: String
                 , index :: Int
                 , subnet :: Subnet
                 } deriving (Show, Eq)

-- Wrap a subnet into a Cell data type.
prepareCell :: [Subnet] -> [Cell]
prepareCell subnets = [Cell p i s | (p, i, s) <- zip3 (repeat genPrefix) [1..] subnets]

-- FIXME or rather IMPOVE ME
-- There are duplication in the two cases.
writeDrawio :: [Subnet] -> IO ()
writeDrawio [] = writeFile def "subnets.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "mxGraphModel" mxGraphModelAttributes [xml|
                                    <root>
                                        <mxCell id=0>
                                        <mxCell id=1 parent=0>
                                        |]

writeDrawio subnets = writeFile def "subnets.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "mxGraphModel" mxGraphModelAttributes [xml|
                                    <root>
                                        <mxCell id=0>
                                        <mxCell id=1 parent=0>
                                        $forall cell <- (prepareCell subnets)
                                            ^{subnetNodes cell}
                                            |]


createId :: String -> Int -> String
createId p i = p ++ "-" ++ (show i)

-- Each node need an ID
-- See https://github.com/jgraph/mxgraph/blob/ff141aab158417bd866e2dfebd06c61d40773cd2/javascript/src/js/model/mxGraphModel.js#L727
-- Not sure how the 20 characters prefix is generated, so assume that
-- we can just random 20 characters as a prefix.
-- id format: <prefix>-<index>
subnetNodes :: Cell -> [Node]
subnetNodes (Cell p i s) = [xml|
<mxCell id=#{pack $ createId p i} value="<div>NET #{pack $ networkAddressAndSlashMask s}</div><div>GW #{pack $ inetNtoA (lastUsableAddress s)}</div>" style=#{pack mxCellDefaultStyle} vertex=1 parent=1>
    <mxGeometry width=150 height=30 as=geometry>
|]
