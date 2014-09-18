import Record (printRecords)
import Topology (Topology(..), adjMatrix)
import Connections.Tree (treeG)
import Connections (generate, ConnectionsGen)
import Analysis (stats)
import System.Environment (getArgs)
import Cluster


data Variant = Variant Cluster ConnectionsGen

calcStats (Variant cluster conGen) levels =
    map stats adjs
    where connections = map (generate conGen) [1 .. levels]
          topologies = map (Topology cluster) connections
          adjs = map adjMatrix topologies


lab2 = Variant second treeG


main = do
    [n'] <- getArgs
    let n = read n' :: Int
    let recs = calcStats lab2 n
    printRecords recs
