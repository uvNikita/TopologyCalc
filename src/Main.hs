import Record
import Topology
import Connections.Tree (treeG)
import Connections
import Cluster.Second
import Analysis
import System.Environment (getArgs)

test levels = stats adj
    where treeC = generate treeG levels
          topology = Topology second treeC
          adj = adjMatrix topology


main = do
    [n'] <- getArgs
    let n = read n' :: Int
    let recs = map test [1 .. n]
    printRecords recs
