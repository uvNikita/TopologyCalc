import Record (printRecords)
import Topology (Topology(..), adjMatrix)
import Connections.Tree (treeG)
import Connections (generate)
import Analysis (stats)
import System.Environment (getArgs)
import Cluster

test levels = stats adj
    where treeC = generate treeG levels
          topology = Topology single treeC
          adj = adjMatrix topology


main = do
    [n'] <- getArgs
    let n = read n' :: Int
    let recs = map test [1 .. n]
    printRecords recs
