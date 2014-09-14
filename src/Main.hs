import Record
import Topology
import Connections.Tree (treeG)
import Connections
import Cluster.Second

test levels = adjMatrix topology
    where treeC = generate treeG levels
          topology = Topology second treeC
          adj = adjMatrix topology

main = print $ test 2
