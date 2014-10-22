import Record (render, Record)
import Topology (Topology(..), adjMatrix)
import Connections.Tree (treeG)
import Connections.Ring (ringG)
import Connections.Mash (mashG)
import Connections (generate, ConnectionsGen)
import Analysis (stats)
import System.Environment (getArgs)
import Cluster


data Variant = Variant Cluster ConnectionsGen

calcStats :: Variant -> Int -> [Record]
calcStats (Variant cluster conGen) levels =
    map stats adjs
    where connections = map (generate conGen) [1 .. levels]
          topologies = map (Topology cluster) connections
          adjs = map adjMatrix topologies


variants :: [(String, Variant)]
variants = [("lab1", Variant second treeG),
            ("lab2", Variant square ringG),
            ("lab3", Variant pyramid mashG)]

main :: IO ()
main = do
    [variantName, n'] <- getArgs
    let n = read n' :: Int
    case lookup variantName variants of
        Nothing -> putStrLn $ "Unknown variant: " ++ variantName
        Just variant -> do
            let recs = calcStats variant n
            putStrLn $ render recs
