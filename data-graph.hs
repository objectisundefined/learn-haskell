import Data.Graph

graph = buildG (1, 6) [(1, 2), (1, 3), (2, 4), (5, 6)]

main :: IO ()
main = do
  print graph -- array (1,6) [(1,[3,2]),(2,[4]),(3,[]),(4,[]),(5,[6]),(6,[])]
  print $ vertices graph -- [1,2,3,4,5,6]
  print $ edges graph -- [(1,3),(1,2),(2,4),(5,6)]
  print $ edges $ transposeG graph -- [(2,1),(3,1),(4,2),(6,5)]

  print $ outdegree graph -- array (1,6) [(1,2),(2,1),(3,0),(4,0),(5,1),(6,0)]
  print $ indegree graph -- array (1,6) [(1,0),(2,1),(3,1),(4,1),(5,0),(6,1)]

  print $ topSort graph -- [5,6,1,2,4,3]
  print $ reachable graph 1 -- [1,3,2,4]

  print $ path graph 1 4 -- True
  print $ path graph 1 5 -- Flase

  print $ components graph
  -- [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []}]},Node {rootLabel = 3, subForest = []}]},Node {rootLabel = 5, subForest = [Node {rootLabel = 6, subForest = []}]}]

  print $ scc graph
  -- [Node {rootLabel = 6, subForest = []},Node {rootLabel = 5, subForest = []},Node {rootLabel = 4, subForest = []},Node {rootLabel = 3, subForest = []},Node {rootLabel = 2, subForest = []},Node {rootLabel = 1, subForest = []}]

  print $ bcc graph
  -- [Node {rootLabel = [1,3], subForest = []},Node {rootLabel = [1,2], subForest = [Node {rootLabel = [2,4], subForest = []}]},Node {rootLabel = [5,6], subForest = []}]


  print $ dff graph
  -- [Node {rootLabel = 1, subForest = [Node {rootLabel = 3, subForest = []},Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []}]}]},Node {rootLabel = 5, subForest = [Node {rootLabel = 6, subForest = []}]}]

  print $ dfs graph [2]
  -- [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []}]}]


