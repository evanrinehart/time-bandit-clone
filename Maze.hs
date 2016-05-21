module Maze where

-- a pacman maze consists of linear spaces connected at nodes.
-- there two kinds of linear spaces, horizontal and vertical, and the
-- two endpoints are of two distinct classes, two classes for each of the
-- horizontal and vertical segments. nodes have up to 4 "ports" to connect
-- nodes at, namely the four kinds of segment end points. objects position
-- in the maze consists of either a node or any point on a segment.

type R = Double
type R = Double
type SegNo = Int
type NodeNo = Int
data Orient = Horiz | Vert
data Exit = L | R | T | B
data Segment = Segment
  { segNo :: SegNo
  , segLen :: R
  , segOrient :: Orient
  , segGhostOnly :: Bool
  , segA :: NodeNo
  , segB :: NodeNo }
data Node = Node
  { nodeNo :: NodeNo
  , nodeL :: Maybe SegNo
  , nodeR :: Maybe SegNo
  , nodeT :: Maybe SegNo
  , nodeB :: Maybe SegNo }
data MazePtr = AtNode NodeNo | OnSeg SegNo R
data Maze = Maze
  { mazeNodes :: Map NodeNo Node
  , mazeSegs :: Map SegNo Segment
  , mazeStart :: MazePtr
  , mazePills :: [MazePtr]
  , mazeDots :: [MazePtr]
  }
data MazeVel = MazeVel R Exit
type Object = (MazePtr, MazeVel, R)

motion :: Maze -> Object -> Delta -> Object
motion _ x _ = x

overlapping :: Maze -> Object -> Object -> Bool
overlapping _ _ _ = False

-- solve for collision time between two objects
collidesBefore :: Maze -> Object -> Object -> Delta -> Maybe Delta
collidesBefore _ _ _ _ = Nothing
