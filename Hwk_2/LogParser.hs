module LogParser where

parseMessage str = parseEntry (words str) where
  parseEntry ("E":code:ts:msg) = LogMessage (Error (read code :: Integer)) (read ts :: Integer) (unwords msg)
  parseEntry ("W":ts:msg) = LogMessage Warning (read ts :: Integer) (unwords msg)
  parseEntry ("I":ts:msg) = LogMessage Info (read ts :: Integer) (unwords msg)
  parseEntry chars = Unknown (unwords chars)

-- Extracts timestamp from log message
timestamp (LogMessage t ts str) = ts

-- Inserts a log message into log message tree. Ignores invalid messages.
insert (Unknown msg) tree = tree
insert msg (Leaf) = Node Leaf msg Leaf
insert msg (Node left node right)
  | (timestamp msg) > (timestamp node) = Node left node (insert msg right)
  | (timestamp msg) < (timestamp node) = Node (insert msg left) node right
  | otherwise = Node Leaf msg Leaf

-- insert Leaf x = Leaf x Leaf
-- insert Leaf
-- insert (Unknown msg) tree = tree
-- insert msg (Leaf) = Node Leaf msg Leaf
-- insert msg (Node left node right)
--   | (timestamp msg) > (timestamp node) = Node left node (insert msg right)
--   | (timestamp msg) < (timestamp node) = Node (insert msg left) node right
--   | otherwise = Node Leaf msg Leaf


-- Builds a tree from a list of messages.
build [] = Leaf
build xs = foldl (\tree msg -> insert msg tree) Leaf xs

-- Return list of messages by ordered by timestamp
inOrder Leaf = []
inOrder (Node x y z) = (inOrder x) ++ [y] ++ (inOrder z)

whatWentWrong msgs = filter (\msg -> timestamp msg >= 50) (inOrder (build msgs))
