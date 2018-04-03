phoneBook :: PhoneBook
phoneBook =
  [("betty","555-2938")
   ,("bonnie","452-2928")
   ,("patsy","493-2928")
   ,("lucille","205-2928")
   ,("wendy","939-8282")
   ,("penny","853-2492")
   ]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (elem) (name, number) book

-- keyword list

type AssocList k v = [(k, v)]

get :: AssocList a b -> a -> b
get xs k = find (\(x, y)-> x == k) xs

-- Each node is either red or black.
--
-- The root is black. This rule is sometimes omitted. Since the root can always be
--  changed from red to black, but not necessarily vice versa, this rule has little
--  effect on analysis.
--
-- All leaves (NIL) are black.
--
-- If a node is red, then both its children are black.
--
-- Every path from a given node to any of its descendant NIL nodes contains the
-- same number of black nodes. Some definitions: the number of black nodes from the
--  root to a node is the node's black depth; the uniform number of black nodes in
--  all paths from root to the leaves is called the black-height of the red–black tree.[17]


data Animal a = Cat a | Dog a deriving (Show)

instance Functor Animal where
  fmap f (Dog a) = Dog (f a)
  fmap f (Cat a) = Cat (f a)


instance Applicative Animal where
  pure (Dog x) = Dog x
  fs <*> xs = fmap fs xs
