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



data Animal a = Cat a | Dog a deriving (Show)

instance Functor Animal where
  fmap f (Dog a) = Dog (f a)
  fmap f (Cat a) = Cat (f a)


instance Applicative Animal where
  pure (Dog x) = Dog x
  fs <*> xs = fmap fs xs
