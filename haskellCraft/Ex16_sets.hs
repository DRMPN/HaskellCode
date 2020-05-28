module Set
  ( Set,
    empty,              -- Set a
    sing,               -- a -> Set a
    memSet,             -- Ord a => Set a -> a -> Bool
    union, inter, diff, -- Ord a => Set a -> Set a -> Set a
    eqSet,              -- Eq a => Set a -> Set a -> Bool
    subSet,             -- Ord a => Set a -> Set a -> Bool
    makeSet,            -- Ord a => [a] -> Set a
    mapSet,             -- Ord b => (a -> b) -> Set a -> Set b
    filterSet,          -- (a -> Bool) -> Set a -> Set a
    foldSet,            -- (a -> a -> a) -> a -> Set a -> Set a
    showSet,            -- (a -> String) -> Set a -> String
    card                -- Set a -> Int
  ) where
