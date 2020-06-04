-- another way to implement Store
type Var = Int

newtype Store = Sto (Var -> Int)

initial :: Store
initial = Sto (\v -> 0)

value :: Store -> Var -> Int
value (Sto sto) v = sto v

update :: Store -> Var -> Int -> Store
update (Sto sto) v n
  = Sto (\w -> if v == w then n else sto w)

