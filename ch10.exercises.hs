-- 10.20
-- addOne x = x + 1
-- addTen x = x + 10
-- switchMap addOne addTen [1,2,3,4] # => [2,12,4,14]

switchMap f g xs = map step $ zip xs (take (length xs) [1..])
    where step (x, index) = if odd index then f x else g x

-- 10.21
-- split [1,2,3,4,5]      # => ([1,3,5], [2,4])
-- merge ([1,3,5], [2,4]) # => [1,2,3,4,5]

split xs = foldr step ([], []) $ zip xs (take (length xs) [1..])
    where step (x, index) (oddList, evenList) = if odd index
                                                then (x:oddList, evenList)
                                                else (oddList, x:evenList)
merge (xs, ys) = addLast xs $ foldr step [] $ zip xs ys
    where step (x,y) zs = [x,y] ++ zs
          addLast xs zs = if (length xs == length zs)
                          then zs
                          else zs ++ [last xs]