-- 10.20
-- addOne x = x + 1
-- addTen x = x + 10
-- switchMap addOne addTen [1,2,3,4] # => [2,12,4,14]

switchMap f g xs = map step $ zip xs (take (length xs) [1..])
    where step (x, index) = if odd index then f x else g x
