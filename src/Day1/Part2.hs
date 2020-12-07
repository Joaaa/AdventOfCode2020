module Day1.Part2 where

import Day1.Part1 ( inputs )

-- >>> solution
-- 292093004
solution = (\(a, b, c) -> a * b * c) . head $ [(a, b, c) | a <- inputs, b <- inputs, c <- inputs, a + b + c == 2020]