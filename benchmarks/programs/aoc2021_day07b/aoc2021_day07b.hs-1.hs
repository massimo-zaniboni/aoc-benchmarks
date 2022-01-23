
-- author: f-a

import Data.List as L

main = main2

main1 = sol1 <$> input >>= print
main2 = sol2 <$> input >>= print

input :: IO [Int]
input = read . ('[':) . (++"]") <$> getContents

cost :: (Int -> Int) -> [Int] -> Int
cost d ps = sum $ map d ps

median :: [Int] -> Int
median ps = last . take (quot (length ps) 2) $ L.sort ps

mean :: [Int] -> Int
mean ps = quot (sum ps) (length ps)

-- sol1 <$> input    344138
-- sol2 <$> input    94862124
sol1, sol2 :: [Int] -> Int
sol1 ps = cost (abs . subtract (median ps)) ps
sol2 ps = cost (\p -> sum [1..(abs $ (mean ps - p))]) ps
