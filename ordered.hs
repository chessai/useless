import Data.Foldable

data Ordered a = Empty | Decreasing a | Increasing a

inc :: Ordered a -> Bool
inc (Decreasing _) = False
inc _              = True

dec :: Ordered a -> Bool
dec (Increasing _) = False
dec _              = True

increasing :: (Foldable t, Ord a) => t a -> Bool
increasing = inc . foldl' go Empty where
  go Empty y = Increasing y
  go (Decreasing x) _ = Decreasing x
  go (Increasing x) y
    | x <= y = Increasing y
    | otherwise = Decreasing y

decreasing :: (Foldable t, Ord a) => t a -> Bool
decreasing = dec . foldl' go Empty where
  go Empty y = Decreasing y
  go (Increasing x) _ = Increasing x
  go (Decreasing x) y
    | x >= y = Decreasing y
    | otherwise = Increasing y

main :: IO ()
main = do
  let xs = [5,4] :: [Int]
  print $ decreasing xs
