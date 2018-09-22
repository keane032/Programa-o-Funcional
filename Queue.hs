module Queue (Queue, enqueue, dequeue, front, empty, isEmpty, makeQueue, size) where
data Queue a = Q [a] deriving (Show)

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (xs ++ [x])
frqueue x (Q xs) = Q ([x] ++ xs)

dequeue :: Queue a -> Queue a
dequeue (Q (_:xs)) = Q xs
dequeue _          = error "Queue.dequeue: empty queue"

front :: Queue a -> a
front (Q (x:_)) = x
front _         = error "Queue.front: empty queue"

empty :: Queue a
empty = Q []

isEmpty :: Queue a -> Bool
isEmpty (Q []) = True
isEmpty (Q _ ) = False

makeQueue :: [a] -> Queue a
makeQueue xs = foldl (flip enqueue) empty xs

size :: Queue a -> Int
size s | isEmpty s = 0
			 | otherwise = 1 + size (dequeue s)

makeList x = [1..x]

pula x (Q xs) | x > 0 = pula (x-1) (enqueue fist tail)
              | otherwise = (Q xs)
            	  where
					fist = front (Q xs) 
					tail = dequeue (Q xs)

remove x (Q []) = (Q [])					
remove x (Q xs) = if x /= (front (Q xs)) then frqueue (front ( Q xs)) (remove x (dequeue (Q xs))) else (remove x (dequeue (Q xs))) 

removeInd y x (Q []) = (Q [])
removeInd y x (Q xs) | y == x = (dequeue (Q xs))
				     |otherwise = frqueue (front (Q xs)) (removeInd (y+1) (x) (dequeue (Q xs)))

simula x (Q xs) = (removeInd (1) (x) (pula (x-1) (Q xs)))

