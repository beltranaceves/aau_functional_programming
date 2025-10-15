-- linked list datatype
data LinkedList a = Empty | Node a (LinkedList a)
  deriving (Show, Eq)

-- converts regular list to LinkedList
toLinkedList :: [a] -> LinkedList a
toLinkedList [] = Empty
toLinkedList (x:xs) = Node x (toLinkedList xs)

-- removes th k-th element counting from the end using a fast/slow pointers strategy
linkedListRemoveKthFromEnd :: Int -> LinkedList a -> LinkedList a
linkedListRemoveKthFromEnd k xs = moveFast xs xs k
  where
    -- move the slow pointer k times forward
    moveFast slow fast 0 = removeSlow slow fast
    -- slow should be removed for _, but makes the function diffictul to read
    moveFast slow Empty _ = xs  -- special case where k is larger than list, so we return the original
    moveFast slow (Node _ fastTail) n = moveFast slow fastTail (n-1)
    
    -- move both pointers at the same time until fast reaches the end
    removeSlow Empty Empty = Empty
    removeSlow (Node _ slowTail) Empty = slowTail  -- we reach the end of the list with fast, so we omit current slow node
    removeSlow Empty (Node _ _) = Empty  -- ghci complains if i don't include this case
    removeSlow (Node slowVal slowTail) (Node _ fastTail) = 
        Node slowVal (removeSlow slowTail fastTail) -- moves both pointers at the same tim