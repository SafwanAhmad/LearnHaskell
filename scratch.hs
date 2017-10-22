data List = Node {value:: Double, next:: List}
        |   Empty

printList :: List -> String
--printList x | x == (Node v n) = show v ++ printList n
--        | otherwise = show '_'
printList (Node v n) = show v ++ " " ++ printList n
printList Empty = ""