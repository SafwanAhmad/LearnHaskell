data Tree = InteriorNode {value:: Double, left:: Tree, right:: Tree}
           | LeafNode {value:: Double}

-- Create a binary tree
buildTree = 
    let left = LeafNode 2.0
        right = LeafNode 3.0
    in  InteriorNode 1.0 left right


-- Print the tree in inorder
inOrder:: Tree -> String
inOrder (InteriorNode v l r) = (inOrder l) ++ " " ++ (show v) ++ " " ++ (inOrder r)
inOrder (LeafNode v) = show v
