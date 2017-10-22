data Tree = InteriorNode {value:: Double, left:: Tree, right:: Tree}
           | LeafNode {value:: Double}

-- Create a binary tree
buildTree = 
    let left = InteriorNode 2.0 (LeafNode 4.0) (LeafNode 5.0) 
        right = InteriorNode 3.0 (LeafNode 6.0) (LeafNode 7.0)
    in  InteriorNode 1.0 left right


-- Print the tree in inorder
inOrder:: Tree -> String
inOrder (InteriorNode v l r) = (inOrder l) ++ " " ++ (show v) ++ " " ++ (inOrder r)
inOrder (LeafNode v) = show v

-- Find the sum of all nodes
treeSum:: Tree -> Double
treeSum (InteriorNode v l r) = v + (treeSum l) + (treeSum r)
treeSum (LeafNode v) = v 