This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> data Shape = Rectangle Side Side
>            | Ellipse Radius Radius
>            | RtTriangle Side Side
>            | Polygon [Vertex]
>      deriving Show
>
> type Radius = Float
> type Side   = Float
> type Vertex  = (Float,Float)

> data List a = Nil | MkList a (List a)

< Nil    :: List a
< MkList :: a -> List a -> List a

< []  :: [a]
< (:) :: a -> [a] -> [a]

> data Tree a = Leaf a | Branch (Tree a) (Tree a)

< data IntegerTree = Leaf Integer | Branch IntegerTree IntegerTree
< data SimpleTree  = Leaf | Branch SimpleTree SimpleTree

> data InternalTree a = ILeaf 
>                     | IBranch a (InternalTree a) (InternalTree a)

> data FancyTree a b = FLeaf a 
>                    | FBranch b (FancyTree a b) (FancyTree a b)

> mapTree :: (a->b) -> Tree a -> Tree b

> mapTree f (Leaf x)       = Leaf (f x)
> mapTree f (Branch t1 t2) = Branch (mapTree f t1) 
>                                    (mapTree f t2)

> fringe               :: Tree a -> [a]
> fringe (Leaf x)       = [x]
> fringe (Branch t1 t2) = fringe t1 ++ fringe t2

> treeSize               :: Tree a -> Integer
> treeSize (Leaf x)       = 1
> treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

> treeHeight		   :: Tree a -> Integer
> treeHeight (Leaf x)       = 0
> treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) 
>                                     (treeHeight t2)

< data Expr = C Float | Add Expr Expr | Sub Expr Expr
<           | Mul Expr Expr | Div Expr Expr

> data Expr = C Float | Expr :+ Expr | Expr :- Expr
>           | Expr :* Expr | Expr :/ Expr

| (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)

> evaluate :: Expr -> Float

> evaluate (C x) = x
> evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
> evaluate (e1 :- e2) = evaluate e1 - evaluate e2
> evaluate (e1 :* e2) = evaluate e1 * evaluate e2
> evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

< data InternalTree a = ILeaf 
<                     | IBranch a (InternalTree a) (InternalTree a)

< takeTree      :: Int -> InternalTree a -> InternalTree a
< takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a

< t = let t' = IBranch 1 ILeaf ILeaf
<     in Branch 2 t' t'

| evaluate (Let "x" (C 5) (V "x" :+ V "x"))
| ===> 10

