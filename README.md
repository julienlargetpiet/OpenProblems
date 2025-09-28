# OpenProblems

A collection of original algorithmic problems I couldn’t solve — people who crack them get $50

Contact me at:

  <li><a href="mailto:j.larget-piet@proton.me">j.larget-piet@proton.me</a></li>

The name of the first person to solve the problem will be displayed with congratulations in this repo!

## FormalismConversions (Haskell)

### Statement and motivation

This problem originates from the motivation to find all possible results given a formula with `n` values, like:

`X1 + X2 - X3 * X4`

So here we are given a set of operators and a set of values.

Basiacally there are `n - 1` operators, one between each value.

So it is trivial to find all possible results, we just have to use a cartesian product of all operators `n - 1` times, then put the operators between each value, and calculate the result.

In Haskell, to find all operators combinations, it would look like this:

`sequence . replicate 3 $ "+-*/"`

But as you know, in each formula, comes the parenthesis.

So it will entirely reshape the number of possible results from `len of the operator set ^ (n - 1)` to something much bigger.

The first idea i had was to implement a function that gives me all the possible set sizes, i successfully did it, and it is named `howAdd`.

It takes the number of values as inputs and returns a vector of vector of Int containing all the set sizes.

For example for 4 values:

```
ghci> howAdd 4
[[1,1,1,1],[2,2],[2,1,1],[1,2,1],[1,1,2],[3,1],[1,3]]
```

```
ghci> howAdd 5
[[1,1,1,1,1],[2,2,1],[2,1,2],[1,2,2],[2,1,1,1],[1,2,1,1],[1,1,2,1],[1,1,1,2],[3,2],[2,3],[3,1,1],[1,3,1],[1,1,3],[4,1],[1,4]]
```

By the way the sum of each vector is always equal to the number of values:

```
ghci> all (==5) . map (sum) $ howAdd 5
True
```

Then i created a Data Structure that will help me "taking elements" from a formula

`data PTree a = PNode a [[PTree a]] deriving (Show, Eq)`

It is basically a list that allows different depth lists inside.

Why ?

Because look at the outputs for `howAdd 4` for example, at a point i have `[1, 3]`

Now the question is: how is `3` partitioned ?

The function `howIntricated` with the `PTree` data structure will recursively find all the possible sub-partitions for all partitions. 

Example:

```
ghci> howAddIntricated $ howAdd 4
[[PNode 1 [],PNode 1 [],PNode 1 [],PNode 1 []],
 [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 2 [[PNode 1 [],PNode 1 []]]],
 [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 [],PNode 1 []],
 [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []],
 [PNode 1 [],PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]],
 [PNode 3 [[PNode 1 [],PNode 1 [],PNode 1 []],
          [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []],
          [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]]],
          PNode 1 []],
 [PNode 1 [],
 PNode 3 [[PNode 1 [],PNode 1 [],PNode 1 []],
          [PNode 2 [[PNode 1 [],PNode 1 []]],PNode 1 []],
          [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]]]]]]
```

As you see, we found all the possible partitions !

Great, we just invented a formalism !!!

Indeed, with some effort, we can reconstruct a formula from this data.

But it is literally a huge mess to work with this structure.

All the others function i wrote to construct all the possible formulas are done with another formalism that i manually created, but is a lot easier to work with:

Example:

```
examplePTree :: PTree Int
examplePTree = PNode 4 [[PNode 1 [],PNode 1 [],PNode 1 [], PNode 1 []],
              [PNode 2 [[PNode 1 [], PNode 1 []]],PNode 1 [], PNode 1 []],
              [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]], PNode 1 []],
              [PNode 1 [], PNode 1 [], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 2 [[PNode 1 [], PNode 1 []]], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 [], PNode 1 [], PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]], 
                        PNode 1 []],
              [PNode 1 [], 
                  PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]]],
              [PNode 1 [], 
                  PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]]]]
```

Spot the differences ? 

Instead of having **intricated set of partitions representation**, we now got just one set of partitions representation !

So your goal, is to find an algorithm that would correctly convert from the first formalism to the second.

You will find all the functions you need to solve this problem in `FormalismConversions/FormalismTries.hs`

I also provided what i tried, maybe it can help you.
  
  
  
