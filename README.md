## Cellular Autommata for CS 557
In this project I used `Comoands` and `Brick` to implement sa simple cellular automata. I had a stretch goal of implementing several different rule sets, but did not get quite as far as I would have liked. I did get to implement a generic structure. that makes it very easy to implement new sets of rules.

The most interesting part of the project was using `Comonads` for the Cellular automata functions.
```
instance Comonad Grid where
  -- extract:: w a -> a
  extract (Grid mtx (x,y)) = getElem x y mtx
  -- extend::(w a -> b)->w a->w b
  extend f = fmap f . duplicate
  -- duplicate::w a -> w(w a) 
  duplicate (Grid mtx (x,y)) = Grid (matrix sizeX sizeY (\(w,z)->(Grid mtx (w,z)))) (x,y)
````

Then the next thing that was somewhat difficult to tackle was making a class for the CA that was generic enough to be used in any of the Brick functions in main, this would allow me to easily change between rule sets without any work. I settled on making a class for `Cell` as well as a `CA` class that contained both a `Cell` and some `Comonad`.

```
class CA w c | w->c where
  next_gen::w->c
  neighborValues::w->[c]
  start_state::w
  . . .

class Cell c where
  cellAttr::c->AttrName

```