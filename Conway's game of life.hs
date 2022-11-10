import CodeWorld

data Line a = Line [a] a [a] deriving (Show)
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- |1.1
-- A function that lets go of all elements after a given size in both sides of the focus
cutLine :: Int -> Line a -> Line a
cutLine x (Line xs y ys) = Line (take x xs) y (take x ys)

-- |1.2
-- A function that keeps applying a given function to a given parameter until the result is Nothing. Collects the results in a list
applyFunction ::(a -> Maybe a) -> a -> [a]
applyFunction f x = case f x of
                      Nothing -> []
                      Just y -> y : applyFunction f y

-- A function that generates a line given two functions, the first of which fills the the xs of the line, the second of which fills the ys of the line.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (applyFunction f x) x (applyFunction g x)
  
-- |1.3
-- A function that maps a given function onto all elements of a line
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs p ys) = Line (map f xs) (f p) (map f ys)

-- |1.4
-- A function that zips two lines
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs1 p1 ys1) (Line xs2 p2 ys2) = Line (zip xs1 xs2) (p1,p2) (zip ys1 ys2)

-- A function that zips two lines using a given function
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line xs1 p1 ys1) (Line xs2 p2 ys2) = Line (zipWith f xs1 xs2) (f p1 p2) (zipWith f ys1 ys2)

---------------------------------------------------------------
data Cell = Alive | Dead deriving (Show, Eq)

-- A function that applies the xor binary operator on two Cells
xor_ :: Cell -> Cell -> Cell
xor_ Dead Dead = Dead
xor_ Alive Alive = Dead
xor_ _ _ = Alive

-- A function that applies the or binary operator on two Cells
or_ :: Cell -> Cell -> Cell
or_ Dead Dead = Dead
or_ _ _ = Alive

-- |1.5
-- A function that calculates the result of applying Rule30 on the focus of a given line
rule30 :: Line Cell -> Cell
rule30 (Line [] p (y:ys)) = xor_ Dead (or_ p y)
rule30 (Line (x:xs) p []) = xor_ x (or_ p Dead)
rule30 (Line (x:xs) p (y:ys)) = xor_ x (or_ p y)

-- |1.6
-- A function that shifts the focus of a given line to the left
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing 
shiftLeft (Line (x:xs) p ys) = Just (Line xs x (p:ys))

-- A function that shifts the focus of a given line to the right
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight (Line xs p (y:ys)) = Just (Line (p:xs) y ys)

-- |1.7
-- A function that collects the results of applying shiftRight on a line in a list
lineShiftsRight :: Line a -> [Line a]
lineShiftsRight (Line _ _ []) = []
lineShiftsRight (Line xs p (y:ys)) = go (Line (p:xs) y ys)
  where
    go :: Line a -> [Line a]
    go line = case (shiftRight line) of
                Nothing -> [line]
                Just line1 -> line : go line1
                
-- A function that collects the results of applying shiftLeft on a line in a list
lineShiftsLeft :: Line a -> [Line a]
lineShiftsLeft (Line [] _ _) = []
lineShiftsLeft (Line (x:xs) p ys) = go (Line xs x (p:ys))
  where
    go :: Line a -> [Line a]
    go line = case (shiftLeft line) of
                Nothing -> [line]
                Just line1 -> line : go line1

-- A function that, using the previous functions, creates a line of lines which repreasents all possible lines with different shifts from a given line.
-- Keeps the original line as the focus of the returned one.
lineShifts :: Line a -> Line (Line a)
lineShifts line = (Line (lineShiftsLeft line) line (lineShiftsRight line))

applyRule30 :: Line Cell -> Line Cell 
applyRule30 line = mapLine rule30 (lineShifts line)

-- |1.8
-- A function that renders a line of pictures onto the console
renderLine :: Line Picture -> Picture
renderLine (Line xs p ys) = translated (-1) 0 (renderLeft xs) <> p <> translated 1 0 (renderRight ys)
  where
    renderLeft :: [Picture] -> Picture
    renderLeft [] = blank
    renderLeft (x:xs) = x <> translated (-1) 0 (renderLeft xs)
    
    renderRight :: [Picture] -> Picture
    renderRight [] = blank
    renderRight (x:xs) = x <> translated 1 0 (renderRight xs)

-- A function that applies Rule30 on a line a given number of times, and prints the result in a grid, the last row of which is the first application of Rule30
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 _ = blank
renderRule30 n line =  translated 0 1 (renderRule30 (n-1) (applyRule30 line)) <> renderLine (mapLine f (applyRule30 line))
  where
    f :: Cell -> Picture
    f Alive = colored black (solidRectangle 1 1)
    f _     = colored white (solidRectangle 1 1)

--------------------------------------------------------------------------------
data Space a = Space (Line (Line a))

-- |1.10
-- A function that maps a given function onto all elements of a given space
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space s) = Space (mapLine (\x -> mapLine f (x)) s)

-- A function that zips two spaces
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line xs1 p1 ys1)) (Space (Line xs2 p2 ys2)) =  Space (Line (go xs1 xs2) (zipLines p1 p2) (go ys1 ys2))
  where
    go :: [Line a] -> [Line b] -> [Line (a, b)]
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = zipLines x y : go xs ys
    
-- A function that zips two spaces using a given function
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f (Space (Line xs1 p1 ys1)) (Space (Line xs2 p2 ys2)) =  Space (Line (go f xs1 xs2) (zipLinesWith f p1 p2) (go f ys1 ys2))
  where
    go :: (a -> b -> c) -> [Line a] -> [Line b] -> [Line c]
    go _ [] _ = []
    go _ _ [] = []
    go f (x:xs) (y:ys) = zipLinesWith f x y : go f xs ys
    
--------------------------------------------------------------------------------

-- |1.12
-- A function that returns the result of applying conway's rule to the focus of a given space
-- Considers non exsistent neighboring cells as dead cells
conwayRule :: Space Cell -> Cell
conwayRule (Space (Line [] p ys)) = conwayRule (Space (Line [Line [Dead] Dead [Dead]] p ys))
conwayRule (Space (Line xs p [])) = conwayRule (Space (Line xs p [Line [Dead] Dead [Dead]]))
conwayRule (Space (Line (Line leftUp left leftDown:xs) (Line up p down) (Line rightUp right rightDown:ys))) 
  | isAlive p == 1 && cnt == 2 = Alive
  | cnt == 3 = Alive
  | otherwise = Dead
  where
    isAlive :: Cell -> Int
    isAlive Alive = 1
    isAlive _ = 0
    
    cnt = sum [c1, c2, c3, c4, c5, c6, c7, c8]
      where
        c1 = case up of 
              [] -> 0
              _  -> isAlive (head up)
        c2 = case down of 
              [] -> 0
              _  -> isAlive (head down)
        c3 = isAlive left
        c4 = isAlive right
        c5 = case leftUp of 
              [] -> 0
              _  -> isAlive (head leftUp)
        c6 = case leftDown of 
              [] -> 0
              _  -> isAlive (head leftDown)
        c7 = case rightUp of 
              [] -> 0
              _  -> isAlive (head rightUp)
        c8 = case rightDown of 
              [] -> 0
              _  -> isAlive (head rightDown)
        
-- |1.13
-- A function that extracts the value from a given Maybe data
extractValue :: Maybe a -> a
extractValue (Just a) = a

-- A function that, given a space, returns Just (the space with all its lines having their focus shifted to the left -or up in the bigger picture-), or Nothing if no line can be shifted
shiftUp :: Space a -> Maybe (Space a)
shiftUp (Space (Line xs p ys)) = case (shiftLeft p) of
                                       Nothing -> Nothing
                                       Just x  -> Just (Space (Line (map (\x -> extractValue (shiftLeft x)) xs) x (map (\y -> extractValue (shiftLeft y)) ys)))

-- A function that, given a space, returns Just (the space with all its lines having their focus shifted to the right -or down in the bigger picture-), or Nothing if no line can be shifted
shiftDown :: Space a -> Maybe (Space a)
shiftDown (Space (Line xs p ys)) = case (shiftRight p) of
                                        Nothing -> Nothing
                                        Just x  -> Just (Space (Line (map (\x -> extractValue (shiftRight x)) xs) x (map (\y -> extractValue (shiftRight y)) ys)))
                               
-- A function that, given a space, returns a list of all possible spaces with different focuses (where the focus is shifted up) of lines (doesn't change the Line of focus of the given space)                               
spaceShiftsUp :: Space a -> [Space a]
spaceShiftsUp (Space (Line xs p ys)) = gogo (Space (Line xs p ys))
    where
      go :: Space a -> [Space a]
      go space = case (shiftUp space) of
                  Nothing -> [space]
                  Just space1 -> space : go space1
      gogo space = case (shiftUp space) of
                  Nothing -> []
                  Just space1 -> go space1

-- A function that, given a space, returns a list of all possible spaces with different focuses (where the focus is shifted down) of lines (doesn't change the Line of focus of the given space)  
spaceShiftsDown :: Space a -> [Space a]
spaceShiftsDown (Space (Line xs p ys)) = gogo (Space (Line xs p ys))
    where
      go :: Space a -> [Space a]
      go space = case (shiftDown space) of
                  Nothing -> [space]
                  Just space1 -> space : go space1
      gogo space = case (shiftDown space) of
                  Nothing -> []
                  Just space1 -> go space1

-- A function that, given a space, returns a line that represents all possible spaces with different focuses (Doens't change the line of focus of the given space)
handleColumn :: Space a -> Line (Space a)
handleColumn space = Line (spaceShiftsUp space) space (spaceShiftsDown space)

-- A function that, given a space, returns a list of lines of spaces representing all possbile spaces with differnet lines of focus and different focuses (where the line of focus is above the original line of focus)
goUp :: Space a -> [Line (Space a)]
goUp (Space (Line _ _ [])) = []
goUp (Space (Line xs p ys)) = (go (Space (Line xs p ys)))
  where
    go :: Space a -> [Line (Space a)]
    go (Space (Line _ _ [])) = []
    go (Space (Line xs p (y:ys))) = (handleColumn (Space (Line (p:xs) y ys))) : (go (Space (Line (p:xs) y ys)))

-- A function that, given a space, returns a list of lines of spaces representing all possbile spaces with differnet lines of focus and different focuses (where the line of focus is under the original line of focus)
goDown :: Space a -> [Line (Space a)]
goDown (Space (Line [] _ _)) = []
goDown (Space (Line xs p ys)) = go (Space (Line xs p ys))
  where
    go :: Space a -> [Line (Space a)]
    go (Space (Line [] _ _)) = []
    go (Space (Line (x:xs) p ys)) = (handleColumn (Space (Line xs x (p:ys)))) : (go (Space (Line xs x (p:ys))))

-- A function that, using the above functions, and given a space, returns a space of spaces representing all possbile spaces having the same elements of the given space, with
-- different lines of focus and different focuses
spaceShifts :: Space a -> Space (Space a)
spaceShifts space = Space (Line (goDown space) (handleColumn space) (goUp space))

-- A function that returns a space representing the result of applying conwayRule to all elements of a given space once.
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- |1.14
-- A function that renders a space of pictures line by line.
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs p ys)) = translated 0 (-1) (renderUp xs) <> renderLine p <> translated 0 1 (renderDown ys)
  where
    renderUp :: [Line Picture] -> Picture
    renderUp [] = blank
    renderUp (x:xs) = renderLine x <> translated 0 (-1) (renderUp xs)
    
    renderDown :: [Line Picture] -> Picture
    renderDown [] = blank
    renderDown (y:ys) = renderLine y <> translated 0 1 (renderDown ys)

-- A function the converts a cell to a picture
cellToPicture :: Cell -> Picture
cellToPicture x = case x of 
                    Dead -> colored black (rectangle 1 1)
                    Alive  -> colored black (solidRectangle 1 1)

-- A function that, given a number and a space, applies conwaysRule (floor times -thus changing every second-) time, and returns a picture representing the result
applyConwayRuleTimes :: Double -> Space Cell -> Picture 
applyConwayRuleTimes times space = go (floor times) space
  where
    go :: Int -> Space Cell -> Picture
    go 0 space = renderSpace (mapSpace cellToPicture (applyConwayRule space))
    go n space = go (n-1) (applyConwayRule space)

-- A function that animates the conway game of life, updating every second
animateConway :: Space Cell -> IO ()
animateConway space = animationOf go
  where
    go :: Double -> Picture
    go seconds = applyConwayRuleTimes seconds space

--------------------------------------------------------------------------------

-- Some samples to test our function on
-- some were shared from friends
pentaDecathlon :: Space Cell 
pentaDecathlon = Space(
    Line 
        [
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ]
        (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
        [
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
            Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ])
        
glider :: Space Cell
glider =  Space(
    Line 
        [
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ]
        (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
        [
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Alive, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Alive [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ])


sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)
    
blinker = (Space (Line blinker' (Line [Alive, Dead, Dead] Alive [Alive, Dead, Dead]) blinker')) 
  where
    blinker' = replicate 3 (Line (replicate 3 Dead) Dead (replicate 3 Dead))
    
toad = (Space (Line toad' (Line (replicate 2 Dead) Alive [Alive, Alive, Dead]) ((Line [Alive, Dead] Alive [Alive, Dead, Dead]):toad')))
  where 
    toad' = replicate 2 (Line (replicate 2 Dead) Dead (replicate 3 Dead))
   

-- | You can test different drawings or animations by removing the comment from one of the lines below, and commenting the rest.    
main :: IO()
main = do
  print (cutLine 3 integers)
  print (cutLine 4 (mapLine (^2) integers))
  print (cutLine 4 (zipLines integers integers))
  print (cutLine 4 (zipLinesWith (*) integers integers))
  print (rule30 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive]))
  print (rule30 (Line [Alive, Alive, Alive] Alive [Alive, Alive, Alive]))
  print (shiftRight (Line [0,1,1] 1 [1,1,1]))
  print (shiftLeft (Line [0,1,1] 1 [1,1,1]))
  print (lineShifts (Line [2,1] 3 [4,5]))
  print (cutLine 4 $ applyRule30 (Line ([Dead, Alive, Alive] ++ repeat Dead) Alive ([Alive, Alive, Alive] ++ repeat Dead)))
  --print (genLine (applyIf (> -3) (subtract 1)) 0 (applyIf (< 3) (+1)))
  print (genLine (\_ -> Nothing) 1 (\_ -> Nothing))
  print (cutLine 3 (genLine Just 0 Just))
  print (applyRule30 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive]))
  --drawingOf (renderLine sampleLine)
  --drawingOf (renderRule30 9 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive])& (colored black (solidRectangle 20 20)))
  --drawingOf (renderSpace (mapSpace cellToPicture (applyConwayRule (applyConwayRule toad))))
  --drawingOf (renderSpace (mapSpace cellToPicture glider))
  animateConway pentaDecathlon
  --animateConway blinker
  --animateConway toad
  --animateConway glider