data Frame = FinalFrame Int Int | Strike | Spare Int Int | Roll Int Int

rollsToFrames :: Int -> [Int] -> [Frame]
rollsToFrames _ [] = [Roll 0 0]
rollsToFrames frameCount _ | frameCount > 10 = error "too many frames"
rollsToFrames 10 [x,y] = [FinalFrame x y]
rollsToFrames 10 [x] = [FinalFrame x 0]
rollsToFrames 10 (x:y:_:_) = error "too many rolls"
rollsToFrames frameCount (10:rs) = Strike : (rollsToFrames (frameCount+1) rs)
rollsToFrames frameCount (x:y:rs) | x+y == 10 = (Spare x y) : (rollsToFrames (frameCount+1) rs)
rollsToFrames frameCount (x:y:rs) = (Roll x y) : (rollsToFrames (frameCount+1) rs)
rollsToFrames _ [x] = [Roll x 0]

pinsPerFrame :: Frame -> Int
pinsPerFrame Strike = 10
pinsPerFrame (Spare _ _) = 10
pinsPerFrame (Roll x y) = x + y
pinsPerFrame (FinalFrame x y) = x + y

firstRollPins :: Frame -> Int
firstRollPins Strike = 10
firstRollPins (Spare x _) = x
firstRollPins (Roll x _) = x
firstRollPins (FinalFrame x _) = x

score' :: [Frame] -> Int
score' [FinalFrame x y] = 0
score' (Strike:fs@(Strike:f:_)) = (20 + firstRollPins f) + (score' fs)
score' (Strike:fs@(f:_)) = (10 + pinsPerFrame f) + (score' fs)
score' ((Spare _ _):fs@(f:_)) = (10 + firstRollPins f) + (score' fs)
score' (f@(Roll x y):fs) = (pinsPerFrame f) + (score' fs)
score' [] = 0

score :: [Int] -> Int
score = score' . rollsToFrames 0