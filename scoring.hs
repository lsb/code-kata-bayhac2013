import Control.Applicative ((<$>), (<*>),liftA2)

data Frame = FinalFrame Int Int | Strike | Spare Int Int | Roll Int Int | PartialFrame Int | PartialFinalFrame Int deriving Show

rollsToFrames :: Int -> [Int] -> [Frame]
rollsToFrames frameCount _ | frameCount > 10 = error "too many frames"
rollsToFrames 10 [x,y] = [FinalFrame x y]
rollsToFrames 10 [x] = [PartialFinalFrame x]
rollsToFrames 10 [] = []
rollsToFrames 10 (x:y:_:_) = error "too many rolls"
rollsToFrames frameCount (10:rs) = Strike : (rollsToFrames (frameCount+1) rs)
rollsToFrames frameCount (x:y:rs) | x+y == 10 = (Spare x y) : (rollsToFrames (frameCount+1) rs)
rollsToFrames frameCount (x:y:rs) = (Roll x y) : (rollsToFrames (frameCount+1) rs)
rollsToFrames _ [x] = [PartialFrame x]
rollsToFrames _ [] = []

pinsPerFrame :: Frame -> Maybe Int
pinsPerFrame Strike = Just 10
pinsPerFrame (Spare _ _) = Just 10
pinsPerFrame (Roll x y) = Just (x + y)
pinsPerFrame (FinalFrame x y) = Just (x + y)
pinsPerFrame (PartialFrame _) = Nothing
pinsPerFrame (PartialFinalFrame _) = Nothing

firstRollPins :: Frame -> Int
firstRollPins Strike = 10
firstRollPins (Spare x _) = x
firstRollPins (Roll x _) = x
firstRollPins (FinalFrame x _) = x
firstRollPins (PartialFrame x) = x
firstRollPins (PartialFinalFrame x) = x

scores :: [Frame] -> [Maybe Int]
scores [FinalFrame x y] = [Just 0]
scores (Strike:fs@(Strike:f:_)) = (Just (20 + firstRollPins f)) : (scores fs)
scores [Strike, Strike] = [Nothing, Nothing]
scores (Strike:fs@(f:_)) = ((10 +) <$> (pinsPerFrame f)) : (scores fs)
scores [Strike] = [Nothing]
scores ((Spare _ _):fs@(f:_)) = (Just (10 + firstRollPins f)) : (scores fs)
scores [Spare _ _] = [Nothing]
scores (f@(Roll x y):fs) = (pinsPerFrame f) : (scores fs)
scores [PartialFrame _] = [Nothing]
scores [PartialFinalFrame _] = [Nothing]
scores [] = []

runningScore :: [Frame] -> [Maybe Int]
runningScore = tail . scanl (liftA2 (+)) (Just 0) . scores

frameDisplay :: Frame -> String
frameDisplay Strike = "X_"
frameDisplay (Spare x _) = (show x ++ "/")
frameDisplay (Roll 0 y) = ("-" ++ show y)
frameDisplay (Roll x 0) = (show x ++ "-")
frameDisplay (Roll x y) = (show x ++ show y)
frameDisplay (PartialFrame x) = show x
frameDisplay (PartialFinalFrame x) = show x
frameDisplay (FinalFrame x y) = (show x ++ show y)

runningFrameDisplay :: [Frame] -> String
runningFrameDisplay = concatMap frameDisplay

showScore :: Maybe Int -> String
showScore Nothing = "~~~ "
showScore (Just x) = show x ++ " "

runningScoreDisplay :: [Frame] -> String
runningScoreDisplay = concatMap showScore . runningScore

display :: [Int] -> String
display rolls = unlines [runningFrameDisplay fs, runningScoreDisplay fs]
  where fs = rollsToFrames 0 rolls