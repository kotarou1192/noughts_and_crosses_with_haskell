module GameState (processGame, startGame, isP1Turn, isP2Turn, GameState) where

type TurnCount = Int

data CurrentPlayer = Player1 | Player2 deriving (Read, Show, Bounded, Ord, Eq, Enum)

type CurrentTurn = (TurnCount, CurrentPlayer)

type GameState = Maybe CurrentTurn

processGame :: CurrentTurn -> GameState
processGame (t, p)
  | t > 9 = Nothing
  | t < 1 = Nothing
  | otherwise = Just (t + 1, nextP p)

startGame :: GameState
startGame = return (1, Player1)

nextP :: CurrentPlayer -> CurrentPlayer
nextP p
  | isP1Turn p = Player2
  | otherwise = Player1

isP1Turn :: CurrentPlayer -> Bool
isP1Turn p
  | p == Player1 = True
  | otherwise = False

isP2Turn :: CurrentPlayer -> Bool
isP2Turn p
  | p == Player2 = True
  | otherwise = False
