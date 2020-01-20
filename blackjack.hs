import System.Random
import Data.List
import Text.Printf
import Learning

-- a blackjack simulator to measure effectiveness of tactics

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving (Show, Eq, Enum)
type Hand = [Card]
type Deck = [Card]
fullDeck :: Deck
fullDeck = [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]

shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled

  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

cardValues :: Card -> [Int]
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]

-- separate the first N cards in the deck from the rest of the deck
dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)

-- blackjack is a hand of two cards, an ace and a ten/picture card
handIsBlackjack :: Hand -> Bool
handIsBlackjack [card1, card2] =
  ((card1 == Ace) && elem card2 [Ten, Jack, Queen, King]) ||
  ((card2 == Ace) && elem card1 [Ten, Jack, Queen, King])
handIsBlackjack _ = False

handIsSoft :: Hand -> Bool
handIsSoft hand = Ace `elem` hand

-- work out the possible scores this hand could have. No concerns have
-- been given to efficiency here.
possibleHandTotals :: Hand -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- cardValues card]

data Score = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)

scoreInt :: Score -> Int
scoreInt (Value x) = x


handScore :: Hand -> Score
handScore hand
  | null notBustTotals = Bust
  | handIsBlackjack hand = Blackjack
  | otherwise = Value (last notBustTotals)
  where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]

-- todo: Split
data Move = Hit | Stand | DoubleDown deriving (Show, Eq)

-- in Las Vegas, dealer hits on soft 17
dealerNextMove :: Hand -> Move
dealerNextMove hand
  | score < Value 17 = Hit
  | score == Value 17 = if handIsSoft hand then Hit else Stand
  | otherwise = Stand
  where score = handScore hand

-- very simple player for the time being
playerNextMove :: Hand -> Card -> [[Int]] -> (Move, [[Int]])
playerNextMove playerHand dealerVisibleCard lst
  | playerScore == Value 12 = if getVal 0 1 lst < 20 then (Hit, changeElem 0 1 ((getVal 0 1 lst)+1) lst)
      else if ((getVal 0 0 lst) / (getVal 0 1 lst)) < 0.50 then (Stand, changeElem 0 1 ((getVal 0 1 lst)+1) lst)
      else (Hit, changeElem 0 1 ((getVal 0 1 lst)+1) lst)
  | playerScore == Value 13 = if getVal 1 1 lst < 20 then (Hit, changeElem 1 1 ((getVal 1 1 lst)+1) lst)
      else if ((getVal 1 0 lst) / (getVal 1 1 lst)) < 0.50 then (Stand, changeElem 1 1 ((getVal 1 1 lst)+1) lst)
      else (Hit, changeElem 1 1 ((getVal 1 1 lst)+1) lst)
  | playerScore == Value 14 = if getVal 2 1 lst < 20 then (Hit, changeElem 2 1 ((getVal 2 1 lst)+1) lst)
      else if ((getVal 2 0 lst) / (getVal 2 1 lst)) < 0.50 then (Stand, changeElem 2 1 ((getVal 2 1 lst)+1) lst)
      else (Hit, changeElem 2 1 ((getVal 2 1 lst)+1) lst)
  | playerScore == Value 15 = if getVal 3 1 lst < 20 then (Hit, changeElem 3 1 ((getVal 3 1 lst)+1) lst)
      else if ((getVal 3 0 lst) / (getVal 3 1 lst)) < 0.50 then (Stand, changeElem 3 1 ((getVal 3 1 lst)+1) lst)
      else (Hit, changeElem 3 1 ((getVal 3 1 lst)+1) lst)
  | playerScore == Value 16 = if getVal 4 1 lst < 20 then (Hit, changeElem 4 1 ((getVal 4 1 lst)+1) lst)
      else if ((getVal 4 0 lst) / (getVal 4 1 lst)) < 0.50 then (Stand, changeElem 4 1 ((getVal 4 1 lst)+1) lst)
      else (Hit, changeElem 4 1 ((getVal 4 1 lst)+1) lst)
  | playerScore == Value 17 = if getVal 5 1 lst < 20 then (Hit, changeElem 5 1 ((getVal 5 1 lst)+1) lst)
      else if ((getVal 5 0 lst) / (getVal 5 1 lst)) < 0.50 then (Stand, changeElem 5 1 ((getVal 5 1 lst)+1) lst)
      else (Hit, changeElem 5 1 ((getVal 5 1 lst)+1) lst)
  | playerScore == Value 18 = if getVal 6 1 lst < 20 then (Hit, changeElem 6 1 ((getVal 6 1 lst)+1) lst)
      else if ((getVal 6 0 lst) / (getVal 6 1 lst)) < 0.50 then (Stand, changeElem 6 1 ((getVal 6 1 lst)+1) lst)
      else (Hit, changeElem 6 1 ((getVal 6 1 lst)+1) lst)
  | playerScore == Value 19 = if getVal 7 1 lst < 20 then (Hit, changeElem 7 1 ((getVal 7 1 lst)+1) lst)
      else if ((getVal 7 0 lst) / (getVal 7 1 lst)) < 0.50 then (Stand, changeElem 7 1 ((getVal 7 1 lst)+1) lst)
      else (Hit, changeElem 7 1 ((getVal 7 1 lst)+1) lst)
  | playerScore == Value 20 = if getVal 8 1 lst < 20 then (Hit, changeElem 8 1 ((getVal 8 1 lst)+1) lst)
      else if ((getVal 8 0 lst) / (getVal 8 1 lst)) < 0.50 then (Stand, changeElem 8 1 ((getVal 8 1 lst)+1) lst)
      else (Hit, changeElem 8 1 ((getVal 8 1 lst)+1) lst)
  | playerScore == Value 21 = (Stand, lst)
  | playerScore == Value 11 || playerScore == Value 10 = (DoubleDown, lst)
  | otherwise = (Hit, lst)
  where playerScore = handScore playerHand
        dealerScore = handScore [dealerVisibleCard]

-- since the money gained from winning with a blackjack hand is
-- different, we use two wins
data Outcome = Loss | Push | Win | BlackjackWin deriving (Show, Eq)

-- a Las Vegas casino generally only deals with whole numbers of dollars
type Money = Integer

-- calculate the money made in this hand
moneyMade :: Money -> Outcome -> Money
moneyMade bet Loss         = -1 * bet
moneyMade _   Push         = 0
moneyMade bet Win          = bet
moneyMade bet BlackjackWin = ceiling $ (1.5 :: Double) * fromIntegral bet

findOutcome :: Score -> Score -> [[Int]] -> (Outcome, [[Int]])
findOutcome Bust _ lst= (Loss,lst)
findOutcome Blackjack _ lst= (BlackjackWin,lst)
findOutcome _ Bust lst= (Win,lst)
findOutcome playerScore dealerScore lst
  | playerScore > dealerScore = (Win, (changeElem (scoreInt(playerScore)-12) 0 ((getVal (scoreInt (playerScore)-12) 0 lst)+1) lst))
  | playerScore == dealerScore = (Push, lst)
  | otherwise = (Loss, lst)

data GameState = PlayerPlaying | DealerPlaying

-- we pass the bet during the round too, since the bet can change
roundOutcome :: Money -> GameState -> Hand -> Hand -> Deck -> [[Int]] -> ((Outcome,[[Int]]), Money)
roundOutcome _ _ _ _ [] _ = error "Deck is empty!"
roundOutcome bet PlayerPlaying playerHand dealerHand (card:cards) lst
  | playerScore == Bust      = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) lst
  | playerMove == Stand      = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) lst
  | playerMove == Hit        = roundOutcome bet PlayerPlaying (card:playerHand) dealerHand cards lst
  | playerMove == DoubleDown = roundOutcome (2 * bet) DealerPlaying (card:playerHand) dealerHand cards lst
  where playerScore = handScore playerHand
        (playerMove, lst) = playerNextMove playerHand (head dealerHand) lst

roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) lst
  | dealerScore == Bust = (findOutcome playerScore dealerScore lst, bet)
  | dealerMove == Hit   = roundOutcome bet DealerPlaying playerHand (card:dealerHand) cards lst
  | dealerMove == Stand = (findOutcome playerScore dealerScore lst, bet)
  where playerScore = handScore playerHand
        dealerScore = handScore dealerHand
        dealerMove = dealerNextMove dealerHand

roundTakings :: Money -> Hand -> Hand -> Deck -> [[Int]] -> ([[Int]], Money)
roundTakings bet playerHand dealerHand remainingDeck lst = (lstUpdated, (moneyMade finalBet outcome))
  where ((outcome, lstUpdated), finalBet) = roundOutcome bet PlayerPlaying playerHand dealerHand remainingDeck lst

-- play a game with the current strategy
playRound :: Money -> [[Int]] -> ([[Int]], IO Money)
playRound bet lst = do
  shuffledDeck <- shuffleDeck
  -- we don't deal cards in an alternating order, but it makes no difference
  let (playerHand, remainingDeck) = dealCards 2 shuffledDeck
      (dealerHand, remainingDeck') = dealCards 2 remainingDeck
      (lst', takings) = roundTakings bet playerHand dealerHand remainingDeck' lst
  return (lst', takings)

-- play a game N times and work out the overall takings/losses for the
-- given bet size
play :: Integer -> Money -> [[Int]] -> IO Money
play 0 _ lst = return 0
play count bet lst =
  play' count bet 0 lst
  where
    play' 0 _ accum lst' = return accum
    play' count' bet' accum lst'= do
      (lst', takings) <- playRound bet' lst'
      play' (count' - 1) bet' (accum + takings) lst'

main = do
  let iterations = 1000 :: Integer
      bet = 10 :: Money
  let lst = [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]
  takings <- play iterations bet lst :: IO Money
  let houseEdge = fromInteger (-1 * takings) / fromInteger (bet * iterations)
      housePercentage = 100 * houseEdge :: Double
  printf "After %d $%d hands, total money made was $%d (house made %.2f%%).\n"
    iterations bet takings housePercentage
