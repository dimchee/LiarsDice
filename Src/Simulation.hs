{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- TODO delete this option
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Simulation where

import Control.Applicative (Alternative (..))
import Control.Lens
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)
import GHC.Natural
import System.Random

data Face = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Enum, Generic, Show, Uniform, UniformRange, Random)

-- instance FromJSON Face where
--     parseJSON v = toEnum . (\x -> x - 1) <$> parseJSON v
type Dices = [Face]

type Count = Natural
data Bid = Bid
    { count :: Count
    , value :: Face
    }
    deriving (Show)

instance Semigroup Bid

instance Monoid Bid where
    mempty = Bid{count = 0, value = One}

data Move
    = Bid' Bid
    | Challenge'
    | Pass'
    deriving (Generic, Show)
makePrisms ''Move

type PlayerId = String

data Hand = Challenger PlayerId | MakeBid Bid | Invalid PlayerId
makePrisms ''Hand

-- Should be 2 datas -- FinishedRound{[Bid], end :: Challenge | Invalid}, InProgressRound{[Bid]}
data Round = Round
    { _roundNumber :: Natural
    , _initialPlayingOrder :: [PlayerId]
    , _dices :: Map.Map PlayerId Dices
    , _hands :: [Hand]
    , _loser :: PlayerId
    }
makeLenses ''Round
data Game = Game
    { _gameNumber :: Natural
    , _rounds :: [Round]
    , _connectedPlayers :: [PlayerId]
    }
makeLenses ''Game

handNumber :: (Sum Int -> Const (Sum Int) (Sum Int)) -> Game -> Const (Sum Int) Game
handNumber = rounds . _head . hands . to (Sum . length)

-- TODO Make this a lens
playingOrder :: Game -> [PlayerId]
playingOrder game = rotate (game ^. handNumber) $ game ^. rounds . _head . initialPlayingOrder

-- TODO modulo arithmetic
rotate :: Sum Int -> [a] -> [a]
rotate (Sum n) as = ys ++ xs
  where
    (xs, ys) = splitAt n as

-- TODO Make generation random
generateDices :: Map.Map PlayerId Count -> Map.Map PlayerId Dices
generateDices = Map.map (\x -> replicate (fromIntegral x) One)

-- randomPlayer :: IO Player
-- randomPlayer = do
--     dices <- replicateM 5 randomIO
--     playerId <- randomIO
--     pure $ Player{playerId = playerId, dices = dices}

-- TODO shuffle playingOrder
newRound :: Map.Map PlayerId Count -> Round
newRound diceCounts =
    Round
        { _roundNumber = 0
        , _initialPlayingOrder = Map.keys diceCounts
        , _hands = []
        , _dices = generateDices diceCounts
        , -- TODO should change somehow
          _loser = "No Loser"
        }

newGame :: Count -> [PlayerId] -> Game
newGame diceNumber players =
    Game
        { _gameNumber = 0
        , _rounds = [newRound $ Map.fromList $ List.map (,diceNumber) players]
        , _connectedPlayers = players
        }

passToNothing :: Maybe Move -> Maybe Move
passToNothing move = do
    x <- move
    case x of
        Pass' -> Nothing
        _ -> move

bidToNothing :: Maybe Move -> Maybe Move
bidToNothing move = do
    x <- move
    case x of
        Bid' _ -> Nothing
        _ -> move

isChallenge :: Maybe Move -> Bool
isChallenge =
    maybe
        False
        ( \case
            Challenge' -> True
            _ -> False
        )

takeDice :: PlayerId -> Map.Map PlayerId Dices -> Map.Map PlayerId Count
takeDice player =
    (at player %~ maybe Nothing (\x -> if x > 1 then Just $ x - 1 else Nothing))
        . (each %~ (fromIntegral . length))

bidValid :: Bid -> Map.Map PlayerId Dices -> Bool
bidValid bid m = (length . filter (== value bid) . concatMap snd . Map.toList) m >= fromIntegral (count bid)

addLoser :: PlayerId -> Game -> Game
addLoser player = rounds . _head . loser .~ player

applyHand :: Hand -> Game -> Game
applyHand hand game = case hand of
    Invalid loser_ ->
        game & addLoser loser_ & rounds %~ cons (newRound $ takeDice loser_ curDices)
    Challenger p ->
        game & addLoser loser_ & rounds %~ cons (newRound $ takeDice loser_ curDices)
      where
        curBid = game ^. rounds . _head . hands . _head . _MakeBid
        loser_ = if bidValid curBid curDices then p else head $ playingOrder game
    MakeBid _ ->
        game
  where
    curDices = game ^. rounds . _head . dices

step :: Map.Map PlayerId Move -> Game -> Game
step moves game = game & applyHand hand & rounds . _head . hands %~ cons hand
  where
    -- Player on turn can't pass, others can't bid
    sorted =
        map (\x -> (x, moves ^. at x)) (playingOrder game)
            & (_head . _2 %~ passToNothing)
            & (_tail . each . _2 %~ bidToNothing)
    roundOver =
        (Invalid . fst <$> List.find (isNothing . snd) sorted)
            <|> (Challenger . fst <$> List.find (isChallenge . snd) sorted)
    hand = fromMaybe (MakeBid $ sorted ^. _head . _2 . _Just . _Bid') roundOver
