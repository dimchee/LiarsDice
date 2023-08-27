{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- TODO delete this option
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Simulation (
    Move (..),
    Bid (..),
    PlayerId,
    Game,
    rounds,
    gameNumber,
    roundNumber,
    handNumber,
    playingOrder,
    dices,
    hands,
    newGame,
    step,
    loser,
    _MakeBid,
    _Challenger,
    _Pass',
    _Challenge',
    _Bid',
) where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Lens.Extras (is)
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
    }
makeLenses ''Game

handNumber :: (Sum Int -> Const (Sum Int) (Sum Int)) -> Game -> Const (Sum Int) Game
handNumber = rounds . _head . hands . to (Sum . length)

playingOrder :: Game -> [PlayerId]
playingOrder game = rotate (game ^. handNumber) $ game ^. rounds . _head . initialPlayingOrder

rotate :: Sum Int -> [a] -> [a]
rotate (Sum n) as = ys ++ xs
  where
    (xs, ys) = splitAt (n `mod` length as) as

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
        }

takeDice :: PlayerId -> Map.Map PlayerId Dices -> Map.Map PlayerId Count
takeDice player =
    (at player %~ maybe Nothing (\x -> if x > 1 then Just $ x - 1 else Nothing))
        . (each %~ (fromIntegral . length))

bidValid :: Maybe Bid -> Map.Map PlayerId Dices -> Bool
bidValid bid m =
    (length . filter (\x -> maybe False (\b -> x == value b) bid) . concatMap snd . Map.toList) m
        >= fromIntegral (maybe 0 count bid)

applyHand :: Hand -> Game -> Game
applyHand hand game = case hand of
    Invalid loser_ ->
        game & addLoser loser_ & rounds %~ cons (newRound $ takeDice loser_ curDices)
    Challenger p ->
        game & addLoser loser_ & rounds %~ cons (newRound $ takeDice loser_ curDices)
      where
        curBid = game ^? rounds . _head . hands . _head . _MakeBid
        loser_ = if bidValid curBid curDices then p else game ^. to playingOrder . _head
    MakeBid _ ->
        game
  where
    curDices = game ^. rounds . _head . dices
    addLoser player = rounds . _head . loser .~ player

step :: Map.Map PlayerId Move -> Game -> Game
step moves game = game & applyHand hand & rounds . _head . hands %~ cons hand
  where
    -- Player on turn can't pass, others can't bid
    sorted =
        game ^. to playingOrder
            & map (\x -> (x, moves ^. at x))
            & (_head . _2 . filtered (is $ _Just . _Pass') .~ Nothing)
            & (_tail . each . _2 . filtered (is $ _Just . _Bid') .~ Nothing)
    roundOver =
        Nothing
            <|> sorted ^? each . filtered (is _Nothing . snd) . _1 . to Invalid
            <|> sorted ^? each . filtered (is (_Just . _Challenge') . snd) . _1 . to Challenger
    hand = fromMaybe (Invalid "Imposible") $ roundOver <|> (sorted ^? _head . _2 . _Just . _Bid' . to MakeBid)
