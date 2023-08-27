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
    Arena,
    moveNumber,
    playingOrder,
    dices,
    loser,
    _Challenger,
    _Pass',
    _Challenge',
    _Bid',
    finishedGames,
    runningGame,
    finished,
    running,
    bids,
    roundEnd,
    newArena,
    step,
) where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Lens.Extras (is)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)
import GHC.Natural
import System.Random
import Prelude hiding (round)

type PlayerId = String
type Count = Natural
data Face = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Enum, Generic, Show, Uniform, UniformRange, Random)
type Dices = [Face]

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

data RoundEnd = Challenger PlayerId | Invalid PlayerId
makePrisms ''RoundEnd
data Round = Round
    { _initialPlayingOrder :: [PlayerId]
    , _dices :: Map.Map PlayerId Dices
    , _bids :: [Bid]
    }
makeLenses ''Round
type RoundRunning = Round
data RoundFinished = RoundFinished
    { _roundEnd :: RoundEnd
    , _loser :: PlayerId
    -- , _getRound :: Round
    }
makeLenses ''RoundFinished

data Game = Game
    { _finished :: [RoundFinished]
    , _running :: RoundRunning
    }
makeLenses ''Game

data Arena = Arena
    { _finishedGames :: [Game]
    , _runningGame :: Game
    }
makeLenses ''Arena

-- randomPlayer :: IO Player
-- randomPlayer = do
--     dices <- replicateM 5 randomIO
--     playerId <- randomIO
--     pure $ Player{playerId = playerId, dices = dices}

-- TODO shuffle playingOrder
newRound :: Map.Map PlayerId Count -> Round
newRound diceCounts =
    Round
        { _initialPlayingOrder = Map.keys diceCounts
        , _dices = generateDices diceCounts
        , _bids = []
        }
newGame :: Count -> [PlayerId] -> Game
newGame diceNumber players = Game [] $ newRound $ Map.fromList $ (,diceNumber) <$> players
newArena :: Count -> [PlayerId] -> Arena
newArena diceNumber players = Arena [] $ newGame diceNumber players

moveNumber :: Round -> Sum Int
moveNumber round = round ^. bids . to length . to Sum
playingOrder :: Round -> [PlayerId]
playingOrder round = round ^. initialPlayingOrder . to (rotate $ moveNumber round)
rotate :: Sum Int -> [a] -> [a]
rotate (Sum n) as = ys ++ xs
  where
    (xs, ys) = splitAt (n `mod` length as) as

-- TODO Make generation random
generateDices :: Map.Map PlayerId Count -> Map.Map PlayerId Dices
generateDices = Map.map (\x -> replicate (fromIntegral x) One)

finishRound :: RoundEnd -> Game -> Game
finishRound end game =
    game
        & (finished %~ cons finishedRound)
        & (running .~ newRoundPunished)
  where
    bidValid bid =
        (game ^. running . dices . to (length . filter (\x -> maybe False (\b -> x == value b) bid) . concatMap snd . Map.toList))
            >= fromIntegral (maybe 0 count bid)
    loser_ =
        case end of
            Invalid x -> x
            Challenger p ->
                if bidValid (game ^? running . bids . _head)
                    then p
                    else game ^. running . to playingOrder . _head
    finishedRound = RoundFinished end loser_ -- \$ game ^. running
    newRoundPunished =
        newRound $
            game ^. running . dices
                & (each %~ (fromIntegral . length))
                & (at loser_ %~ maybe Nothing (\x -> if x > 1 then Just $ x - 1 else Nothing))

step :: Map.Map PlayerId Move -> Arena -> Either PlayerId Arena
step moves arena =
    if arena' ^. runningGame . running . to playingOrder . to length == 1
        then Left $ fromMaybe "Imposible!" (arena' ^? runningGame . running . to playingOrder . _head)
        else Right arena'
  where
    arena' = arena & runningGame %~ stepGame moves

stepGame :: Map.Map PlayerId Move -> Game -> Game
stepGame moves game = case maybeRoundEnd of
    Just end ->
        finishRound end game
    Nothing ->
        game & running . bids %~ cons bid
  where
    -- Player on turn can't pass, others can't bid
    sorted =
        game ^. running . to playingOrder
            & map (\x -> (x, moves ^. at x))
            & (_head . _2 . filtered (is $ _Just . _Pass') .~ Nothing)
            & (_tail . each . _2 . filtered (is $ _Just . _Bid') .~ Nothing)
    maybeRoundEnd =
        Nothing
            <|> sorted ^? each . filtered (is _Nothing . snd) . _1 . to Invalid
            <|> sorted ^? each . filtered (is (_Just . _Challenge') . snd) . _1 . to Challenger
    bid = fromMaybe (error "Imposible!") (sorted ^? _head . _2 . _Just . _Bid')
