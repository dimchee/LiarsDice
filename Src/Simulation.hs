{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- TODO delete this option
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Simulation where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Random
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)
import GHC.Natural
import System.Random.Shuffle (shuffleM)
import Prelude hiding (round)

newtype PlayerId = PlayerId String deriving (Show, Eq, Ord)
type Count = Natural
data Face = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Enum, Generic, Show, Uniform, UniformRange, Random)
type Dice = [Face]

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
    , _dices :: Map.Map PlayerId Dice
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

-- data Status m end = Running m | Finished m end
data Status = Running Game | Finished Game PlayerId
makePrisms ''Status

getGame :: Status -> Game
getGame stats = case stats of
    Running game -> game
    Finished game _ -> game

generateDice :: Map.Map PlayerId Count -> Rand StdGen (Map.Map PlayerId Dice)
generateDice = mapM (\x -> replicateM (fromIntegral x) getRandom)
newRound :: Map.Map PlayerId Count -> Rand StdGen Round
newRound diceCounts = Round <$> shuffleM (Map.keys diceCounts) <*> generateDice diceCounts <*> pure []

newGame :: Count -> [PlayerId] -> Rand StdGen Game
newGame diceNumber players = Game [] <$> newRound (Map.fromList $ (,diceNumber) <$> players)

moveNumber :: Round -> Sum Int
moveNumber round = round ^. bids . to length . to Sum
playingOrder :: Round -> [PlayerId]
playingOrder round = round ^. initialPlayingOrder . to (rotate $ moveNumber round)
rotate :: Sum Int -> [a] -> [a]
rotate (Sum n) as = ys ++ xs
  where
    (xs, ys) = splitAt (n `mod` length as) as

finishRound :: RoundEnd -> Game -> Rand StdGen Game
finishRound end game = do
    newRoundPunished <-
        newRound $
            game ^. running . dices
                & (each %~ (fromIntegral . length))
                & (at loser_ %~ maybe Nothing (\x -> if x > 1 then Just $ x - 1 else Nothing))
    pure $
        game
            & (finished %~ cons finishedRound)
            & (running .~ newRoundPunished)
  where
    bidValid bid =
        -- case bid of
        -- Nothing -> True
        -- Just (Bid _ val) ->
        --     game ^.. running . dices . filtered ()
        (game ^. running . dices . to (length . filter (\x -> maybe False (\b -> x == value b) bid) . concatMap snd . Map.toList))
            >= fromIntegral (maybe 0 count bid)
    loser_ =
        case end of
            Invalid x -> x
            Challenger p ->
                if bidValid (game ^? running . bids . _head)
                    then p
                    else fromMaybe (PlayerId "No players?") $ game ^? running . to playingOrder . _head
    finishedRound = RoundFinished end loser_ -- \$ game ^. running

step :: Map.Map PlayerId Move -> Game -> Rand StdGen Status
step moves game = do
    game' <- stepGame moves game
    pure $ case game' ^. running . to playingOrder of
        [winner] -> Finished game' winner
        _ -> Running game'

stepGame :: Map.Map PlayerId Move -> Game -> Rand StdGen Game
stepGame moves game = case maybeRoundEnd of
    Just end ->
        finishRound end game
    Nothing ->
        pure $ game & running . bids %~ cons bid
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
