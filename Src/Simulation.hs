{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simulation where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Random
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe
import GHC.Generics (Generic)
import GHC.Natural
import System.Random.Shuffle (shuffleM)
import System.Random.Stateful (uniformRM)
import Prelude hiding (round)

newtype PlayerId = PlayerId String deriving (Eq, Ord)
instance Show PlayerId where
    show (PlayerId name) = name

type Count = Natural
data Face = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Enum, Generic, Show, Uniform, Random)
instance UniformRange Face where
    uniformRM (a, b) = fmap toEnum . uniformRM (fromEnum a, fromEnum b)

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
newtype Responses = Responses (Map.Map PlayerId Move)
instance Show Responses where
    show (Responses resp) =
        "\n    [ "
            ++ intercalate "\n    , " (map (\(pid, move) -> show pid ++ " => " ++ show move) $ Map.toList resp)
            ++ "\n    ]"

data RoundEnd = Challenger PlayerId | Invalid PlayerId
makePrisms ''RoundEnd
data Round = Round
    { _initialPlayingOrder :: [PlayerId]
    , _dices :: Map.Map PlayerId Dice
    , _bids :: [Bid]
    }
    deriving (Show)
makeLenses ''Round
type RoundRunning = Round
data RoundFinished = RoundFinished
    { _roundEnd :: RoundEnd
    , _loser :: PlayerId
    , _getRound :: Round
    }
makeLenses ''RoundFinished

data Game = Game
    { _finished :: [RoundFinished]
    , _running :: RoundRunning
    }
makeLenses ''Game

data Status = Running Game | Finished Game PlayerId
makePrisms ''Status

newRound :: Map.Map PlayerId Count -> Rand StdGen Round
newRound diceCounts = Round <$> shuffleM (Map.keys diceCounts) <*> generateDice diceCounts <*> pure []
newGame :: Count -> NonEmpty PlayerId -> Rand StdGen Game
newGame diceNumber (p :| players) = Game [] <$> newRound (Map.fromList $ (,diceNumber) <$> p : players)

getGame :: Status -> Game
getGame stats = case stats of
    Running game -> game
    Finished game _ -> game

generateDice :: Map.Map PlayerId Count -> Rand StdGen (Map.Map PlayerId Dice)
generateDice = mapM (\x -> replicateM (fromIntegral x) getRandom)

playingOrder :: Round -> [PlayerId]
playingOrder round = waitingToPlay ++ played
  where
    initialOrder = round ^. initialPlayingOrder
    moveNumber = round ^. bids . to length
    (played, waitingToPlay) = splitAt (moveNumber `mod` length initialOrder) initialOrder

finishRound :: RoundEnd -> Game -> Rand StdGen Game
finishRound end game = do
    newRoundPunished <-
        newRound $
            game ^. running . dices
                & (each %~ (fromIntegral . length))
                & (at loser_ %~ maybe Nothing (\x -> if x > 1 then Just $ x - 1 else Nothing))
    pure $ game & (finished %~ cons finishedRound) & (running .~ newRoundPunished)
  where
    bidValid (Bid cnt val) =
        (game ^. running . dices . each & filter (== val) & length) <= fromIntegral cnt
    loser_ =
        case end of
            Invalid x -> x
            Challenger p ->
                if maybe False bidValid (game ^? running . bids . _head)
                    then fromMaybe (PlayerId "No players?") $ game ^? running . to playingOrder . _head
                    else p
    finishedRound = RoundFinished end loser_ $ game ^. running

step :: Responses -> Game -> Rand StdGen Status
step (Responses moves) game = do
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
