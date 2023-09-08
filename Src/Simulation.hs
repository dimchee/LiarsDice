{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simulation where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe
import Debug.Trace qualified as Debug
import GHC.Generics (Generic)
import GHC.Natural
import System.Random.Shuffle (shuffleM)
import System.Random.Stateful (uniformRM)
import Prelude hiding (round)

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
    deriving (Show, Generic, Eq, Ord)

data Move
    = Bid' Bid
    | Challenge'
    | Pass'
    deriving (Generic, Show)
makePrisms ''Move
newtype Responses playerId = Responses (Map.Map playerId Move) deriving (Show, Generic)

data RoundEnd playerId = Challenger playerId | Invalid playerId
makePrisms ''RoundEnd
data Round playerId = Round
    { _initialPlayingOrder :: [playerId]
    , _dices :: Map.Map playerId Dice
    , _bids :: [Bid]
    , _responses :: [Responses playerId]
    }
    deriving (Show, Generic)
makeLenses ''Round
type RoundRunning = Round
data RoundFinished playerId = RoundFinished
    { _roundEnd :: RoundEnd playerId
    , _loser :: playerId
    , _getRound :: Round playerId
    }
makeLenses ''RoundFinished

data Game playerId = Game
    { _finished :: [RoundFinished playerId]
    , _running :: RoundRunning playerId
    }
makeLenses ''Game

data Status playerId = Running (Game playerId) | Finished (Game playerId) playerId
makePrisms ''Status

newRound :: Map.Map playerId Count -> Rand StdGen (Round playerId)
newRound diceCounts =
    Round <$> shuffleM (Map.keys diceCounts) <*> generateDice diceCounts <*> pure [] <*> pure []
newGame :: Ord playerId => Count -> NonEmpty playerId -> Rand StdGen (Game playerId)
newGame diceNumber (p :| players) = Game [] <$> newRound (Map.fromList $ (,diceNumber) <$> p : players)

getGame :: Status playerId -> Game playerId
getGame stats = case stats of
    Running game -> game
    Finished game _ -> game

generateDice :: Map.Map playerId Count -> Rand StdGen (Map.Map playerId Dice)
generateDice = mapM (\x -> replicateM (fromIntegral x) getRandom)

playingOrder :: Round playerId -> [playerId]
playingOrder round = waitingToPlay ++ played
  where
    initialOrder = round ^. initialPlayingOrder
    moveNumber = round ^. bids . to length
    (played, waitingToPlay) = splitAt (moveNumber `mod` length initialOrder) initialOrder

finishRound :: Ord playerId => RoundEnd playerId -> Game playerId -> Rand StdGen (Game playerId)
finishRound end game = do
    newRoundPunished <-
        newRound $
            (game ^. running . dices)
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
                    then fromMaybe (error "No players?") $ game ^? running . to playingOrder . _head
                    else p
    finishedRound = RoundFinished end loser_ $ game ^. running

step :: Ord playerId => Responses playerId -> Game playerId -> Rand StdGen (Status playerId)
step (Responses moves) game = do
    game' <- stepGame moves game
    pure $ case game' ^. running . to playingOrder of
        [winner] -> Finished game' winner
        _ -> Running game'

byPlayingOrder :: Ord playerId => Game playerId -> Map.Map playerId a -> [(playerId, Maybe a)]
byPlayingOrder game m = game ^. running . to playingOrder & map (\x -> (x, m ^. at x))

stepGame :: Ord playerId => Map.Map playerId Move -> Game playerId -> Rand StdGen (Game playerId)
stepGame moves game = case maybeRoundEnd of
    Just end ->
        finishRound end game
    Nothing ->
        pure $ game & running . bids %~ cons bid
  where
    -- Player on turn can't pass, others can't bid
    sorted =
        byPlayingOrder game moves
            & (_head . _2 . filtered (is $ _Just . _Pass') .~ Nothing)
            & (_tail . each . _2 . filtered (is $ _Just . _Bid') .~ Nothing)
    bid = fromMaybe (error "Imposible!") (sorted ^? _head . _2 . _Just . _Bid')
    maybeRoundEnd =
        Nothing
            <|> sorted ^? each . filtered (is _Nothing . snd) . _1 . to Invalid
            <|> sorted ^? each . filtered (is (_Just . _Challenge') . snd) . _1 . to Challenger
            <|> case game ^? running . bids . _head of
                Just lastBid -> if lastBid < bid then Nothing else Invalid <$> sorted ^? _head . _1
                Nothing -> Nothing
