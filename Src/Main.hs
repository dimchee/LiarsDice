{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Main where

-- import Debug.Trace (trace)

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Lens.Extras (is)
import Control.Monad.Loops (iterateWhile, unfoldM)
import Control.Monad.Random
import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Network.Simple.TCP
import Simulation
import System.Timeout (timeout)
import Prelude hiding (round)

newtype MoveId = MoveId String deriving (Generic, Show, FromJSON, Eq)
randomMoveId :: IO MoveId
randomMoveId = MoveId <$> replicateM 8 (randomRIO ('0', '9'))

data ClientResponse
    = ClientResponse MoveId Move
    | JsonError
    | TimedOut
    | EndOfInput
    deriving (Show)
makePrisms ''ClientResponse

instance FromJSON ClientResponse where
    parseJSON (JSON.Object o) =
        ClientResponse
            <$> o
            .: "message_id"
            <*> ((o .: "move" >>= stringMove) <|> (bidMove <$> o .: "move"))
      where
        stringMove x = case x of
            "pass" -> pure Pass'
            "challenge" -> pure Challenge'
            s -> fail $ "Should be one of 'challenge' or 'pass', not: '" <> s <> "'"
        bidMove :: (Int, Int) -> Move
        bidMove (face, count) = Bid' $ Bid{count = fromIntegral count, value = toEnum $ face - 1}
    parseJSON _ = mzero

data Arena = Arena
    { _finishedGames :: [Game]
    , _runningGame :: Game
    }
makeLenses ''Arena
newArena :: Count -> [PlayerId] -> Rand StdGen Arena
newArena diceNumber players = Arena [] <$> newGame diceNumber players

data Communication = Communication
    { _respChan :: TChan (PlayerId, ClientResponse)
    , _nextMoveChan :: TChan (MoveId, Arena)
    , _playerQueue :: TChan (Maybe PlayerId)
    }
makeLenses ''Communication
newCommunication :: IO Communication
newCommunication =
    Communication <$> newTChanIO <*> newTChanIO <*> newTChanIO

userCommunication :: Communication -> IO Communication
userCommunication com = do
    nextMove <- atomically $ dupTChan (com ^. nextMoveChan)
    pure $ com & nextMoveChan .~ nextMove

serverResponse :: Arena -> MoveId -> PlayerId -> ByteString
serverResponse arena moveId playerId = do
    JSON.encode $
        JSON.object
            [ "message_id" .= unwrapMove moveId
            , "game_number" .= (arena ^. finishedGames . to length)
            , "round_number" .= (game ^. finished . to length)
            , "move_number" .= (round ^. bids . to length)
            , "your_hand" .= maybe [] (map $ (+ 1) . fromEnum) (round ^. dices . at playerId)
            , "other_hands"
                .= ( round ^. dices . to Map.toList
                        & (each . _2 %~ length)
                        & (each . filtered ((== playerId) . fst) . _1 .~ yourself)
                        & (each . _1 %~ unwrap)
                   )
            , "last_move" .= ("first_move" :: String)
            , "last_bid"
                .= maybe
                    (0, 0)
                    (\bid -> (fromEnum $ value bid, count bid))
                    (round ^? bids . _head)
            , "last_bidder" .= maybeNotAvailable (round ^? to playingOrder . _head)
            , "last_loser" .= maybeNotAvailable (game ^? finished . _head . loser)
            , "last_challenger" .= maybeNotAvailable (game ^? finished . _head . roundEnd . _Challenger)
            ]
  where
    game = arena ^. runningGame
    round = game ^. running
    unwrapMove (MoveId m) = m
    unwrap (PlayerId p) = p
    yourself = PlayerId "yourself"
    maybeNotAvailable = maybe "not_available" unwrap

turn :: Communication -> PlayerId -> Socket -> IO ()
turn comm playerId soc = forever $ do
    (moveId, game) <- atomically $ readTChan $ comm ^. nextMoveChan
    sendLazy soc $ serverResponse game moveId playerId
    clientResponse <- timeout (3 * 1000000) (recv soc 1024) -- TODO magic numbers
    -- putStrLn $ "Player responded: " ++ show clientResponse
    atomically $
        writeTChan
            (comm ^. respChan)
            ( playerId
            , maybe TimedOut (maybe EndOfInput (fromMaybe JsonError . JSON.decodeStrict)) clientResponse
            )

collectResponses :: MoveId -> Int -> TChan (PlayerId, ClientResponse) -> IO (Map.Map PlayerId Move)
collectResponses moveId nrPlayers respCh = go Set.empty Map.empty
  where
    go ids resps = do
        if Set.size ids == nrPlayers
            then pure resps
            else do
                (playerId, response) <- atomically $ readTChan respCh
                putStrLn $ "Response: " ++ show response
                go (Set.insert playerId ids) $ addResp playerId response resps
    addResp :: PlayerId -> ClientResponse -> Map.Map PlayerId Move -> Map.Map PlayerId Move
    addResp playerId resp resps =
        if resp ^? _ClientResponse . _1 == Just moveId
            then resps & at playerId .~ (resp ^? _ClientResponse . _2)
            else resps

-- TODO Use https://wiki.haskell.org/State_Monad for SimulateState
simulate :: Communication -> IO ()
simulate comm = do
    -- TODO handle empty players, should not happen (`newGame`)
    players <- atomically $ unfoldM (readTChan $ comm ^. playerQueue)
    putStrLn $ "Starting simulation. Connected players: " ++ show players
    -- TODO magic number 5
    initialArena <- evalRandIO $ newArena 5 players
    tVarArena <- newTVarIO initialArena
    _ <- iterateWhile (is _Running) $ do
        moveId <- randomMoveId
        arena <- readTVarIO tVarArena
        atomically $ writeTChan (comm ^. nextMoveChan) (moveId, arena)
        let activePlayers = arena ^. runningGame . running . to playingOrder . to length
        responses <- collectResponses moveId activePlayers $ comm ^. respChan
        putStrLn $ "Ending round... responses: " ++ show responses
        -- randomId <- randomIO
        status <- evalRandIO $ arena ^. runningGame . to (step responses)
        atomically $ writeTVar tVarArena (arena & runningGame .~ getGame status)
        putStrLn $ "Winner: " ++ show (status ^? _Finished . _2)
        pure status
    pure ()

main :: IO ()
main = do
    communication <- newCommunication
    _ <- forkIO $ simulate communication
    _ <- forkIO $ serve (Host "localhost") "9000" $ \(_, _) -> do
        atomically $ writeTChan (communication ^. playerQueue) Nothing
    serve (Host "localhost") "8888" $ \(soc, _) -> do
        -- putStrLn $ "Player connected: " ++ show remoteAddr
        -- TODO playerId should be conncted to other stuff
        playerId <- PlayerId <$> replicateM 8 (randomRIO ('a', 'z'))
        atomically $ writeTChan (communication ^. playerQueue) $ Just playerId
        userCom <- userCommunication communication
        turn userCom playerId soc
