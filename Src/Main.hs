{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Main where

-- import Debug.Trace (trace)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Lens.Extras (is)
import Control.Monad.Loops (iterateWhile, unfoldM, untilM_)
import Control.Monad.Random
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Interface
import Network.Simple.TCP
import Simulation hiding (responses)
import System.Timeout (timeout)

randomMoveId :: IO MoveId
randomMoveId = MoveId <$> replicateM 8 (randomRIO ('0', '9'))

data Arena = Arena
    { _finishedGames :: [Game PlayerId]
    , _runningGame :: Game PlayerId
    }
makeLenses ''Arena
newArena :: Count -> NonEmpty PlayerId -> Rand StdGen Arena
newArena diceNumber players = Arena [] <$> newGame diceNumber players

data Communication = Communication
    { _respChan :: TChan (PlayerId, ClientResponse MoveChecked)
    , _nextMoveChan :: TChan (Maybe (PlayerId -> ByteString))
    , _playerQueue :: TChan (Maybe PlayerId)
    , _connectingEnded :: TChan ()
    }
makeLenses ''Communication
newCommunication :: IO Communication
newCommunication =
    Communication <$> newTChanIO <*> newTChanIO <*> newTChanIO <*> newTChanIO

userCommunication :: Communication -> IO Communication
userCommunication com = do
    nextMove <- atomically $ dupTChan (com ^. nextMoveChan)
    pure $ com & nextMoveChan .~ nextMove

handlePlayer :: Communication -> PlayerId -> Socket -> IO ()
handlePlayer comm playerId soc = do
    _ <- iterateWhile (is _Just) $ do
        nextMove <- atomically $ readTChan $ comm ^. nextMoveChan
        flip (maybe (pure ())) nextMove $ \serverResp -> do
            sendLazy soc $ serverResp playerId
            clientResponse <- timedRecv soc
            -- putStrLn $ "Player responded: " ++ show clientResponse
            atomically $ writeTChan (comm ^. respChan) (playerId, clientResponse)
        pure nextMove
    pure ()

collectResponses :: MoveId -> Int -> TChan (PlayerId, ClientResponse MoveChecked) -> IO (Responses PlayerId)
collectResponses moveId nrPlayers respCh = go Set.empty (Responses Map.empty)
  where
    go ids resps = do
        if Set.size ids == nrPlayers
            then pure resps
            else do
                (playerId, response) <- atomically $ readTChan respCh
                -- putStrLn $ "Response: " ++ show response
                go (Set.insert playerId ids) $ addResp playerId response resps
    addResp :: PlayerId -> ClientResponse MoveChecked -> Responses PlayerId -> Responses PlayerId
    addResp playerId resp (Responses resps) =
        if resp ^? _ClientResponse . _MoveChecked . _1 == Just moveId
            then resps & at playerId .~ (resp ^? _ClientResponse . _MoveChecked . _2) & Responses
            else Responses resps

-- TODO Use https://wiki.haskell.org/State_Monad for SimulateState
simulate :: Communication -> IO ()
simulate comm = do
    players <- unfoldM $ do
        mplayer <- atomically $ readTChan $ comm ^. playerQueue
        maybe (pure ()) (\p -> putStrLn $ "Player '" ++ show p ++ "' connected") mplayer
        pure mplayer
    case players of
        [] -> putStrLn "No players connected, Aborting simulation"
        p : ps -> do
            putStrLn $ "Starting simulation. Connected players: " ++ show (p : ps)
            initialArena <- evalRandIO $ newArena 5 (p :| ps) -- TODO magic number 5
            tVarArena <- newTVarIO initialArena
            Finished game playerId <- iterateWhile (is _Running) $ do
                moveId <- randomMoveId
                arena <- readTVarIO tVarArena

                let resp = serverResponse (arena ^. finishedGames . to length) (arena ^. runningGame) moveId
                atomically $ writeTChan (comm ^. nextMoveChan) $ Just resp
                let activePlayers = arena ^. runningGame . running . to playingOrder . to length
                responses <- collectResponses moveId activePlayers $ comm ^. respChan
                -- putStrLn $ "Responses: " ++ prettyPrintResponses responses
                -- putStrLn $ "ActiveRound: " ++ show (arena ^. runningGame . running)
                status <- evalRandIO $ arena ^. runningGame . to (step responses)
                atomically $ writeTVar tVarArena (arena & runningGame .~ getGame status)
                pure status
            ByteString.writeFile "game.json" $ getJSON game
            putStrLn $ "Winner: " ++ show playerId
            atomically $ writeTChan (comm ^. nextMoveChan) Nothing
            pure ()

newPlayerId :: ClientResponse NameWish -> IO PlayerId
newPlayerId mConnectResponse = do
    randEnd <- replicateM 8 (randomRIO ('a', 'z'))
    pure $ PlayerId $ case mConnectResponse of
        ClientResponse (NameWish p) -> do
            p ++ "_" ++ randEnd
        _ -> randEnd

main :: IO ()
main = do
    communication <- newCommunication
    putStrLn "Waiting for players"
    putStrLn "Connect as bot at 'localhost:8888'"
    putStrLn "To start round run 'curl -s localhost:9000'"
    _ <- forkIO $ serve (Host "localhost") "9000" $ \(_, _) -> do
        atomically $ writeTChan (communication ^. playerQueue) Nothing
        atomically $ writeTChan (communication ^. connectingEnded) ()
    _ <- forkIO $ simulate communication
    listen (Host "localhost") "8888" $ \(lsoc, _) -> do
        flip untilM_ (atomically $ fmap not $ isEmptyTChan $ communication ^. connectingEnded)
        $ timeout (2 * 100000) -- TODO magic numbers
        $ acceptFork lsoc
        $ \(soc, _) -> do
            playerId <- timedRecv soc >>= newPlayerId
            atomically $ writeTChan (communication ^. playerQueue) $ Just playerId
            userCom <- userCommunication communication
            handlePlayer userCom playerId soc
