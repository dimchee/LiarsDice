{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface where

import Control.Applicative ((<|>))
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Network.Simple.TCP (Socket, recv)
import Simulation
import System.Timeout (timeout)
import Prelude hiding (round)

data ClientResponse content
    = ClientResponse content
    | JsonError
    | TimedOut
    | EndOfInput
    deriving (Show)
makePrisms ''ClientResponse

timedRecv :: JSON.FromJSON a => Socket -> IO (ClientResponse a)
timedRecv soc =
    maybe TimedOut (maybe EndOfInput (maybe JsonError ClientResponse . JSON.decodeStrict))
        -- TOOD magic numbers
        <$> timeout (3 * 1000000) (recv soc 1024)

newtype PlayerId = PlayerId String deriving (Eq, Ord, JSON.ToJSON, JSON.ToJSONKey)
instance Show PlayerId where
    show (PlayerId name) = name

instance Show (Responses PlayerId) where
    show (Responses resp) =
        "\n    [ "
            ++ List.intercalate "\n    , " (map (\(pid, move) -> show pid ++ " => " ++ show move) $ Map.toList resp)
            ++ "\n    ]"

newtype MoveId = MoveId String deriving (Generic, Show, FromJSON, Eq)
newtype NameWish = NameWish String
instance FromJSON NameWish where
    parseJSON (JSON.Object o) =
        NameWish <$> (o .: "name")
    parseJSON _ = mzero

data MoveChecked = MoveChecked MoveId Move
makePrisms ''MoveChecked

instance FromJSON MoveChecked where
    parseJSON (JSON.Object o) =
        MoveChecked
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

type GameNumber = Int

instance JSON.ToJSON (Game PlayerId) where
    toJSON game =
        JSON.object
            [ "winner" .= (game ^? running . to playingOrder . _head)
            , "rounds" .= (encodeRound <$> game ^. finished)
            ]
      where
        encodeRoundEnd rend = case rend of
            Challenger pid -> JSON.object ["challenger" .= pid]
            Invalid pid -> JSON.object ["invalid" .= pid]
        encodeBid bid = (fromEnum $ value bid, count bid)
        encodeRound round =
            JSON.object
                [ "loser" .= (round ^. loser)
                , "initialPlayingOrder" .= (round ^. getRound . initialPlayingOrder)
                , "dices" .= Map.map (fmap fromEnum) (round ^. getRound . dices)
                , "bids" .= (encodeBid <$> round ^. getRound . bids)
                , "roundEnd" .= encodeRoundEnd (round ^. roundEnd)
                ]

serverResponse :: GameNumber -> Game PlayerId -> MoveId -> PlayerId -> ByteString
serverResponse gameNumber game moveId playerId = do
    JSON.encode $
        JSON.object
            [ "message_id" .= unwrapMove moveId
            , "game_number" .= gameNumber -- (arena ^. finishedGames . to length)
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
                    emptyBid
                    (\bid -> (fromEnum $ value bid, count bid))
                    (round ^? bids . _head)
            , "last_bidder" .= maybeNotAvailable (round ^? to playingOrder . _head)
            , "last_loser" .= maybeNotAvailable (game ^? finished . _head . loser)
            , "last_challenger" .= maybeNotAvailable (game ^? finished . _head . roundEnd . _Challenger)
            ]
  where
    round = game ^. running
    unwrapMove (MoveId m) = m
    unwrap (PlayerId p) = p
    yourself = PlayerId "yourself"
    emptyBid = (1, 0)
    maybeNotAvailable = maybe "not_available" unwrap
