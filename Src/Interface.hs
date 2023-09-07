{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface where

import Control.Applicative ((<|>))
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Simulation
import Prelude hiding (round)

newtype MoveId = MoveId String deriving (Generic, Show, FromJSON, Eq)
newtype NameResponse = NameResponse String
instance FromJSON NameResponse where
    parseJSON (JSON.Object o) =
        NameResponse <$> (o .: "name")
    parseJSON _ = mzero

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

type GameNumber = Int

serverResponse :: GameNumber -> Game -> MoveId -> PlayerId -> ByteString
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
