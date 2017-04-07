{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nanochain where

import Protolude hiding (get, put)

import Crypto.Hash

import Control.Concurrent (MVar)
import Control.Arrow ((&&&))

import Data.Aeson hiding (json)
import Data.Int (Int64)
import Data.List (unzip, nub)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T

import qualified Data.Serialize as S
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as BS64
import Data.Text.Encoding (decodeUtf8)

import Web.Scotty

import Network.Socket (HostName, PortNumber)

import qualified Multicast as M
import qualified Prelude as P

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Index      = Integer
type Hash       = ByteString
type Timestamp  = Integer
type BlockData  = ByteString
type Blockchain = [Block]

data Block = Block
  { bindex       :: Index        -- ^ Block height
  , previousHash :: Hash         -- ^ Previous block hash
  , timestamp    :: Timestamp    -- ^ Creation timestamp
  , bdata        :: BlockData    -- ^ Block data
  , nonce        :: Int64        -- ^ Nonce for Proof-of-Work
  , bhash        :: Hash         -- ^ Block hash
  } deriving (Eq, Generic, S.Serialize)

instance Show Block where
  show (Block index pHash ts blockData n bHash) =
    "Block {" ++
    "  bindex: " ++ show index ++
    "  previousHash: " ++ T.unpack (encode64 pHash) ++
    "  timestamp: " ++ show ts ++
    "  bdata: " ++ T.unpack (encode64 blockData) ++
    "  n: " ++ show n ++
    "  bHash: " ++ T.unpack (encode64 bHash)

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

sha3_256 :: ByteString -> ByteString
sha3_256 = BA.convert . hashWith SHA3_256

calculateHash
  :: Index
  -> Hash
  -> Timestamp
  -> BlockData
  -> Int64
  -> ByteString
calculateHash i p t d n =
  sha3_256 $ BS.concat [B8.pack (show i), p, B8.pack (show t), d, B8.pack (show n)]

calculateHashForBlock :: Block -> Hash
calculateHashForBlock (Block i ph ts bd n _) = calculateHash i ph ts bd n

-------------------------------------------------------------------------------
-- Block Building
-------------------------------------------------------------------------------

now :: IO Integer
now = round `fmap` getPOSIXTime

genesisBlock :: Block
genesisBlock = Block index previousHash' timestamp' bdata' nonce' hash'
  where
    index         = 0
    previousHash' = "0" -- empty for genesis block
    timestamp'    = 0
    bdata'        = "<<Genesis block data>>"
    nonce'        = 0
    hash'         = calculateHash index previousHash' timestamp' bdata' nonce'

-- | Generates (mines) a new block using the `proofOfWork` function
generateNextBlock :: Block -> Timestamp -> BlockData -> Block
generateNextBlock previousBlock ts blockData =
  let index        = bindex previousBlock + 1
      previousHash' = bhash previousBlock
      nonce' = proofOfWork index previousHash' ts blockData
      blockHash = calculateHash index previousHash' ts blockData nonce'
  in Block index previousHash' ts blockData nonce' blockHash

proofOfWork
  :: Index
  -> Hash
  -> Timestamp
  -> BlockData
  -> Int64
proofOfWork idx prevHash ts bdata' = calcNonce 0
  where
    dbits = round $ logBase (2 :: Float) $ fromIntegral idx
    prefix = toS $ replicate dbits '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        hash' = calculateHash idx prevHash ts bdata' n
        prefix' = T.take dbits $ encode64 hash'

isValidBlock :: Block -> Block -> Maybe Text
isValidBlock previousBlock newBlock
  | bindex previousBlock + 1       /= bindex newBlock       = Just "Index is invalid"
  | bhash previousBlock            /= previousHash newBlock = Just "PreviousHash is invalid"
  | calculateHashForBlock newBlock /= bhash newBlock        = Just "Hash is invalid"
  | otherwise                                               = Nothing

isValidChain :: Blockchain -> Maybe Text
isValidChain (x0:x1:xs) = isValidBlock x1 x0 <|> isValidChain (x1:xs)
isValidChain [_] = Nothing
isValidChain [] = Just "Empty chain"

emptyBlockchain :: Blockchain
emptyBlockchain = []

addBlock :: Block -> Blockchain -> Blockchain
addBlock _ [] = []
addBlock b (pb:bs)
  | isNothing (isValidBlock pb b) = b : pb : bs
  | otherwise = pb : bs

addBlockMVar :: Block -> MVar Blockchain -> IO ()
addBlockMVar b = flip modifyMVar_ (return . addBlock b)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Block where
  toJSON (Block i ph t d n h) =
    object [ "bindex"       .= i
           , "previousHash" .= encode64 ph
           , "timestamp"    .= toJSON t
           , "bdata"        .= encode64 d
           , "nonce"        .= toJSON n
           , "bhash"        .= encode64 h
           ]

instance FromJSON Block where
  parseJSON (Object o) =
    Block <$>  o .: "bindex"
          <*> (o .: "previousHash" >>= decode64)
          <*> (o .: "timestamp"    >>= pure)
          <*> (o .: "bdata"        >>= decode64)
          <*> (o .: "nonce"        >>= pure)
          <*> (o .: "bhash"        >>= decode64)
  parseJSON _ = mzero

encode64 :: ByteString -> Text
encode64 = decodeUtf8 . BS64.encode

decode64 :: (Monad m) => Text -> m ByteString
decode64 = either (panic . toS) pure . BS64.decode . toS

-------------------------------------------------------------------------------
-- P2P
-------------------------------------------------------------------------------

data Msg
  = QueryLatestBlock
  | QueryBlockchain
  | RespBlockchain Blockchain
  | RespLatestBlock Block
  deriving (Eq, Show, Generic, S.Serialize)

type Sender = Msg -> IO ()

-- | Initializes a p2p node and returns a sender function so that
--   the http server can broadcast messages to the p2p network
p2p :: HostName -> PortNumber -> MVar Blockchain -> IO Sender
p2p hostname p2pPort' chain = do -- TODO: Take buffer size as argument, max size of blockchain
  (nodeReceiver, nodeSender) <- M.initMulticast hostname p2pPort' 65536
  prefix <- mkThreadDebugPrefix "p2p"
  void $ forkIO $ forever $ nodeReceiver >>= -- | Forever handle messages
    either (printWithPrefix prefix) (msgHandler nodeSender chain . fst)
  return nodeSender

-- | Main dispatch function to handle all messages received from network
msgHandler :: Sender -> MVar Blockchain -> Msg -> IO ()
msgHandler sender chain msg' = do
  prefix <- mkThreadDebugPrefix "msgHandler"
  printWithPrefix prefix $ "Received Msg: " <> (show msg' :: Text)
  case msg' of
    QueryLatestBlock -> do
      mBlock <- getLatestBlock chain
      case mBlock of
        Nothing -> printWithPrefix prefix "no block to return"
        Just block -> sender $ RespLatestBlock block
    QueryBlockchain -> sender . RespBlockchain =<< readMVar chain
    RespLatestBlock block -> do
      mMsg <- handleResponse chain [block]
      forM_ mMsg sender
    RespBlockchain blockchain -> do
      mMsg <- handleResponse chain blockchain
      forM_ mMsg sender

handleResponse :: MVar Blockchain -> Blockchain -> IO (Maybe Msg)
handleResponse chain chainResponse = do
  prefix <- mkThreadDebugPrefix "handleBlockchainResponse"
  case head chainResponse of
    Nothing -> do
      printWithPrefix prefix "Empty response chain..."
      return Nothing
    Just latestBlockRec -> do
      mLatestBlockHeld <- getLatestBlock chain
      case mLatestBlockHeld of
        Nothing -> do
          printWithPrefix prefix "Empty local chain..."
          return Nothing
        Just latestBlockHeld
          | bindex latestBlockRec > bindex latestBlockHeld -> do
              printWithPrefix prefix "Local chain potentially behind..."
              respond latestBlockRec latestBlockHeld
          | otherwise -> do
              printWithPrefix prefix "received chain is not longer than local."
              return Nothing
  where
    -- respond :: Block -> Block -> IO (Maybe Msg)
    respond latestBlockRec latestBlockHeld
      | bhash latestBlockHeld == previousHash latestBlockRec = do
          addBlockMVar latestBlockRec chain
          return $ Just $ RespLatestBlock latestBlockRec
      | length chainResponse == 1 = return $ Just QueryBlockchain
      | otherwise = do
          setChain chain chainResponse
          return $ Just $ RespLatestBlock latestBlockRec

-- | Returns Nothing if chain should be replaced
replaceChain :: Blockchain -> Blockchain -> Maybe Text
replaceChain oldChain newChain = case isValidChain newChain of
  Nothing
    | length newChain > length oldChain -> Nothing
    | otherwise -> Just "replaceChain: invalid chain"
  Just err -> Just err

-- | Replaces local block chain if new chain is longer
setChain :: MVar Blockchain -> Blockchain -> IO ()
setChain chain newChain = modifyMVar_ chain $ \oldChain ->
  case replaceChain oldChain newChain of
    Nothing -> return newChain
    Just err -> putStrLn err >> return oldChain

-- | Queries latest block from peers
connectToPeers :: Sender -> IO ()
connectToPeers nodeSender = nodeSender QueryLatestBlock

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

-- | Get the latest block from the chain
getLatestBlock :: MVar Blockchain -> IO (Maybe Block)
getLatestBlock chain = head <$> readMVar chain

-- | Generate a new block in the chain
mineAndAppendBlock :: MVar Blockchain -> Maybe Block -> IO (Maybe Block)
mineAndAppendBlock _ Nothing = pure Nothing
mineAndAppendBlock chain (Just latestBlock) = modifyMVar chain $ \chain' -> do
  ts <- now
  let newBlock = generateNextBlock latestBlock ts "" -- XXX: empty for now
  let newChain = addBlock newBlock chain'
  return (newChain, Just newBlock)

-- | Starts httpServer for interaction via HTTP
httpServer :: Peer -> MVar [Peer] -> MVar Blockchain -> Sender -> IO ()
httpServer (Peer hostName' httpPort' p2pPort') peersMV chainMV p2pSender =

  scotty httpPort' $ do

    get "/blocks" $ do
      blks <- liftIO $ readMVar chainMV
      json blks

    get "/mineBlock" $ do
      blk <- liftIO $ getLatestBlock chainMV
      mNewBlock <- liftIO $ mineAndAppendBlock chainMV blk
      case mNewBlock of
        Nothing -> text "error mining block"
        Just newBlock -> do
          putStrLn $ "adding block with hash: " <> encode64 (bhash newBlock)
          liftIO $ p2pSender $ RespLatestBlock newBlock
          json newBlock

    get "/peers" $ do
      peers <- liftIO $ readMVar peersMV
      json peers

    -- | Should be POST but can't query with browser unless GET
    get "/addPeer/:httpPort" $ do
      newHttpPort <- param "httpPort"
      portsInUse <- liftIO $ getPortsInUse peersMV
      let errMsg = "Port " <> show newHttpPort <> " in use."
      if newHttpPort `elem` portsInUse
         then text errMsg
      else do
        let newPeer = Peer hostName' newHttpPort p2pPort'
        void $ liftIO $ forkIO $
          initNode peersMV newPeer
        liftIO $ modifyMVar_ peersMV $ return . (:) newPeer
        json newPeer

-- | Initializes a node on the network with it's own copy of
--   the blockchain, and invokes a p2p server and an http server
--   listening on `p2pPort` and `httpPort` respectively.
initNode :: MVar [Peer] -> Peer -> IO ()
initNode peersMV peer@(Peer hostName' _ p2pPort') = do
  chainMV <- newMVar [genesisBlock]
  nodeSender <- p2p hostName' p2pPort' chainMV
  connectToPeers nodeSender
  httpServer peer peersMV chainMV nodeSender

-- | Initializes all default nodes
initNodes :: MVar [Peer] -> IO ()
initNodes peersMV = do
  peers <- readMVar peersMV
  forM_ peers $ \peer ->
    forkIO $ initNode peersMV peer

type HttpPort = Int
type P2PPort = PortNumber
data Peer = Peer
  { hostName :: HostName
  , httpPort :: HttpPort
  , p2pPort  :: P2PPort
  }

instance ToJSON Peer where
  toJSON (Peer hn hp pp) =
    object [ "hostName" .= toJSON hn
           , "httpPort" .= toJSON hp
           , "p2pPort"  .= (fromIntegral pp :: Int)
           ]

defaultPeers :: IO (MVar [Peer])
defaultPeers = newMVar peers
  where
    p2pPort' = 8000
    httpPorts = [3000,3001,3002]
    peers = Peer "224.0.0.1" <$> httpPorts <*> [p2pPort']

-- | Returns ports in use by the blockchain network
getPortsInUse :: MVar [Peer] -> IO [Int]
getPortsInUse peersMV = do
  peers <- readMVar peersMV
  let (httpPorts,p2pPorts) = unzip $
         map (httpPort &&& (fromIntegral . p2pPort)) peers
  return $ nub $ httpPorts ++ p2pPorts

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

main :: IO ()
main = do
  peersMV <- defaultPeers
  initNodes peersMV
  forever $ threadDelay 10000000

-------------------------------------
-- DEBUG
-------------------------------------

mkThreadDebugPrefix :: Text -> IO Text
mkThreadDebugPrefix funcName = do
  threadIdStr <- show <$> myThreadId
  return $ threadIdStr <> " - " <> funcName <> ": "

-- | Set `debug` to `True` to enable logging
printWithPrefix :: Text -> Text -> IO ()
printWithPrefix prefix
  | debug = print . (<>) prefix . show
  | otherwise = const $ return ()
  where -- |
    debug = True
