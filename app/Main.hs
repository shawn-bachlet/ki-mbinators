module Main where

import Control.Exception (SomeException, bracket, bracketOnError, try)
import Control.Exception.Base (MaskingState(MaskedInterruptible))
import Control.Monad (forever)
import Data.Functor (void)
import GHC.Conc (atomically)
import GHC.IO (getMaskingState, mask)
import GHC.IO.Handle (BufferMode(LineBuffering), Handle, hGetLine)
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import Ki (ThreadOptions(maskingState), Scope, Thread, defaultThreadOptions, fork, forkWith, scoped)
import Network.Socket
  ( AddrInfo(..), AddrInfoFlag(AI_PASSIVE), SocketOption(ReuseAddr), SocketType(Stream), accept
  , bind, close, defaultHints, getAddrInfo, gracefulClose, listen, mkSocket, openSocket
  , setCloseOnExecIfNeeded, setSocketOption, socketToHandle, withFdSocket, withSocketsDo
  )
import System.IO (hClose, hPrint, hPutStrLn, hSetBuffering)
import Text.Printf (printf)

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = scoped $ \scope ->
      forever $ bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $ do
            h <- socketToHandle conn ReadWriteMode
            forkFinally
              scope
              (talk h)
              (const $ gracefulClose conn 5000)

port :: String
port = "44444"

talk :: Handle -> IO ()
talk h =
  do
    hSetBuffering h LineBuffering
    loop
  where
    loop =
      do
        line <- hGetLine h
        if line == "end"
        then hPutStrLn h "Thank you for using the Haskell doubling service."
        else do
          hPrint h (2 * (read line :: Integer))
          loop

forkFinally :: Scope -> IO a -> (Either SomeException a -> IO b) -> IO (Thread b)
forkFinally scope action and_then =
  mask $ \restore -> do
    m <- getMaskingState
    -- Since Ki's fork is not a GHC primative the masking state is not inherited
    -- from the call to mask it must be set manually
    forkWith
      scope
      (defaultThreadOptions {maskingState = m} )
      $ try (restore action) >>= and_then
