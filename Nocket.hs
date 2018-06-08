-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [network bytestring stm])"

module Nocket
  ( foo
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Functor
import System.Posix.Types
import Network.Socket

-- | Perform the first action in the event of a timeout
--   that you provide (in milliseconds),
--   and the second action if your connection was OK.
foo :: Int    -- ^ Timeout in milliseconds
    -> Socket -- ^ Socket
    -> IO a   -- ^ IO action to perform in the event of a timeout
    -> IO a   -- ^ IO action to perform if you connected to the socket normally
    -> IO a
foo dt sock mTimedOut mOK = do
  (isReadyAction,deregister) <- threadWaitReadSTM (mySockFd sock)
  delay <- registerDelay dt
  isContentReady <- atomically $ (isReadyAction $> True) <|> (fini delay $> False)
  deregister
  if not isContentReady
  then mTimedOut
  else mOK

fini :: TVar Bool -> STM ()
fini = check <=< readTVar

mySockFd :: Socket -> Fd
mySockFd (MkSocket n _ _ _ _) = Fd n

