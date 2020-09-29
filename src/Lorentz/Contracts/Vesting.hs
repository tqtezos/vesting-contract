-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Vesting where

import Data.Bool
import Data.Functor
import Control.Monad (Monad((>>=)))
import Data.Eq
import Data.Function
import Data.Maybe
import GHC.Enum
import System.IO
import Text.Show

import Lorentz
import Lorentz.Entrypoints ()
import Tezos.Core

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.IO.Utf8 as Utf8

type TokensPerTick = Natural
type SecondsPerTick = Natural
type VestedTicks = Natural

-- secondsPerTick must be non-zero
data VestingSchedule = VestingSchedule
  { epoch :: Timestamp
  , secondsPerTick :: SecondsPerTick
  , tokensPerTick :: TokensPerTick
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

mkVestingSchedule :: SecondsPerTick -> TokensPerTick -> IO VestingSchedule
mkVestingSchedule secondsPerTick' tokensPerTick' = do
  (\epoch' -> VestingSchedule epoch' secondsPerTick' tokensPerTick') <$> getCurrentTime

-- secondsPerTick must be non-zero
assertValidSchedule :: VestingSchedule -> VestingSchedule
assertValidSchedule xs@(VestingSchedule{..}) =
  bool
    (error "secondsPerTick must be non-zero")
    xs
    (secondsPerTick /= 0)

-- secondsPerTick must be non-zero

unVestingSchedule :: VestingSchedule & s :-> (Timestamp, (SecondsPerTick, TokensPerTick)) & s
unVestingSchedule = forcedCoerce_

secondsSinceEpoch :: Timestamp & s :-> Integer & s
secondsSinceEpoch = do
  now
  sub

ticksSinceEpoch :: forall s t. KnownValue t
  => VestingSchedule & t & s :-> Integer & t & s
ticksSinceEpoch = do
  unVestingSchedule
  dup
  car
  secondsSinceEpoch
  dip $ do
    cdr
    car
  ediv
  if IsSome
     then car
     else failWith

-- | The number of `ticksSinceEpoch` that have not been vested
unvestedTicks :: VestingSchedule & VestedTicks & s :-> Maybe Natural & s
unvestedTicks = do
  ticksSinceEpoch
  sub
  isNat

-- | Given a number of ticks to vest, assert that it's at most
-- `unvestedTicks` and return the updated number of `VestedTicks`
assertVestTicks :: Natural & VestingSchedule & VestedTicks & s :-> VestedTicks & s
assertVestTicks = do
  dip $ do
    dip dup
    unvestedTicks
  swap
  ifNone
    failWith
    (do
      -- unvested & to_vest
      dip dup
      -- unvested & to_vest & to_vest
      swap
      -- to_vest & unvested & to_vest
      if IsLe -- to_vest <= unvested
         then add
         else failWith
    )

vestTokens :: Natural & VestingSchedule & s :-> Natural & s
vestTokens = do
  dip $ do
    unVestingSchedule
    cdr
    cdr
  mul

data Storage st = Storage
  { wrapped :: st
  , vested :: VestedTicks
  , schedule :: VestingSchedule
  }
  deriving stock Generic

deriving stock instance (Show st) => Show (Storage st)
deriving anyclass instance (IsoValue st) => IsoValue (Storage st)

unStorage :: Storage st & s :-> (st, (VestedTicks, VestingSchedule)) & s
unStorage = forcedCoerce_

toStorage :: (st, (VestedTicks, VestingSchedule)) & s :-> Storage st & s
toStorage = forcedCoerce_

vestingContract :: forall st. () -- KnownValue st
  => (forall s. Natural & st & s :-> [Operation] & s)
  -> ContractCode Natural (Storage st)
vestingContract vest = do
  dup
  car
  dip $ do
    cdr
    unStorage
    dup
    dip $ do
      car
    cdr
    dup
    dip cdr
    dup
    cdr
    dip car
  dup
  dip $ do
    assertVestTicks
    dip dup
    swap
    dip $ do
      pair
      swap
      dup
  vestTokens
  vest
  dip $ do
    pair
    toStorage
  pair

data TezParameter
  = SetDelegate (Maybe KeyHash)
  | Vest Natural
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

data TezStorage = TezStorage
  { target :: Address
  , delegateAdmin :: Address
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

unTezStorage :: TezStorage & s :-> (Address, Address) & s
unTezStorage = forcedCoerce_

vestingTezContract :: ContractCode TezParameter (Storage TezStorage)
vestingTezContract = do
  unpair
  caseT @TezParameter
    ( #cSetDelegate /-> do
        swap
        dup
        dip $ do
          unStorage
          car
          unTezStorage
          cdr -- delegateAdmin
          sender
          if IsEq
             then do
               dip nil
               setDelegate
               cons
             else failWith
        swap
        pair
    , #cVest /-> do
        pair
        vestingContract $ do
          swap
          unTezStorage
          car -- target
          contract @()
          if IsNone
             then failWith
             else do
               swap
               push $ toEnum @Mutez 1
               mul
               unit
               transferTokens
               dip nil
               cons
    )


instance HasAnnotation VestingSchedule where
instance HasAnnotation TezStorage where
instance HasAnnotation (Storage TezStorage) where

instance ParameterHasEntrypoints TezParameter where
  type ParameterEntrypointsDerivation TezParameter = EpdPlain

-- | Print `permitAdmin42Contract`
--
-- @
--  printVestingTezContract (Just "contracts/vesting_tez.tz") False
-- @
printVestingTezContract :: Maybe FilePath -> Bool -> IO ()
printVestingTezContract mOutput forceOneLine' =
  maybe TL.putStrLn Utf8.writeFile mOutput $
  printLorentzContract forceOneLine' $
    (defaultContract vestingTezContract)
      { cDisableInitialCast = True }

initialVestedTicks :: VestedTicks
initialVestedTicks = 0

-- Tezos.Crypto.Orphans> A.printInitPermitAdmin42 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
-- Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
printInitVestingTezContract :: Address -> Address -> SecondsPerTick -> TokensPerTick -> IO ()
printInitVestingTezContract target adminAddr secondsPerTick' tokensPerTick' =
  mkVestingSchedule secondsPerTick' tokensPerTick' >>= (\schedule' ->
    TL.putStrLn $
      printLorentzValue @(Storage TezStorage) forceOneLine $
      Storage
        (TezStorage target adminAddr)
        initialVestedTicks
        (assertValidSchedule schedule')
  )
  where
    forceOneLine = True

