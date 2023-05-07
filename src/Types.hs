{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Default

type PID = Int

data CpuTimeEntry = CpuTimeEntry
  { _ctePid :: Integer,
    _cteCpuTime :: Integer
  }
  deriving (Eq, Show)

data OrderBy = OrderByPID | OrderByName | OrderByMemory | OrderByCPU | OrderByOwner
  deriving (Eq, Show)

data Process = Process
  { _pPid :: PID,
    _pOwner :: String,
    _pName :: String,
    _pMemory :: Integer,
    _pCpuTime :: Integer,
    _pPrevCpuTime :: Integer,
    _pCpuPerc :: Double
  }
  deriving (Eq, Show)

instance Default Process where
  def = Process 0 "" "" 0 0 0 0

data AppModel = AppModel
  { _amProcesses :: [Process],
    _amCpuTimeTotal :: Integer,
    _amPreviousCpuTimeTotal :: Integer,
    _amPreviousCpuTimes :: [CpuTimeEntry],
    _amOrderBy :: OrderBy,
    _amOrderAscending :: Bool,
    _amCpuHistory :: [Double],
    _amMemTotal :: Int,
    _amMemFree :: Int,
    _amMemHistory :: [Double]
  }
  deriving (Eq, Show)

instance Default AppModel where
  def =
    AppModel
      { _amProcesses = [],
        _amCpuTimeTotal = 0,
        _amPreviousCpuTimeTotal = 0,
        _amPreviousCpuTimes = [],
        _amOrderBy = OrderByCPU,
        _amOrderAscending = False,
        _amCpuHistory = map (const 0) [0 .. 100],
        _amMemTotal = 0,
        _amMemFree = 0,
        _amMemHistory = map (const 0) [0 .. 100]
      }

data AppEvent
  = AppInit
  | AppUpdate [Process]
  | AppTick ([Process], Integer, (Int, Int))
  | AppIncrease
  | AppReadProc
  | AppSortBy OrderBy
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel
makeLensesWith abbreviatedFields 'Process
makeLensesWith abbreviatedFields 'CpuTimeEntry
