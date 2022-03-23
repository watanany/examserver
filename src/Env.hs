module Env (Env (..)) where

import RIO

data Env = Env
    { envLogFunc :: !LogFunc
    }

instance HasLogFunc Env where
    logFuncL = lens envLogFunc (\x y -> x{envLogFunc = y})
