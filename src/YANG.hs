{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module YANG (
  YANG.parse
  ) where

import YANG.Internal

parse = YANG.Internal.parse
