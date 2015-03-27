{-# LANGUAGE Safe #-}
-- | This module parses files using the syntax demonstrated below.
-- The full grammar is available in the Happy source file.
--
-- @
-- -- Line comments until newline
-- layout:
--   based:
--     configuration:
--       {} -- empty section
--
--     sections:
--      "glguy"
--
--     booleans   : yes
--     complicated: no
--
--     decimal    : -1234
--     hexadecimal: 0x1234
--     octal      : 0o1234
--     binary     : 0b1010
--
-- lists:
--    * 1
--    * [ "inline", "lists" ]
--    * * "nestable"
--      * "layout"
--      * "lists"
--    * 3
--
-- unicode : "standard Haskell format strings (1 ≤ 2)\x2228(2 ≤ 3)"
-- @
module Config
  ( Section(..)
  , Value(..)
  , parse
  , pretty
  ) where

import Config.Value
import Config.Parser
import Config.Pretty
