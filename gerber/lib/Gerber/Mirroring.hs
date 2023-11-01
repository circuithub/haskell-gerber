module Gerber.Mirroring (Mirroring (..)) where


data Mirroring
  = MirrorNone
  | MirrorX
  | MirrorY
  | MirrorXY
  deriving (Eq, Show)
