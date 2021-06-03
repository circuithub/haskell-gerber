{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}

module Gerber.Evaluate.GraphicsState.StackStream
  ( StackStream
  , push
  , add
  , pop
  , top
  , null
  , toLists
  ) where

import Data.Monoid
import Prelude hiding (null)

-- | A stack of stream elements with each stack level having a name.
--
-- Used in "Gerber.Evaluate" to keep track of block aperture definitions.
--newtype StackStream stackId streamElem = StackStream (Dual (Endo [(stackId, Endo [streamElem])] )) deriving (Semigroup, Monoid)

data StackStream stackId streamElem
  = Empty
  | Push stackId (Endo [streamElem]) (StackStream stackId streamElem)
  | Pop (StackStream stackId streamElem)
  | Add (Endo [streamElem])

instance (Eq stackId, Eq streamElem) => Eq (StackStream stackId streamElem) where
  -- | Equality is defined in terms of 'toLists'
  a == b = toLists a == toLists b

instance (Show stackId, Show streamElem) => Show (StackStream stackId streamElem) where
  show = \case
    Empty -> "Empty"
    Push stackId es rest -> "Push ( " ++ show stackId ++ " ) " ++ show (appEndo es []) ++ "( " ++ show rest ++ " )"
    Pop rest -> "Pop ( " ++ show rest ++ " ) "
    Add es -> "Add " ++ show (appEndo es [])

instance Semigroup (StackStream stackId streamElem) where
  Empty                        <> rhs                          = rhs -- 4
  lhs                          <> Empty                        = lhs -- 3
  ---------------------------------------------------------------------
  Pop lhs                      <> rhs                          = Pop (lhs <> rhs) -- 3
  ---------------------------------------------------------------------
  Push idLHS elemsLHS lowerLHS <> Push idRHS elemsRHS lowerRHS = Push idRHS elemsRHS (Push idLHS elemsLHS lowerLHS <> lowerRHS)
  Push _ _ lhs                 <> Pop rhs                      = lhs <> rhs
  Push idLHS elemsLHS lowerLHS <> Add rhs                      = Push idLHS (elemsLHS <> rhs) lowerLHS
  ---------------------------------------------------------------------
  Add lhs                      <> Add rhs                      = Add (lhs <> rhs)
  Add lhs                      <> Pop rhs                      = Pop (Add lhs <> rhs)
  Add lhs                      <> Push idRHS elemsRHS lowerRHS = Push idRHS elemsRHS (Add lhs <> lowerRHS)

instance Monoid (StackStream stackId streamElem) where
  mempty = Empty


-- | Push a new element stream with the provided name onto the stack.
push :: stackId -> StackStream stackId streamElem
push stackId = Push stackId mempty mempty

-- | Add an element to the top the stream at the top of the stack.
--
-- Note adding elements to an empty stack are discarded.
add :: forall stackId streamElem. streamElem -> StackStream stackId streamElem
add a = Add (Endo (a:))


-- | Pop the top element stream from the stack.
pop :: StackStream stackId streamElem
pop = Pop mempty

-- | Get the element stream currently at the top of the stack.
top :: StackStream stackId streamElem -> Maybe (stackId, [streamElem])
top (Push stackId streamElems _) = Just (stackId, appEndo streamElems [])
top _ = Nothing

-- | Is the stack currently empty.
null :: StackStream stackId streamElem -> Bool
null Empty = True
null (Pop _) = True
null (Add _) = True
null (Push _ _ _) = False

-- | Convert the stack stream to a list of lists (equality is defined with respect to this operation).
toLists :: StackStream stackId streamElem -> [(stackId, [streamElem])]
toLists a =
  case top a of
    Just x -> x : toLists (a <> pop)
    Nothing -> []
