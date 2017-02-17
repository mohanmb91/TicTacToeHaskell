-- ====================================================================
--                    Tic tac toe with history
-- ====================================================================
-- The following are the history operations when applied to TicTacToe.
-- For another type, similar operations are required.

import History (applyAChange, Change, History(..), redo, undo)
import TicTacToe (newGame, next, play, TTT)

-- Play "next" on the current TTT state.
-- The Change TTT declaration isn't necessary.
-- applyAChange expects an argument of type Change a
twhNext :: Change (History TTT)
twhNext = applyAChange (next :: Change TTT)

-- Similar to next except with an explicit move. 
twhPlay :: Int -> Int -> Change (History TTT)
-- Can you explain these types:
--        play r c :: Change TTT, which is TTT -> TTT
--        applyAChange (play r c) :: History TTT -> History TTT
-- The Change TTT declaration isn't necessary.
-- applyAChange expects an argument of type Change a
twhPlay = \r c -> applyAChange (play r c :: Change TTT)

-- Start a TTT. Always start at (3, 1).
twhStart :: History TTT
twhStart = twhStartAt 3 1 

-- Start a TTT at r c.
twhStartAt :: Int -> Int -> History TTT
twhStartAt = \r c -> twhPlay r c $ History [newGame] []
