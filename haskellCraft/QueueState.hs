module QueueState
  ( QueueState,
    addMessage,  -- Inmess -> QueueState -> QueueState
    queueStep,   -- QueueState -> ( QueueState, [Outmess] )
    queueStart,  -- QueueState
    queueLength, -- QueueState -> Int
    queueEmpty   -- QueueState -> Bool
  ) where

-- Ex 16.16
-- Are there redundant operations in the signatures of the ADTs QueueState and QueueServer?
-- We don't need funcitons like length and empty because we don't use them in our 2 main functions - message and step therefore they may be omitted
-- I'm not sure about queueStart, but we should know from definition of queueState, that empty queue is an empty list

data QueueState = QS Time Service [Inmess]
  deriving (Eq,Show)

addMessage :: Inmess -> QueueState -> QueueState
addMessage im (QS time serv ml) = QS time serv (ml++[im])

queueStep :: QueueState -> ( QueueState, [Outmess] )
queueStep (QS time servSoFar (Yes arr serv : inRest))
  | servSoFar < serv
    = (QS (time+1) (servSoFar+1) (Yes arr serv : inRest), [])
  | otherwise
    = (QS (time+1) 0 inRest, [Discharge arr (time-serv-arr) serv])
queueStep (QS time serv []) = (QS (time+1) serv [] , [])

queueStart :: QueueState
queueStart = QS 0 0 []

queueLength :: QueueState -> Int
queueLength (QS _ _ q) = length q

queueEmpty :: QueueState -> Bool
queueEmpty (QS _ _ q) = null q
