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
