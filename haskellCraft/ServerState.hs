module ServerState
  ( ServerState,
    addToQueue,     -- Int -> Inmess -> ServerState -> ServerState
    serverStep,     -- ServerState -> ( ServerState, [Outmess] )
    simulationStep, -- ServerState -> Inmess -> ( ServerState, [Outmess] )
    serverStart,    -- ServerState
    serverSize,     -- ServerState -> Int
    shortestQueue,  -- ServerState -> Int
  ) where

-- Ex 16.16
-- Are there redundant operations in the signatures of the ADTs QueueState and QueueServer?
-- size and shortest are completly redundant, I don't see a good use of them in implementations of functions.
-- serverStart is controversial for me
