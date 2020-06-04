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

newtype ServerState = SS [QueueState]
  deriving (Eq, Show)

addToQueue :: Int -> Inmess -> ServerState -> ServerState
addToQueue n im (SS st)
  = SS (take n st ++ [newQueueState] ++ drop (n+1) st)
  where
    newQueueState = addMessage im (st!!n)

serverStep :: ServerState -> ( ServerState, [Outmess] )
serverStep (SS []) = (SS [], [])
serverStep (SS (q:qs)) = (SS (q':qs'), mess++messes)
  where
    (q', mess) = queueStep q
    (SS qs', messes) - serverStep (SS qs)

simulationStep
  :: ServerState -> Inmess -> (ServerState, [Outmess])
simulationStep servSt im
  = (addNewObject im servSt1, outmess)
  where
    (servSt1, outmess) = servStep servSt

addNewObject :: Inmess -> ServerState -> ServerState
addNewObject No servSt = servSt
addNewObject (Yes arr wait) servSt
  = addToQueue (shortestQueue servSt) (Yes arr wait) servSt

serverStart :: ServerState
serverStart = SS (repllicate numQueues queueStart)

serverSize :: ServerState -> Int
serverSize (SS xs) = length xs

shortestQueue :: ServerState -> Int
shortestQueue (SS (q:qs))
  | (queueLength (qs!!short) <= queueLength q) = short + 1
  | otherwise = 0
  where
    short = shortestQueue (SS qs)
