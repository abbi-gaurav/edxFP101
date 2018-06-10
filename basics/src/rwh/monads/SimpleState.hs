module SimpleState where

type SimpleState s a = s -> (a,s)

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a,s)

-- bindSt :: (s -> (a,s))             step
--        -> (a -> (s -> (b,s)))      makeStep
--        -> (s -> (b,s))             SimpleState b
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt step makeStep = \oldState -> let (result, newState) = step oldState
                                    in (makeStep result) newState


getSt :: SimpleState s s
getSt = \s -> (s,s)

putSt :: s -> SimpleState s ()
putSt s' = \_ -> ((),s')
