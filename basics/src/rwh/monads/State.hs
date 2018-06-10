module State where

newtype State s a = State {
  runState :: s -> (a,s)
  }

returnState :: a -> State s a
returnState a = State {
  runState = \s -> (a,s)
  }

bindState :: State s a -> (a -> State s b) -> State s b
bindState m f = State $ \s -> let (a, s') = runState m s
                              in runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
