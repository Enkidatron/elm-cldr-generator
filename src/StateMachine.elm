module StateMachine exposing (Looper(..), continue, halt, run)

import Posix.IO as IO exposing (IO, Process)


type Looper a
    = Halt
    | Continue a


run : (Process -> a) -> (a -> IO (Looper a)) -> Process -> IO ()
run initialize stepFun process =
    IO.return (Continue (initialize process))
        |> IO.andThen (runMachineLoop stepFun)


runMachineLoop : (a -> IO (Looper a)) -> Looper a -> IO ()
runMachineLoop stepFun looper =
    case looper of
        Halt ->
            IO.return ()

        Continue a ->
            stepFun a |> IO.andThen (runMachineLoop stepFun)


continue : a -> IO (Looper a)
continue a =
    IO.return (Continue a)


halt : IO (Looper a)
halt =
    IO.return Halt
