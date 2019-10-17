module History exposing
    ( History
    , canUndo
    , init
    , reset
    , undo
    , update
    )

import List
import List.Extra



-- The `states` is a stack. The first element on the stack is the
-- last known good state, and then we either pop the first or
-- second element on an undo.


type alias History a =
    { states : List a
    }


init : History a
init =
    { states = [] }


priorStates : History a -> a -> List a
priorStates history state =
    history.states
        |> List.Extra.dropWhile (\s -> s == state)


update : History a -> a -> History a
update history state =
    let
        states =
            state :: priorStates history state
    in
    { states = states }


reset : a -> History a
reset state =
    { states = [ state ] }


canUndo : History a -> a -> Bool
canUndo history state =
    List.length (priorStates history state) > 0


undo : History a -> a -> ( History a, a )
undo history state =
    case priorStates history state of
        [] ->
            -- caller should really use canUndo to suppress
            -- the undo feature when nothing's actually changed,
            -- but we just silently succeed here
            ( history, state )

        head :: rest ->
            let
                newHistory =
                    { states = rest }
            in
            ( newHistory, head )
