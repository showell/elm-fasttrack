module Main exposing (main)

import Browser
import DictHelper exposing (..)
import Game
    exposing
        ( beginGame
        , updateGame
        )
import History
    exposing
        ( canUndo
        , init
        )
import Html
import List.Extra
import NewDict exposing (..)
import Set
import Task
import Time
import Type
    exposing
        ( Model
        , Msg(..)
        )
import View
    exposing
        ( gameView
        )



-- MODEL / INIT


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { game = Nothing
            , history = History.init
            }
    in
    ( model, Cmd.none)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        history =
            model.history

        newModel =
            case msg of
                BeginGame time ->
                    let
                        newGame =
                            beginGame time
                    in
                    { model
                        | game = Just newGame
                        , history = History.reset newGame
                    }

                UpdateGame gameMsg ->
                    case model.game of
                        Nothing ->
                            -- should not happen
                            model

                        Just game ->
                            let
                                ( newHistory, newGame ) =
                                    updateGame gameMsg history game
                            in
                            { model
                                | game = Just newGame
                                , history = newHistory
                            }
    in
    ( newModel, Cmd.none )


type alias DictGeneration =
    { aliases : Dict String String
    , humanOutput : String
    , interestingLists : List (List String)
    , n : Int
    }


type alias SingleGenerationResult =
    { humanOutput : String
    , aliases : Dict String String
    , interestingLists : List (List String)
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW (see View.elm for the "guts")

show =
    DictHelper.show
        |> Html.text
        |> List.singleton
        |> Html.pre []

view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.game of
                Nothing ->
                    -- The load should basically happen instantly, so this
                    -- is just defensive against race conditions.  Of course,
                    -- this may change in the future if we do things like
                    -- connect to a server.
                    [ Html.text "loading..." ]

                Just game ->
                    let
                        showUndoButton =
                            canUndo model.history game
                    in
                    gameView game showUndoButton
                        |> Html.map UpdateGame
                        |> List.singleton
    in
    { title = "Fast Track"
    , body = [ show ]
    }
