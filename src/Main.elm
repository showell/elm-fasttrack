module Main exposing (main)

import Browser
import Game
    exposing
        ( beginGame
        , updateGame
        )
import Html
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
            }
    in
    ( model, Task.perform BeginGame Time.now )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newGame =
            case msg of
                BeginGame time ->
                    Just (beginGame time)

                UpdateGame gameMsg ->
                    model.game
                        |> Maybe.map (updateGame gameMsg)

        newModel =
            { game = newGame
            }
    in
    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW (see View.elm for the "guts")


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
                    gameView game
                        |> List.map (\msg -> Html.map UpdateGame msg)
    in
    { title = "Fast Track"
    , body = body
    }
