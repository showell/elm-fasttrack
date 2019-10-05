module Main exposing (..)

import Browser
import Config
    exposing
        ( get_zone_colors
        )
import Move
    exposing
        ( maybe_auto_move
        , perform_move
        )
import Piece
    exposing
        ( config_pieces
        )
import Player
    exposing
        ( activate_card
        , config_players
        , finish_card
        , get_active_location
        , get_player
        , replenish_hand
        , set_active_location
        , set_turn
        , update_player
        )
import Random
import Set
import Task
import Time
import Type
    exposing
        ( AppState(..)
        , Color
        , Location
        , Model
        , Msg(..)
        , PieceLocation
        , Player
        , Turn(..)
        , UpdatePlayerFunc
        )
import View
    exposing
        ( view
        )



-- MODEL / INIT


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        num_players =
            4

        zone_colors =
            get_zone_colors num_players

        active_color =
            get_active_color zone_colors

        model =
            { zone_colors = zone_colors
            , piece_map = config_pieces zone_colors
            , players = config_players active_color zone_colors
            , seed = Random.initialSeed 42
            , state = Loading
            , get_active_color = get_active_color
            }
    in
    ( model, Task.perform LoadGame Time.now )


randomize : Cmd Msg
randomize =
    Task.perform NewSeed Time.now


update_active_player : Model -> UpdatePlayerFunc
update_active_player model f =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            update_player model.players active_color f
    in
    { model | players = players }


replenish_active_hand : Model -> Model
replenish_active_hand model =
    let
        active_color =
            get_active_color model.zone_colors
    in
    replenish_hand active_color model


seed_from_time : Time.Posix -> Random.Seed
seed_from_time time =
    Random.initialSeed (Time.posixToMillis time)



-- TODO: just put this on model


get_active_color : List Color -> Color
get_active_color zone_colors =
    -- appease compiler with Maybe
    List.head zone_colors
        |> Maybe.withDefault "bogus"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadGame time ->
            let
                seed =
                    seed_from_time time

                model_ =
                    { model
                        | seed = seed
                        , state = Ready
                    }
                        |> replenish_active_hand
            in
            ( model_, Cmd.none )

        NewSeed time ->
            -- this command is invoked to add extra randomness
            -- (and theoretically prevent a tech savvy player with
            -- a computer from anticipating a whole sequence of draws)
            let
                seed =
                    seed_from_time time

                model_ =
                    { model
                        | seed = seed
                    }
            in
            ( model_, Cmd.none )

        ClickLocation clicked_loc ->
            let
                model_ =
                    handle_loc_click model clicked_loc
            in
            ( model_, Cmd.none )

        ReplenishHand ->
            let
                model_ =
                    replenish_active_hand model
            in
            ( model_, Cmd.none )

        ActivateCard player_color idx ->
            let
                model_ =
                    update_active_player model (activate_card idx)
            in
            ( model_, randomize )

        FinishCard player_color ->
            let
                active_color =
                    get_active_color model.zone_colors

                model_ =
                    model |> finish_card player_color
            in
            ( model_, Cmd.none )

        RotateBoard ->
            let
                old_player_color =
                    get_active_color model.zone_colors

                new_zone_colors =
                    rotate_board model.zone_colors

                new_player_color =
                    get_active_color new_zone_colors

                players =
                    model.players
                        |> set_turn old_player_color TurnIdle
                        |> set_turn new_player_color TurnInProgress

                model_ =
                    { model
                        | zone_colors = new_zone_colors
                        , players = players
                    }
                        |> replenish_active_hand
            in
            ( model_, Cmd.none )


rotate_board : List Color -> List Color
rotate_board zones =
    List.drop 1 zones ++ List.take 1 zones


handle_loc_click : Model -> PieceLocation -> Model
handle_loc_click model location =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players

        update_player =
            update_active_player model

        active_player =
            get_player players active_color

        active_location =
            get_active_location active_player
    in
    case active_location of
        Nothing ->
            update_player (set_active_location location)
                |> maybe_auto_move location update_player

        Just start ->
            let
                move =
                    { start = start
                    , end = location
                    }
            in
            perform_move model move active_color update_player



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- for VIEW, see View.elm
