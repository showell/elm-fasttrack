module Main exposing (..)

import Browser
import Config
    exposing
        ( get_zone_colors
        )
import Move
    exposing
        ( maybe_auto_move
        , move_to_end_loc
        )
import Piece
    exposing
        ( config_pieces
        )
import Player
    exposing
        ( activate_card
        , begin_turn
        , config_players
        , discard_card
        , get_player
        , get_player_move_type
        , replenish_hand
        , set_start_location
        , set_turn
        , update_active_player
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
        , TurnNeedEndLocInfo
        , TurnNeedStartLocInfo
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

        model =
            { zone_colors = zone_colors
            , piece_map = config_pieces zone_colors
            , players = config_players zone_colors
            , seed = Random.initialSeed 42
            , state = Loading
            , get_active_color = get_active_color
            }
    in
    ( model, Task.perform LoadGame Time.now )


replenish_active_hand : Model -> Model
replenish_active_hand model =
    let
        active_color =
            get_active_color model.zone_colors
    in
    replenish_hand active_color model


begin_active_turn : Model -> Model
begin_active_turn model =
    let
        active_color =
            get_active_color model.zone_colors
    in
    begin_turn active_color model


seed_from_time : Time.Posix -> Random.Seed
seed_from_time time =
    Random.initialSeed (Time.posixToMillis time)


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
                        |> begin_active_turn
            in
            ( model_, Cmd.none )

        SetStartLocation clicked_loc ->
            let
                model_ =
                    handle_start_loc_click model clicked_loc
            in
            ( model_, Cmd.none )

        SetEndLocation clicked_loc ->
            let
                model_ =
                    handle_end_loc_click model clicked_loc
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
                    update_active_player (activate_card idx) model
            in
            ( model_, Cmd.none )

        DiscardCard player_color idx ->
            let
                model_ =
                    update_active_player (discard_card idx) model
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
                        |> set_turn new_player_color TurnBegin

                model_ =
                    { model
                        | zone_colors = new_zone_colors
                        , players = players
                    }
                        |> begin_active_turn
            in
            ( model_, Cmd.none )


rotate_board : List Color -> List Color
rotate_board zones =
    List.drop 1 zones ++ List.take 1 zones


handle_start_loc_click : Model -> PieceLocation -> Model
handle_start_loc_click model location =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players

        active_player =
            get_player players active_color
    in
    case active_player.turn of
        TurnNeedStartLoc _ ->
            model
                |> update_active_player (set_start_location location)
                |> maybe_auto_move location

        _ ->
            -- something is wrong with our click handlers
            model


handle_end_loc_click : Model -> PieceLocation -> Model
handle_end_loc_click model end_loc =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players

        active_player =
            get_player players active_color
    in
    case active_player.turn of
        TurnNeedEndLoc info ->
            let
                start_loc =
                    info.start_location
            in
            model
                |> move_to_end_loc active_player active_color start_loc end_loc

        _ ->
            -- something is wrong with our click handlers
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- for VIEW, see View.elm
