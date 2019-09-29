module Main exposing (..)

import Board
    exposing
        ( board_rotate_button
        , board_view
        , rotate_board
        )
import Browser
import Config
    exposing
        ( orig_zone_colors
        )
import Html exposing (..)
import Move
    exposing
        ( perform_move
        )
import Msg
    exposing
        ( Msg(..)
        )
import Piece
    exposing
        ( config_pieces
        )
import Player
    exposing
        ( activate_card
        , can_player_start_move_here
        , config_players
        , finish_card
        , get_active_square
        , get_player
        , player_view
        , replenish_hand
        , set_active_square
        , set_turn
        , update_player
        )
import Random
import Task
import Time
import Type
    exposing
        ( AppState(..)
        , Color
        , Model
        , PieceLocation
        , Player
        , Square
        , Turn(..)
        , UpdatePlayerFunc
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
        zone_colors =
            orig_zone_colors

        active_color =
            get_active_color zone_colors

        model =
            { zone_colors = zone_colors
            , piece_map = config_pieces zone_colors
            , players = config_players active_color zone_colors
            , seed = Random.initialSeed 42
            , state = Loading
            }
    in
    ( model, Task.perform LoadGame Time.now )


randomize : Cmd Msg
randomize =
    Task.perform NewSeed Time.now


get_active_color : List Color -> Color
get_active_color zone_colors =
    -- appease compiler with Maybe
    List.head zone_colors
        |> Maybe.withDefault "bogus"


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

        ClickSquare clicked_square ->
            let
                model_ =
                    handle_square_click model clicked_square
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


handle_square_click : Model -> PieceLocation -> Model
handle_square_click model square_loc =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players

        update_player =
            update_active_player model

        active_player =
            get_player players active_color

        active_square =
            get_active_square active_player
    in
    case active_square of
        Nothing ->
            let
                piece_map =
                    model.piece_map

                can_move =
                    can_player_start_move_here active_player active_color piece_map square_loc
            in
            if can_move then
                update_player (set_active_square square_loc)

            else
                model

        Just prev ->
            let
                move =
                    { prev = prev
                    , next = square_loc
                    }
            in
            perform_move model move active_color update_player



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.state of
        Loading ->
            -- The load should basically happen instantly, so this
            -- is just defensive against race conditions.  Of course,
            -- this may change in the future if we do things like
            -- connect to a server.
            { title = "Fast Track"
            , body = [ Html.text "loading..." ]
            }

        Ready ->
            normal_view model


normal_view : Model -> Browser.Document Msg
normal_view model =
    let
        active_color =
            get_active_color model.zone_colors

        board =
            div
                []
                [ board_view model.piece_map model.zone_colors model.players active_color ]

        cards =
            player_view model.players active_color

        body =
            [ board
            , hr [] []
            , cards
            , hr [] []
            , board_rotate_button
            ]
    in
    { title = "Fast Track"
    , body = body
    }
