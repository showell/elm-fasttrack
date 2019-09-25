module Main exposing (..)

import Browser
import Html exposing (..)
import Type
    exposing
        ( SquareKind(..)
        , Square
        , SquareKey
        , Color
        , Turn(..)
        , Model
        )
import Config
    exposing
        ( orig_zone_colors
        )
import Board
    exposing
        ( board_view
        , board_rotate_button
        , rotate_board
        )
import Piece
    exposing
        ( get_piece
        , config_pieces
        )
import Move
    exposing
        ( perform_move
        )
import Player
    exposing
        ( config_players
        , player_view
        , draw_card_cmd
        , draw_card
        , activate_card
        , finish_card
        , set_turn
        , can_player_move
        , get_active_square
        , set_active_square
        )
import Msg
    exposing
        ( Msg(..)
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
            }
    in
        ( model, Cmd.none )


get_active_color : List Color -> Color
get_active_color zone_colors =
    -- appease compiler with Maybe
    List.head zone_colors
        |> Maybe.withDefault "bogus"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare clicked_square ->
            let
                model_ =
                    handle_square_click model clicked_square
            in
                ( model_, Cmd.none )

        DrawCard player_color ->
            ( model
            , draw_card_cmd model.players player_color
            )

        DrawCardResult player_color idx ->
            let
                model_ =
                    { model
                        | players = draw_card model.players player_color idx
                    }
            in
                ( model_, Cmd.none )

        ActivateCard player_color idx ->
            let
                model_ =
                    { model
                        | players = activate_card model.players player_color idx
                    }
            in
                ( model_, Cmd.none )

        FinishCard player_color ->
            let
                model_ =
                    { model
                        | players = finish_card model.players player_color
                    }
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
            in
                ( model_, Cmd.none )


handle_square_click : Model -> SquareKey -> Model
handle_square_click model clicked_square =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players
    in
        case get_active_square players active_color of
            Nothing ->
                let
                    piece_color =
                        get_piece model.piece_map ( clicked_square.zone_color, clicked_square.id )

                    can_move =
                        case piece_color of
                            Nothing ->
                                False

                            Just piece_color_ ->
                                if piece_color_ == active_color then
                                    can_player_move model.players active_color
                                else
                                    False

                    new_players =
                        if can_move then
                            set_active_square players active_color clicked_square
                        else
                            players
                in
                    { model
                        | players = new_players
                    }

            Just prev ->
                let
                    move =
                        { prev = prev
                        , next = clicked_square
                        }
                in
                    perform_move model move active_color



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
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
            , cards
            , board_rotate_button
            ]
    in
        { title = "Fast Track"
        , body = body
        }
