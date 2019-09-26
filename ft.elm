module Main exposing (..)

import Browser
import Html exposing (..)
import Type
    exposing
        ( Square
        , PieceLocation
        , Color
        , Turn(..)
        , Player
        , Model
        , UpdatePlayerFunc
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
        , update_player
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


update_active_player : Model -> UpdatePlayerFunc
update_active_player model f =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            update_player model.players active_color f
    in
        { model | players = players }



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
                    update_active_player model (draw_card idx)
            in
                ( model_, Cmd.none )

        ActivateCard player_color idx ->
            let
                model_ =
                    update_active_player model (activate_card idx)
            in
                ( model_, Cmd.none )

        FinishCard player_color ->
            let
                model_ =
                    update_active_player model (finish_card)
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


handle_square_click : Model -> PieceLocation -> Model
handle_square_click model square_loc =
    let
        active_color =
            get_active_color model.zone_colors

        players =
            model.players

        update_player =
            update_active_player model
    in
        case get_active_square players active_color of
            Nothing ->
                let
                    piece_color =
                        get_piece model.piece_map square_loc

                    can_move =
                        case piece_color of
                            Nothing ->
                                False

                            Just piece_color_ ->
                                if piece_color_ == active_color then
                                    can_player_move model.players active_color
                                else
                                    False
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
                    perform_move model move update_player



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
