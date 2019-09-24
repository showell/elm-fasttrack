module Main exposing (..)

import Browser
import Html exposing (..)
import Type exposing
    ( SquareKind(..)
    , Square
    , SquareKey
    , Color
    , Model
    )
import Config exposing
    ( orig_zone_colors
    )
import Board exposing
    ( board_view
    , board_rotate_button
    , rotate_board
    )
import Piece exposing
    ( get_piece
    , config_pieces
    )
import Move exposing
    ( perform_move
    )
import Player exposing
    ( config_players
    , player_view
    , draw_card_cmd
    , draw_card
    , activate_card
    , finish_card
    )
import Square exposing
    ( square_desc
    )
import Msg exposing
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
        zone_colors = orig_zone_colors

        model =
            { zone_colors = zone_colors
            , piece_map = config_pieces zone_colors
            , status = "beginning"
            , active_square = Nothing
            , players = config_players zone_colors
            }

    in
        ( model, Cmd.none )

active_color : Model -> Color
active_color model =
    List.head model.zone_colors
        |> Maybe.withDefault "bogus" -- appease compiler

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare clicked_square ->
            let
                model_ = handle_square_click model clicked_square
            in
                (model_, Cmd.none)
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
                (model_, Cmd.none)
        ActivateCard player_color idx ->
            let
                model_ =
                    { model
                    | players = activate_card model.players player_color idx
                    }
            in
                (model_, Cmd.none)
        FinishCard player_color ->
            let
                model_ =
                    { model
                    | players = finish_card model.players player_color
                    }
            in
                (model_, Cmd.none)
        RotateBoard ->
            let
                model_ =
                    { model
                    | zone_colors = rotate_board model.zone_colors
                    }
            in
                (model_, Cmd.none)

handle_square_click: Model -> SquareKey -> Model
handle_square_click model clicked_square =
    case model.active_square of
        Nothing ->
            let
                piece_color = get_piece model.piece_map clicked_square.zone_color clicked_square.id
                active_square = case piece_color of
                    Nothing ->
                        Nothing
                    Just piece_color_ ->
                        if piece_color_ == (active_color model) then
                            Just clicked_square
                        else
                            Nothing
                desc = square_desc model.piece_map clicked_square piece_color
                status = case active_square of
                    Just _ ->
                        desc ++ " (CLICK square to move piece)"
                    Nothing ->
                        desc
            in
                { model
                | status = status
                , active_square = active_square
                }
        Just prev ->
            let
                move =
                    { prev = prev
                    , next = clicked_square
                    , piece_map = model.piece_map
                    }

                move_results = perform_move move
            in
                { model
                | status = move_results.status
                , piece_map = move_results.piece_map
                , active_square = move_results.active_square
                }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        heading = div
            []
            [ Html.text model.status ]

        board = div
            []
            [ board_view model.piece_map model.zone_colors model.active_square]

        cards = player_view model.players (active_color model)

        body =
            [ heading
            , board
            , cards
            , board_rotate_button
            ]
    in
        { title = "Fast Track"
        , body = body
        }
