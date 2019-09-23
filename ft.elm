module Main exposing (..)

import Html exposing (..)
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Type exposing
    ( SquareKind(..)
    , Zone
    , Square
    , SquareKey
    )
import Config exposing
    ( zone_config
    , gutter_size
    , square_size
    , board_size
    )
import Piece exposing
    ( PieceDict
    , get_piece
    , config_pieces
    )
import Move exposing
    ( perform_move
    )
import Card exposing
    ( AllCards
    , config_all_cards
    , card_view
    , draw_card_cmd
    , draw_card
    , activate_card
    , finish_card
    )
import Square exposing
    ( square_view
    , zone_height
    )
import Msg exposing
    ( Msg(..)
    )

main: Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { zones: List Zone
    , piece_map: PieceDict
    , status: String
    , active_square: Maybe SquareKey
    , active_color: String
    , all_cards: AllCards
    }

init : ( Model, Cmd Msg )
init =
    let
        zones = zone_config

        model =
            { zones = zones
            , piece_map = config_pieces
            , status = "beginning"
            , active_square = Nothing
            , active_color = "green"
            , all_cards = config_all_cards
            }

    in
        ( model, Cmd.none )


square_desc: PieceDict -> SquareKey -> Maybe String -> String
square_desc piece_map info piece_color =
    let
        square_info_str = info.zone_color ++ " " ++ info.id
    in
        case piece_color of
            Just color ->
                square_info_str ++ " (" ++ color ++ " piece)"
            other ->
                square_info_str


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
            , draw_card_cmd model.all_cards player_color
            )
        DrawCardResult player_color idx ->
            let
                model_ =
                    { model
                    | all_cards = draw_card model.all_cards player_color idx
                    }
            in
                (model_, Cmd.none)
        ActivateCard player_color idx ->
            let
                model_ =
                    { model
                    | all_cards = activate_card model.all_cards player_color idx
                    }
            in
                (model_, Cmd.none)
        FinishCard player_color ->
            let
                model_ =
                    { model
                    | all_cards = finish_card model.all_cards player_color
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
                        if piece_color_ == model.active_color then
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


view : Model -> Html Msg
view model =
    let
        heading = div [] [ Html.text model.status ]
        board = svg
            [ width board_size, height board_size]
            [ (draw_zones model.piece_map model.zones model.active_square) ]
    in
        div []
            [ heading
            , div [] [board]
            , card_view model.all_cards model.active_color
            ]

draw_zones: PieceDict -> List Zone -> Maybe SquareKey -> Html Msg
draw_zones piece_map zones active_square =
    g [] (List.map (draw_zone piece_map active_square) zones)


draw_zone: PieceDict -> Maybe SquareKey -> Zone -> Html Msg
draw_zone piece_map active_square zone =
    let
        squares = zone.squares
        angle = zone.angle
        color = zone.color

        center = toString (zone_height + 30)

        translate = "translate(" ++ center ++ " " ++ center ++ ")"
        rotate = "rotate(" ++ (toString angle) ++ ")"
        transform_ = translate ++ " " ++ rotate

        drawn_squares = List.map (square_view piece_map color active_square) squares

    in
        g [transform transform_] drawn_squares

