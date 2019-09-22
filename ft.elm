module Main exposing (..)

import Html exposing (..)
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
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
    )
import Piece exposing
    ( PieceDict
    , get_piece
    , config_pieces
    )
import Move exposing
    ( perform_move
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

is_active_square: SquareKey -> Maybe SquareKey -> Bool
is_active_square square_info active_square =
    case active_square of
        Nothing ->
            False
        Just active ->
            square_info.zone_color == active.zone_color && square_info.id == active.id

-- UPDATE


type Msg
    = ClickSquare SquareKey

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare clicked_square ->
            let
                model_ = handle_square_click model clicked_square


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
    in
        div []
            [ heading
            , svg [ width "800", height "800" ] [ (draw_zones model.piece_map model.zones model.active_square) ]
            ]

zone_height: Float
zone_height = 7 * square_size

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

        drawn_squares = List.map (draw_square piece_map color active_square) squares

    in
        g [transform transform_] drawn_squares

draw_square: PieceDict -> String -> Maybe SquareKey -> Square -> Html Msg
draw_square piece_map zone_color active_square square =
    let
        square_info =
            { zone_color = zone_color
            , id = square.id
            , kind = square.kind
            }

        w = square_size - gutter_size

        h = square_size - gutter_size

        color = case square.kind of
            FastTrack -> zone_color
            HoldingPen -> zone_color
            Base -> zone_color
            HideyHole -> "gray"
            other -> "black"

        cx_ = square.x * square_size

        cy_ = zone_height - (square.y * square_size)

        xpos = cx_ - w / 2
        ypos = cy_ - h / 2

        my_piece = get_piece piece_map zone_color square.id

        my_pieces = case my_piece of
            Just piece_color -> [piece_color]
            other -> []

        is_active = is_active_square square_info active_square

        draw_piece color =
            let
                radius =
                    if is_active then
                        "7"
                    else
                        "5"
            in
                circle
                [ cx (toString cx_)
                , cy (toString cy_)
                , fill color
                , stroke color
                , r radius
                , onClick (ClickSquare square_info)
                ] []

        s_pieces = List.map draw_piece my_pieces

        fill_color =
            if is_active then
                "lightblue"
            else
                "white"

        s_square = rect
            [ x (toString xpos)
            , y (toString ypos)
            , fill fill_color
            , stroke color
            , width (toString w)
            , height (toString h)
            , onClick (ClickSquare square_info)
            ] []

        contents = List.concat
            [ [s_square]
            , s_pieces
            ]

    in
        g [] contents

