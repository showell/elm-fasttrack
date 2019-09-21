module Main exposing (..)

import Html exposing (..)
import Dict
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Type exposing
    ( SquareKind(..)
    , Zone
    , Square
    )
import Config exposing
    ( zone_config
    )
import Piece exposing
    ( PieceDict
    , config_pieces
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


type alias SquareKey =
    { zone_color : String, id: String }

type alias Model =
    { zones: List Zone
    , piece_map: PieceDict
    , status: String
    }

init : ( Model, Cmd Msg )
init =
    let
        zones = zone_config

        model =
            { zones = zones
            , piece_map = config_pieces
            , status = "beginning"
            }

    in
        ( model, Cmd.none )


square_status: PieceDict -> SquareKey -> String
square_status piece_map info =
    let
        square_info_str = info.zone_color ++ " " ++ info.id

        piece_color = get_piece piece_map info.zone_color info.id

    in
        case piece_color of
            Just color ->
                square_info_str ++ " (" ++ color ++ " piece)"
            other ->
                square_info_str

-- UPDATE


type Msg
    = ClickSquare SquareKey

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare info ->
            let
                status = square_status model.piece_map info
                model_ = { model | status = status }
            in
                (model_, Cmd.none)

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
            , svg [ width "800", height "800" ] [ (draw_zones model.piece_map model.zones) ]
            ]

gutter: Float
gutter = 4.0

square_size: Float
square_size = 35.0

zone_height: Float
zone_height = 7 * square_size

draw_zones: PieceDict -> List Zone -> Html Msg
draw_zones piece_map zones =
    g [] (List.map (draw_zone piece_map) zones)


draw_zone: PieceDict -> Zone -> Html Msg
draw_zone piece_map zone =
    let
        squares = zone.squares
        angle = zone.angle
        color = zone.color

        center = toString (zone_height + 30)

        translate = "translate(" ++ center ++ " " ++ center ++ ")"
        rotate = "rotate(" ++ (toString angle) ++ ")"
        transform_ = translate ++ " " ++ rotate

        drawn_squares = List.map (draw_square piece_map color) squares

    in
        g [transform transform_] drawn_squares

get_piece: PieceDict -> String -> String -> Maybe String
get_piece dct key sub_key =
    case Dict.get key dct of
        Just sub_dict -> Dict.get sub_key sub_dict
        other -> Nothing


draw_square: PieceDict -> String -> Square -> Html Msg
draw_square piece_map zone_color square =
    let
        square_info =
            { zone_color  = zone_color
            , id = square.id
            }

        w = square_size - gutter

        h = square_size - gutter

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

        draw_piece color =
            let
                radius = "5"
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

        s_square = rect
            [ x (toString xpos)
            , y (toString ypos)
            , fill "white"
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

