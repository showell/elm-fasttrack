module Main exposing (..)

import Html exposing (..)
import Dict
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


main: Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type SquareKind
    = HoldingPen
    | Normal
    | FastTrack
    | DoorStep
    | HideyHole
    | Base

type alias Square =
    { x : Float, y : Float, kind: SquareKind, id: String }

type alias Zone =
    { squares: List Square
    , color: String
    , angle: Float
    }

type alias ZonePieceDict = Dict.Dict String String
type alias PieceDict = Dict.Dict String ZonePieceDict
type alias PieceConfig =
    { zone_color: String
    , color: String
    , id: String
    }

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


zone_config : List Zone
zone_config =
    [
        { squares = config_squares
        , color = "green"
        , angle = 0
        },

        { squares = config_squares
        , color = "red"
        , angle = 90
        },

        { squares = config_squares
        , color = "blue"
        , angle = 180
        },

        { squares = config_squares
        , color = "purple"
        , angle = 270
        }
    ]

config_squares: List Square
config_squares =
    [
        -- holding pen
        { x = -3.2
        , y = 0.7
        , kind = HoldingPen
        , id = "HP1"
        },

        { x = -3.2
        , y = 1.7
        , kind = HoldingPen
        , id = "HP2"
        },

        { x = -4.2
        , y = 0.7
        , kind = HoldingPen
        , id = "HP3"
        },

        { x = -4.2
        , y = 1.7
        , kind = HoldingPen
        , id = "HP4"
        },

        -- base
        { x = 0
        , y = 1
        , kind = Base
        , id = "B1"
        },

        { x = 0
        , y = 2
        , kind = Base
        , id = "B2"
        },

        { x = 0
        , y = 3
        , kind = Base
        , id = "B3"
        },

        { x = 0
        , y = 4
        , kind = Base
        , id = "B4"
        },

        -- bottom
        { x = -1
        , y = 0
        , kind = HideyHole
        , id = "HH"
        },

        { x = 0
        , y = 0
        , kind = DoorStep
        , id = "DS"
        },

        { x = 1
        , y = 0
        , kind = Normal
        , id = "BL"
        },

        -- left
        { x = -2
        , y = 0
        , kind = Normal
        , id = "L0"
        },

        { x = -2
        , y = 1
        , kind = Normal
        , id = "L1"
        },

        { x = -2
        , y = 2
        , kind = Normal
        , id = "L2"
        },

        { x = -2
        , y = 3
        , kind = Normal
        , id = "L3"
        },

        { x = -2
        , y = 4
        , kind = Normal
        , id = "L4"
        },

        { x = -2
        , y = 5
        , kind = FastTrack
        , id = "FT"
        },

        -- right
        { x = 2
        , y = 0
        , kind = Normal
        , id = "R0"
        },

        { x = 2
        , y = 1
        , kind = Normal
        , id = "R1"
        },

        { x = 2
        , y = 2
        , kind = Normal
        , id = "R2"
        },

        { x = 2
        , y = 3
        , kind = Normal
        , id = "R3"
        },

        { x = 2
        , y = 4
        , kind = Normal
        , id = "R4"
        }
    ]


config_zone_pieces: String -> PieceDict -> PieceDict
config_zone_pieces color_ dct =
    let
        assign id_ dct =
            assign_piece dct {zone_color = color_, color = color_, id = id_}

        squares = ["HP1", "HP2", "HP3", "HP4"]
    in
        List.foldr assign dct squares

config_pieces: PieceDict
config_pieces =
    let
        dct = Dict.empty

        colors = ["red", "blue", "green", "purple"]
    in
        List.foldr config_zone_pieces dct colors

assign_piece: PieceDict -> PieceConfig -> PieceDict
assign_piece dct config =
    let
        key = config.zone_color
        sub_key = config.id
        val = config.color

        maybe_sub_dict = Dict.get key dct

        sub_dict = case maybe_sub_dict of
            Just sd -> sd
            Nothing -> Dict.empty

        new_sub_dict = Dict.insert sub_key val sub_dict

    in
        Dict.insert key new_sub_dict dct

-- UPDATE


type Msg
    = ClickSquare

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare ->
            ( { model | status = "click" } , Cmd.none)

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

deep_get: PieceDict -> String -> String -> Maybe String
deep_get dct key sub_key =
    case Dict.get key dct of
        Just sub_dict -> Dict.get sub_key sub_dict
        other -> Nothing


draw_square: PieceDict -> String -> Square -> Html Msg
draw_square piece_map zone_color square =
    let
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

        my_piece = deep_get piece_map zone_color square.id

        my_pieces = case my_piece of
            Just piece_color -> [piece_color]
            other -> []

        draw_piece color =
            let
                radius = "5"
            in
                circle [
                    cx (toString cx_),
                    cy (toString cy_),
                    fill color,
                    stroke color,
                    r radius
                ] []

        s_pieces = List.map draw_piece my_pieces

        s_square = rect
            [ x (toString xpos)
            , y (toString ypos)
            , fill "white"
            , stroke color
            , width (toString w)
            , height (toString h)
            , onClick ClickSquare
            ] []

        contents = List.concat
            [ [s_square]
            , s_pieces
            ]

    in
        g [] contents

