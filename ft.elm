module Main exposing (..)

import Html exposing (..)
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)


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

type alias Model =
    { zones: List Zone
    }

init : ( Model, Cmd Msg )
init =
    let
        zones = zone_config

        model =
            { zones = zones
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


-- UPDATE


type Msg
    = Never

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "800", height "800" ] [ (draw_zones model.zones) ]
        ]

gutter: Float
gutter = 4.0

square_size: Float
square_size = 35.0

zone_height: Float
zone_height = 7 * square_size

draw_zones: List Zone -> Html Msg
draw_zones zones =
    g [] (List.map draw_zone zones)


draw_zone: Zone -> Html Msg
draw_zone zone =
    let
        squares = zone.squares
        angle = zone.angle
        color = zone.color

        center = toString (zone_height + 30)

        translate = "translate(" ++ center ++ " " ++ center ++ ")"
        rotate = "rotate(" ++ (toString angle) ++ ")"
        transform_ = translate ++ " " ++ rotate

        drawn_squares = List.map (draw_square color) squares

    in
        g [transform transform_] drawn_squares

draw_square: String -> Square -> Html Msg
draw_square zone_color square =
    let
        w = square_size - gutter

        h = square_size - gutter

        color = case square.kind of
            FastTrack -> zone_color
            HoldingPen -> zone_color
            Base -> zone_color
            HideyHole -> "gray"
            other -> "black"

        xpos =
            square.x * square_size - w / 2

        ypos =
            zone_height - (square.y * square_size) - h / 2

        events =
            []

        contents =
            g events
                [ rect [ x (toString xpos), y (toString ypos), fill "white", stroke color, width (toString w), height (toString h) ] []
                ]

    in
        g [] [contents]

