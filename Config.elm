module Config exposing
    ( config_locations
    , get_piece_kind
    , gutter_size
    , holding_pen_locations
    , orig_zone_colors
    , square_size
    )

import List.Extra
import Type
    exposing
        ( PieceLocation
        , Location
        , LocationKind(..)
        )


gutter_size : Float
gutter_size =
    3.0


square_size : Float
square_size =
    26.0


orig_zone_colors : List String
orig_zone_colors =
    [ "red", "blue", "green", "purple" ]


holding_pen_locations : List String
holding_pen_locations =
    [ "HP1", "HP2", "HP3", "HP4" ]


get_piece_kind : String -> LocationKind
get_piece_kind id =
    let
        item =
            List.Extra.find (\sq -> sq.id == id) config_locations
    in
    case item of
        Nothing ->
            -- appease compiler
            Normal

        Just item_ ->
            item_.kind


config_locations : List Location
config_locations =
    [ -- holding pen
      { x = -4.2
      , y = 1.7
      , kind = HoldingPen
      , id = "HP1"
      }
    , { x = -3.2
      , y = 1.7
      , kind = HoldingPen
      , id = "HP2"
      }
    , { x = -4.2
      , y = 0.7
      , kind = HoldingPen
      , id = "HP3"
      }
    , { x = -3.2
      , y = 0.7
      , kind = HoldingPen
      , id = "HP4"
      }
    , -- base
      { x = 0
      , y = 1
      , kind = Base
      , id = "B1"
      }
    , { x = 0
      , y = 2
      , kind = Base
      , id = "B2"
      }
    , { x = 0
      , y = 3
      , kind = Base
      , id = "B3"
      }
    , { x = 0
      , y = 4
      , kind = Base
      , id = "B4"
      }
    , -- bottom
      { x = -1
      , y = 0
      , kind = HideyHole
      , id = "HH"
      }
    , { x = 0
      , y = 0
      , kind = DoorStep
      , id = "DS"
      }
    , { x = 1
      , y = 0
      , kind = Normal
      , id = "BL"
      }
    , -- left
      { x = -2
      , y = 0
      , kind = Normal
      , id = "L0"
      }
    , { x = -2
      , y = 1
      , kind = Normal
      , id = "L1"
      }
    , { x = -2
      , y = 2
      , kind = Normal
      , id = "L2"
      }
    , { x = -2
      , y = 3
      , kind = Normal
      , id = "L3"
      }
    , { x = -2
      , y = 4
      , kind = Normal
      , id = "L4"
      }
    , { x = -2
      , y = 5
      , kind = FastTrack
      , id = "FT"
      }
    , -- right
      { x = 2
      , y = 0
      , kind = Normal
      , id = "R0"
      }
    , { x = 2
      , y = 1
      , kind = Normal
      , id = "R1"
      }
    , { x = 2
      , y = 2
      , kind = Normal
      , id = "R2"
      }
    , { x = 2
      , y = 3
      , kind = Normal
      , id = "R3"
      }
    , { x = 2
      , y = 4
      , kind = Normal
      , id = "R4"
      }
    ]
