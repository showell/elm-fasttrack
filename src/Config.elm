module Config exposing
    ( config_locations
    , get_zone_colors
    , gutter_size
    , holding_pen_locations
    , is_base_id
    , is_holding_pen_id
    , square_size
    )

import List.Extra
import Set
import Type
    exposing
        ( Location
        , PieceLocation
        )


gutter_size : Float
gutter_size =
    4.0


square_size : Float
square_size =
    26.0


get_zone_colors : Int -> List String
get_zone_colors num_players =
    List.take num_players [ "red", "blue", "green", "purple", "aqua", "brown" ]


base_locations : List String
base_locations =
    [ "B1", "B2", "B3", "B4" ]


is_base_id : String -> Bool
is_base_id id =
    Set.member id (Set.fromList base_locations)


holding_pen_locations : List String
holding_pen_locations =
    [ "HP1", "HP2", "HP3", "HP4" ]


is_holding_pen_id : String -> Bool
is_holding_pen_id id =
    Set.member id (Set.fromList holding_pen_locations)


config_locations : List Location
config_locations =
    [ -- holding pen
      { x = -4.2
      , y = 1.7
      , id = "HP1"
      }
    , { x = -3.2
      , y = 1.7
      , id = "HP2"
      }
    , { x = -4.2
      , y = 0.7
      , id = "HP3"
      }
    , { x = -3.2
      , y = 0.7
      , id = "HP4"
      }
    , -- base
      { x = 0
      , y = 1
      , id = "B1"
      }
    , { x = 0
      , y = 2
      , id = "B2"
      }
    , { x = 0
      , y = 3
      , id = "B3"
      }
    , { x = 0
      , y = 4
      , id = "B4"
      }
    , -- bottom
      { x = -1
      , y = 0
      , id = "HH"
      }
    , { x = 0
      , y = 0
      , id = "DS"
      }
    , { x = 1
      , y = 0
      , id = "BR"
      }
    , -- left
      { x = -2
      , y = 0
      , id = "L0"
      }
    , { x = -2
      , y = 1
      , id = "L1"
      }
    , { x = -2
      , y = 2
      , id = "L2"
      }
    , { x = -2
      , y = 3
      , id = "L3"
      }
    , { x = -2
      , y = 4
      , id = "L4"
      }
    , { x = -2
      , y = 5
      , id = "FT"
      }
    , -- right
      { x = 2
      , y = 0
      , id = "R0"
      }
    , { x = 2
      , y = 1
      , id = "R1"
      }
    , { x = 2
      , y = 2
      , id = "R2"
      }
    , { x = 2
      , y = 3
      , id = "R3"
      }
    , { x = 2
      , y = 4
      , id = "R4"
      }
    ]
