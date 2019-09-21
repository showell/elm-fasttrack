module Config exposing
    ( zone_config
    , holding_pen_squares
    , zone_colors
    )

import Type exposing
    ( Zone
    , Square
    , SquareKind(..)
    )

zone_colors: List String
zone_colors = ["red", "blue", "green", "purple"]

holding_pen_squares: List String
holding_pen_squares = ["HP1", "HP2", "HP3", "HP4"]

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
        { x = -4.2
        , y = 1.7
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

        { x = -3.2
        , y = 0.7
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


