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
    , SquareKey
    )
import Config exposing
    ( zone_config
    )
import Piece exposing
    ( PieceDict
    , config_pieces
    , assign_piece
    , unassign_piece
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


type alias Move =
    { prev: SquareKey
    , next: SquareKey
    , piece_map: PieceDict
    }

type alias MoveStatus =
    { piece_map: PieceDict
    , status: String
    , active_square: Maybe SquareKey
    }

type alias Model =
    { zones: List Zone
    , piece_map: PieceDict
    , status: String
    , active_square: Maybe SquareKey
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

perform_move: Move -> MoveStatus
perform_move move =
    let
        piece_map = move.piece_map
        prev = move.prev
        next = move.next

        piece_color = get_piece piece_map prev.zone_color prev.id
    in
        case piece_color of
            Nothing ->
                { piece_map = piece_map
                , status = "program failure"
                , active_square = Nothing
                }
            Just piece_color_ ->
                let
                    new_config =
                        { zone_color = next.zone_color
                        , color = piece_color_
                        , id = next.id
                        }
                    new_map = piece_map
                        -- TODO: banish piece we're landing on
                        |> unassign_piece prev
                        |> assign_piece new_config
                in
                    { piece_map = new_map
                    , status = "moved!"
                    , active_square = Nothing
                    }

-- UPDATE


type Msg
    = ClickSquare SquareKey

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSquare info ->
            let model_ =
                case model.active_square of
                    Nothing ->
                        let
                            status = square_status model.piece_map info
                            piece_color = get_piece model.piece_map info.zone_color info.id
                            active_square = case piece_color of
                                Nothing -> Nothing
                                Just _ -> Just info
                        in
                            { model
                            | status = status
                            , active_square = active_square
                            }
                    Just prev ->
                        let
                            move =
                                { prev = prev
                                , next = info
                                , piece_map = model.piece_map
                                }

                            move_results = perform_move move
                        in
                            { model
                            | status = move_results.status
                            , piece_map = move_results.piece_map
                            , active_square = move_results.active_square
                            }
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

