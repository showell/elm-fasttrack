This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:
    code cleanup:
        flatten console_view
        put active_color on model (and de-dup get_active_color)
        use holding_pen_locations
        add base_locations
        grow Config

    show only pieces that can move

    auto-move when only one choice

    jack swap/kill

    improve messaging for second seven
    make it easier to change pieces
    get-out pile

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    basic undo
    have landing screen between players
    add title hovers for squares
    add css for move errors
