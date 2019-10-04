This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:
    auto-move when only one choice
    enforce legal moves
    rip out validation code
    seven: force split move once started

    jack swap/kill

    maybe merge Move/LegalMove
    maybe merge Config/Deck
    maybe rename HH/DS/BR

    improve messaging for second seven
    make it easier to change pieces
    get-out pile

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    basic undo
    have landing screen between players
    add title hovers for squares
    add css for move errors
