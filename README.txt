This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize ft.elm --output ft.html

TODO:
    change status to title hovers

    Turn type:
        highlight active marbles
        auto-finish after move (but handle 7)
        prevent rotate until non-face card played 

    extract code from handle_square_click -> Player
    have landing screen between players
    initial deal of 5 cards
    regenerate hand after 5 face cards
    get-out pile
    disable deck once move starts
    basic undo
    jack swap/kill
    upgrade elm-format, re-run
