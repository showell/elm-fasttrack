This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize ft.elm --output ft.html

TODO:
    Turn type:
        make sure current card is InProgress
        move active_card on to Turn
        prevent move until card is played
        prevent rotate until non-face card played 
        clear square on rotate (may be N/A)

    have landing screen between players
    initial deal of 5 cards
    regenerate hand after 5 face cards
    get-out pile
    disable deck once move starts
    basic undo
    jack swap/kill
    upgrade elm-format, re-run
