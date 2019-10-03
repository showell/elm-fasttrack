This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:

    change prev/next to start/end in Move type

    handle end-game need-to-reverse

    improve messaging for second seven

    finish handling sevens
    show only pieces that can move


    jack swap/kill

    make it easier to change pieces
    get-out pile

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    basic undo
    have landing screen between players
    add title hovers for squares
    add css for move errors
