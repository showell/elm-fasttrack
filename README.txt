This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:

    introduce move_type into tuples (to kill Distance):
        include StartSplit move type

    jack swap/kill

    get-out pile:
        build credits
        also make clicks more rigorous

    fix before-turn-card experience:
        show pieces that can be moved

    basic undo

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    --

    consider opaque type for piece

    log moves

    maybe rename HH/DS/BR

    have landing screen between players

    add title hovers for squares
