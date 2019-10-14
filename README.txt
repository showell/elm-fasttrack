This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:
    snake_case -> elmCase

    show what cards can do

    update README to indicate it mostly works

    jack swap/kill:
        at least fix comment

    fix sizes

    fix before-turn-card experience:
        show pieces that can be moved

    set number of players 2/3/4/5/6

    log moves

    basic undo

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    --

    consider opaque type for piece

    maybe rename HH/DS/BR

    have landing screen between players

    add title hovers for squares
