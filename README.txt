This is an implementation of Fast Track in Elm.
It's still very much a work in progress.

HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:
    jack swap/kill

    maybe merge Move/LegalMove
    maybe merge Config/Deck
    maybe rename HH/DS/BR

    improve messaging for second seven

    get-out pile:
        build credits
        also make clicks more rigorous

    fix before-turn-card experience:
        show pieces that can be moved

    UI to pick number of players:
        clean up Msg/Model to have game vs. setup

    basic undo
    have landing screen between players
    add title hovers for squares
    do bold with style
