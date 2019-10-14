This is an implementation of Fast Track in Elm.
It is playable for one person, but that person has
to play all the colors (which isn't as crazy as it
sounds).  The biggest next steps are these:

* Let the computer play (i.e. basic AI)
* Connect to a game server for multi-person play.

WHAT IS FAST TRACK?

    Fast track is a board game where you move pieces
    around the board from your holding pen to your
    final base.

    You draw cards to move N spaces.  Most cards have
    normal moves.  And they end turn your turn.

* 2 - move 2
* 3 - move 3
* 5 - move 5
* 7 - move 7 (but you can split among two marbles)
* 8 - move 8
* 9 - move 9
* 10 - move 10

    The four goes in reverse, and it ends your turn.

* 4 - move back 4

    The remaining cards extend your turn (A/6/J/Q/K/joker).

    These cards let you leave the holding pen:

* joker - leave pen (or move 1)
* A - leave pen (or move 1)
* 6 - leave pen (or move 1

    These cards let you leave the bullseye:

* J - leave bullseye (or move 1)
* Q - leave bullseye (or move 1)
* K - leave bullseye (or move 1)

    And then Jacks can trade.
* J - trade with opponent piece (or move 1)


HOW TO BUILD:

    elm make --optimize src/ft.elm --output ft.html

TODO:
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
