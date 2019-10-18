This is an implementation of Fast Track in Elm.
It is playable for one person, but that person has
to play all the colors (which isn't as crazy as it
sounds).  The biggest next steps are these:

* Let the computer play (i.e. basic AI)
* Connect to a game server for multi-person play.

## See it in action

As of now, it is deployed here:

[github pages](https://showell.github.io/ft.html)

You can also scroll down to see how to build it locally.  It
is a pretty small, self-contained project.

## Software developers

If you would like to contribute or have questions about
the project, feel free to open an issue or PR, or try
to find me (@showell) on Elm's
[Slack instance](https://elmlang.herokuapp.com/).

## What is FastTrack?

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


## How to build

This project basically only requires installing
[Elm](https://elm-lang.org/).  And then its Elm
dependencies are specified with the standard
[elm.json](https://github.com/showell/elm-fasttrack/blob/master/elm.json)
mechanism.

There are no strange external dependencies yet.  It's
pure Elm code.
    
We are deploying a non-optimized version so that
debug can be turned on.

    elm make src/Main.elm --output ft.html

## Deployment

I have this deployed using [github pages](https://showell.github.io/ft.html).

My command for doing it is this:

    cd ../fast_track/ && cp ../elm/ft.html . && git commit -am 'latest' && git push origin master && cd -

As of this writing, it's a fully self-contained single HTML file.

## Todo

see [here](https://github.com/showell/elm-fasttrack/blob/master/src/todo.txt)

