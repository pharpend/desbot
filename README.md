# desbot

This is an IRC bot for the [`#snowdrift` channel on FreeNode][1].

## How to use the bot

At the moment, the bot responds to three commands:

1. `~help`
2. `~source`
3. `!robotrollcall`

It's a really crappy bot at the moment. Any help is appreciated. See the
"Coding guidelines" section of this document for additional information.

Bugs and general questions should be reported in the
[GitHub bug tracker][6]. See the "Contact" section of this document for
more information.

## Installation and Usage

I have tested this on Arch Linux with stack 0.1.1

1.  Install [stack][2] and [git][3].
2.  Run these commands in a terminal:

        git clone git://github.com/pharpend/desbot.git
        cd desbot
        stack setup
        stack install

To run the bot on your own system:

```
Usage: desbot ([-c|--config-file PATH] | [-e|--config-example])
  A useless IRC bot

Available options:
  -h,--help                Show this help text
  -c,--config-file PATH    The path to the configuration
                           file. (default: "desbot.yaml")
  -e,--config-example      Show an example configuration file
```

Press `RET` to kill the bot.

## Coding guidelines

This program is split into two separate components (see
[`desbot.cabal`][4]):

1. The desbot library, to be found in [`lib/Network/IRC`](lib/Network/IRC)
2. The desbot executable, to be found in [`src/Main.hs`](src/Main.hs)

I roughly follow [Chris Done's style guide][8] for Haskell code.

I strongly suggest you use [Stack][9] for development. You can use the
old cabal sandbox way, but Stack is much nicer. desbot only compiles
with GHC 7.10, so you have to use Stack (or maybe [Halcyon][10]) if you
have an older version.

To use stack:

    stack setup
    stack build
    stack exec desbot

## Contact

* Email: `peter@harpending.org`
* IRC: `pharpend` on FreeNode

[1]: https://webchat.freenode.net/?channels=#snowdrift
[2]: https://github.com/commercialhaskell/stack/wiki/Downloads
[3]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[4]: desbot.cabal
[6]: https://github.com/pharpend/desbot/issues
[8]: https://github.com/chrisdone/haskell-style-guide/blob/master/README.md
[9]: https://github.com/commercialhaskell/stack/wiki
[10]: https://halcyon.sh/
