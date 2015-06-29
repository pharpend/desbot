# desbot

This is an IRC bot for the [`#snowdrift` channel on FreeNode][1].

For detailed usage information, please see the [manual][11].

## Coding guidelines

I roughly follow [Chris Done's style guide][8] for Haskell code.

I strongly suggest you use [Stack][9] for development. You can use the
old cabal sandbox way, but Stack is much nicer. desbot only compiles
with GHC 7.10, so you have to use Stack (or maybe [Halcyon][10]) if you
have an older version.

To use stack:

    git clone git://github.com/pharpend/desbot.git
    cd desbot
    stack setup
    stack build
    stack exec -- desbot --help

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
[11]: MANUAL.md
