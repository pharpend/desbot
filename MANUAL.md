# Desbot usage manual

## Using desbot-repl or the bot on IRC

### Syntax

If you are posting in a channel, you must prefix every command with a
tilde: `~`. If you are posting in a private message, you may omit the
tilde.

To direct a message at someone, use one of the following syntax things:

    person: ~command
    person, ~command

Note that, for the time being, any preceding nick in a private message
is ignored.

### Top-level commands, in alphabetical order

Any aliases are listed beneath the main command.

*  `~bugs` - Show a link to the bug tracker

    Aliases:

    + `~bug-reports`

*  `~help` - Show a brief help page.

    Aliases:

    + `~?`

*   `~license` - Post a link to desbot's license (GNU AGPLv3+).

    Aliases:

    + `~terms`

*   `~manual` - Post a link to this manual.

    Aliases:

    + `~man`

*   `~source` - Show a link to the source code.

    Aliases:

    + `~src`

## Hosting desbot

Desbot is only verified to work on Arch Linux with Stack 0.1.1. Any
Linux system will probably work.

### Installing

It is strongly recommended that you use [Stack][1] to build this
package. [Git][2] is also required.

#### Installing with Stack

Run the following commands:

    git clone git://github.com/pharpend/desbot.git
    stack setup
    stack install

At the moment, the actual bot is not functioning. However, you can test
the parser with the `desbot-repl` program:

```
Usage: desbot-repl ([-c|--config-file PATH] |
                   [-d|-m|--docs|--documentation|--manual])
  REPL to test commands for desbot

Available options:
  -h,--help                Show this help text
  -c,--config-file PATH    The path to the configuration
                           file. (default: "desbot.yaml")
  -d,-m,--docs,--documentation,--manual
                           Show desbot's manual.

```

Note that this


[1]: https://github.com/commercialhaskell/stack/wiki/Downloads
[2]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
