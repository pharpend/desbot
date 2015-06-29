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

If you are using this in a production-ish environment, use:

    stack install -j 5

If you are developing Stack, use:

    stack build -j 5

If you used `stack build`, prefix all of the `desbot` commands below
with `stack exec --`. So, `desbot --repl` becomes `stack exec -- desbot
--repl`.

#### Installing the old fashioned way

I haven't tested this, but this is the usual formula:

    git clone git://github.com/pharpend/desbot.git
    cabal update
    cabal install

### Running the bot

At the moment, the actual bot is not functioning. However, you can test
the parser with `desbot --repl`:

```
Usage: desbot [-c|--config-file PATH] ([-e|--config-example] |
              [-i|--interactive|--repl] | [-m|--man|--manual])
  A useless IRC bot. For a manual see
  <https://github.com/pharpend/desbot/blob/master/MANUAL.md>.

Available options:
  -h,--help                Show this help text
  -c,--config-file PATH    The path to the configuration
                           file. (default: "desbot.yaml")
  -e,--config-example      Show an example configuration file
  -i,--interactive,--repl  Run a REPL to test commands to desbot.
  -m,--man,--manual        Show desbot's manual.
```

Note that if you give `desbot` no arguments, it will attempt to run the
bot. At the moment, the bot part doesn't work, so you'll probably just
get an error message.

#### Generating a desbot.yaml

Before you get started, you need to generate a configuration file with
`desbot --config-example`. You will likely want to change the
configuration.

As of 28 June 2015, this is the output of the aforementioned command:

```yaml
# -*- mode: yaml -*-
# 
# Desbot configuration file example

# This isn't required, but if you're going to fork the bot, you probably want to
# change these values:
info:
  source: "https://github.com/pharpend/desbot"
  bugs: "https://github.com/pharpend/desbot/issues"
  manual: "https://github.com/pharpend/desbot/tree/master/MANUAL.md"

# Configuration for desbot-repl. This is optional, but it's listed here so you
# can change the values.
repl:
  prompt: ">>= "
  # The name by which the bot addresses you when it has an error message.
  name: "luser"
  
# A list of servers to which the bot connects. Don't connect to too many
# servers, because the bot creates a new thread for each server.
# 
# This is the only required field. If you want to be really lean, you can just
# have the list of servers at the top-level, leave everything else to be
# inferred.
servers:
    # The only required fields are hostname and nick. The rest can be
    # inferred. The rest are listed here so you know what they are.
  - hostname: irc.freenode.net
    nick: desbot-example
    port: 6667
    username: desbot-example
    id: freenode
    # It's highly recommended that you have a log in case the bot crashes. By
    # default, the log is "/dev/null".
    log-file: freenode.log
    # This field technically isn't required, but it really should be. By default,
    # desbot will not join any channels.
    channels:
      - "#snowdrift"
      - "#test"
    # Get password from stdin
    # 
    # * If you set password to "prompt" or "stdin", desbot will prompt you for it
    #   when you start the bot.
    # * If you set "password: null", or delete the password field, desbot will
    #   assume the nick is unregistered, and therefore doesn't need a password.
    # * Otherwise, the password is whatever you put in this field.
    password: prompt

    # Another example.
  - hostname: irc.oftc.net
    nick: desbot-example
    id: oftc
    log-file: oftc.log
    channels:
      - "#git-annex"
      - "#debian"
```

A minimal configuration is:

```yaml
- hostname: irc.freenode.net
  nick: desbot-example
```

The rest of the values can be inferred. The comments do a pretty good
job of explaining what each field does, so I'll leave you to read those.

By default, `desbot` will try to read `desbot.yaml` in the current
directory. You can specify an alternate file with `-c
/path/to/other/yaml/file`.

### Using the desbot REPL

Desbot has a read-eval-print-loop (REPL) built in, so you can try out
commands as if you were private-messaging `desbot` on IRC. **Note that
use of the REPL still requires a `desbot.yaml` file.**


    $ desbot --repl
    >>= ~help
    I am desbot, the channel bot and dictator.
    My full manual can be found with `~manual`.

Since this imitates a private message session, you do not need to use
the `~` preceding commands.

    >>= manual
    https://github.com/pharpend/desbot/tree/master/MANUAL.md
    >>= ~manual
    https://github.com/pharpend/desbot/tree/master/MANUAL.md
    >>= source
    https://github.com/pharpend/desbot

If you target someone, using `nick, ` or `nick: ` in a private message,
desbot will ignore this. In the future, this will serve as a messaging
function.

[1]: https://github.com/commercialhaskell/stack/wiki/Downloads
[2]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
