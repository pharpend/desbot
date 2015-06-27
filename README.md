# icebot

This is an IRC bot for the [`#snowdrift` channel on FreeNode][1].

## Installation

I have tested this on Arch Linux with stack 0.0.3

1.  Install [stack][2] and [git][3].
2.  Run these commands in a terminal:

        git clone git://github.com/pharpend/icebot.git
        cd icebot
        stack install

## Usage

```
Usage: icebot ([-c|--config-file PATH] | [-e|--config-example])
  A useless IRC bot

Available options:
  -h,--help                Show this help text
  -c,--config-file PATH    The path to the configuration
                           file. (default: "icebot.yaml")
  -e,--config-example      Show an example configuration file
```

[1]: https://webchat.freenode.net/?channels=#snowdrift
[2]: https://github.com/commercialhaskell/stack/wiki/Downloads
[3]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
