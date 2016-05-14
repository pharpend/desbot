# desbot

This is a small IRC bot for Haskell evaluation, along the lines of
[lambdabot][lb]. In `#learnprogramming`, we continually have people new
to programming and IRC in the channel. A lot of newbies think IRC is
Twitter, and direct a message at someone using the syntax `@nick` rather
than the conventional `nick:` or `nick,`. Prefixing a line with `@` will
trigger lambdabot.

    <newbie> Hello, I have X problem
    <veteran> newbie: could you paste the code somewhere
    <newbie> @veteran sure!
    <lambdabot> Unknown command, try @list

The ratio of instances of `lambdabot` being useful in a
non-Haskell-focused channel to instances of confusing newbies was enough
that I decided to write this program.

## Installation

You need to install [stack] first.

    $ git clone https://github.com/pharpend/desbot.git
    $ cd desbot
    $ stack setup
    $ stack install

That will take a long time, so go get some coffee or something. You
need to do some setup before running `desbot`.

First, generate the needed configuration file and paths:

    $ desbot scaffold

Follow the instructions given. You should then be ready to go!

    $ desbot run

## Contact

* Bugs and/or questions should be reported in the
  [GitHub bug tracker][issues].
* You can email me at `peter@harpending.org` or `pharpend` on
  FreeNode.net.

[lb]: https://github.com/lambdabot/lambdabot
[stack]: http://docs.haskellstack.org/en/stable/README/
