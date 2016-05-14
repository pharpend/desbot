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

## Usage

To use `desbot` on the network, you can either target it in a channel,
or send it a private message.

*   In a channel, you need to prefix your command with `desbot: ` or
    `desbot, `:

        <nick> desbot: 1 + 1
        <desbot> nick: 2

*   In a private message, don't do that

        <nick> 1 + 1
        <desbot> 2

### Running on a server

I copied the [keter upstart script][ketup], and modified it to work with
desbot. The script can be found in
[`config/desbot.conf`](config/desbot.conf).

    $ sudo cp config/desbot.conf /etc/init

**IMPORTANT**: edit the init script and set the user to whoever you
are.

    $ sudo -e /etc/init/desbot.conf

You can find out which user you are by running

    $ whoami

## Contact

* Bugs and/or questions should be reported in the
  [GitHub bug tracker][issues].
* You can email me at `peter@harpending.org` or `pharpend` on
  FreeNode.net.

[lb]: https://github.com/lambdabot/lambdabot
[ketup]: https://github.com/snoyberg/keter/blob/master/setup-keter.sh#L50
[stack]: http://docs.haskellstack.org/en/stable/README/
