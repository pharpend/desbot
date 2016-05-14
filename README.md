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

## Things to do

I'm too lazy to open a new GitHub issue for each of these.

* Desbot needs to display type information when queried with `:t` or
  `:type`
* Desbot needs to display kind information when queried with `:k` or
  `:kind`
* Desbot needs to link to its source code when given `:src` or
  `:source`.
* Desbot needs to respond to `CTCP SOURCE`.
* Desbot needs to display its version when given `:version`.
* I need to clean up the mueval code so that it can be used as a proper
  library, rather than just running `spawnCommand`. I have a fork called
  "nueval", which can be found here:
  <https://github.com/pharpend/nueval>.
* I need to come up with a better name than "nueval", because it's too
  close to "mueval", and not nearly as clever as I thought it would be.
* Desbot needs to have some sort of daemon/client mode, so I can have it
  reload its configuration on-the-fly.
* Desbot needs to respond to `desbot: help` or `desbot: manual`. This
  can be done easily by adding a function called `help` and putting it
  in a loadfile for mueval.
* Desbot needs to run in some sort of security container, like
  [nsjail](https://google.github.io/nsjail/). This is to prevent
  intrusion on the off chance someone manages to find a security
  hole. mueval does an excellent job of sandboxing, but there are still
  probably holes.
  
  Right now, desbot is running on a server running Ubuntu 14.04, which
  has AppArmor. Once I can arrange it, I will probably move it to a
  Linux server running SELinux and grsecuity, or a server running
  OpenBSD, which will further enhance its security.

* Desbot needs to be startable with systemd and/or upstart and/or BSD
  init. This will be much more easily solvable once my mueval fork is
  complete, and we can bundle it as a library rather than depending on
  the `mueval` binary.

## Contact

* Bugs and/or questions should be reported in the
  [GitHub bug tracker](https://github.com/pharpend/desbot/issues).
* You can email me at `peter@harpending.org` or `pharpend` on
  FreeNode.net.

[lb]: https://github.com/lambdabot/lambdabot
[ketup]: https://github.com/snoyberg/keter/blob/master/setup-keter.sh#L50
[stack]: http://docs.haskellstack.org/en/stable/README/
