Fast and memory efficient low-level networking
==============================================

The network-bytestring library provides faster and more memory
efficient low-level socket functions, using `ByteString`s, than those
in the `network` library.

Contributing
------------

Make sure you read the Haskell style guide at:

http://github.com/tibbe/haskell-style-guide

The existing code doesn't follow the style guide fully but you should
follow it for all new code.

The preferred way of contributing changes to the project is to use Git
and send the patches over email using the Git commands `format-patch`
and `send-email`.  Step by step instructions:

Clone the repository:

    git clone github.com/tibbe/network-bytestring,

Make your changes:

    cd network-bytestring
    $EDITOR <file>

Commit your changes in one or more commits:

    git add <file>
    git commit

Make sure you write a good commit message.  Commit messages should
contain a short summary on a separate line and, if needed, a more
thorough explanation of the change.  Write full sentences and use
proper spelling, punctuation, and grammar.

You might want to use `git rebase` to make sure your commits
correspond to nice, logical commits.  Make sure whitespace only
changes are kept in separate commits to ease reviewing.

Prepare the patches for sending:

    git format-patch <revision>

Pick the revision number of the commit preceding your (first) commit.

    git send-email --to <maintainer> <patch files>

The maintainer is specified in the Cabal file.  The maintainer will
review your changes and may ask you to make changes to them.  Make the
changes to your local repository and use `git rebase` to massage them
into nice, logical commits and resend the patches.
