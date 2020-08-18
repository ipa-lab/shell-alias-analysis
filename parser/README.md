# Shell Alias Parser

A [Haskell] script that parses shell aliases into their constituent parts.
Works on [SQLite] databases produced by [github-searcher] and assumes the database schema given in our paper.

To run the script on a database stored in the `notebooks` directory of this repository, simply execute (from within the `parser` directory):

    cabal run parser ../notebooks/results.db

> Note: This assumes a working Haskell toolchain, which you can obtain using [ghcup].

[Haskell]: https://www.haskell.org
[SQLite]: https://www.sqlite.org
[github-searcher]: https://github.com/ipa-lab/github-searcher
[ghcup]: https://www.haskell.org/ghcup/
