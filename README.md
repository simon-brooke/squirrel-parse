# squirrel-parse

A Clojure library designed to parse SQL files into usable Clojure data
structures, for automatic generation of things like
[HugSQL](https://www.hugsql.org/) and [SQL Korma](http://sqlkorma.com/)
(etc) boilerplate code.

## Usage

This is not production ready code as yet. Nevertheless if you want a
sensible entry point, look at the two example functions in `squirrel-parse.core`.



## Status

What is here at present is proof-of-concept code. It does sort-of
work, for a limited subset of Postgres SQL. But it's pretty fragile
and the main issue is that slurping a whole SQL dump at a time tends
to make `instaparse` crash out of memory - even for seriously large
amounts of memory.

Line-by-line parsing won't work because SQL statements tend to span
multiple lines. So what's needed is

1. Clear the input buffer;
2. If the input stream is at end of stream, terminate;
3. Read a line from the stream and append it to the input buffer;
4. Attempt to parse a statement;
5. If successful, append the parsed statement to the current list of
parsed statements and go to 1; else go to 2.

It may also be desirable to split the grammar into modules, each of
which is capable of parsing one sort of statement. This would limit
the memory cost of a parse operation, at the expense of requiring many
more parse operations.

Obviously all this is doable but I'm not there yet!

## License

Copyright © 2018 Simon Brooke

Distributed under the terms of the
[GNU General Public License v2](http://www.gnu.org/licenses/gpl-2.0.html)
