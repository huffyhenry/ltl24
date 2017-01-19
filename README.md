## Synopsis

LTL24™ is a software tool for querying and verification of event-level football 
data collected and marketed by 
[Opta](http://www.optasportspro.com) 
in the form of "F24 feeds". The tool uses a formal specification of queries and 
requirements inspired by 
[Linear Temporal Logic](https://en.wikipedia.org/wiki/Linear_temporal_logic).
This modal logic design enables the user to make context-sensitive queries 
which are impossible or (at best) cumbersome with SQL, and consequently ensure F24 data 
integrity at the cross-event level. An additional application of this system is
querying of the data, based on the idea that searching for a feature 
is dual to proving the negation of that feature.
 
## Motivating example
Consider the following requirement:

> A shot on target (Event type 15) must be immediately followed by a save (Event type 10).

This requirement cannot be checked against given F24 match data if that data 
resides in an unannotated SQL database because the Event objects carry no 
information about what precedes or follows them. Moreover, even if the database 
is annotated with temporal ordering of Events, the appropriate SQL query is 
still relatively cumbersome and inefficient. By contrast, LTL24 expresses it
in an entirely natural fashion as:

> **always**(type_id=15 **implies** (**next** type_id=10))

using LTL's temporal modalities `always` and `next`, 
and the propositional connective `implies`.

## Installation
To use LTL24, you first 
need to install the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) and
the [xml](https://hackage.haskell.org/package/xml), 
[parsec](https://hackage.haskell.org/package/parsec) and 
[datetime](https://hackage.haskell.org/package/datetime) packages from 
[hackage.haskell.org](https://hackage.haskell.org). Then compile the sources with
`$ ghc Main.hs`.


## Example session
At the moment, LTL24 is an interactive console system. Bundled with this 
project is a file `specs.txt` with two sample requirements:
 
> halves: **exists** (type_id=32 **and** period_id=1 **and** **next** (**exists** (type_id=32 **and** period_id=2)));
>
> saves: **always** (type_id=15 **implies** (**next** type_id=10));

The `halves` requirement means simply that first and second half start events
exist and the start of the second must follow the start of the first.

Also in the project is the `f24sample.xml` file, which is 
an anonymized -- names, dates and IDs altered, otherwise untouched -- F24 coding of 
a recent football game (unfortunately I was denied
permission from Opta to include the original file). The following sample session 
lets us determine if the file satisfies the two requirements:

> marek@jules:/home/marek/Projects/ltl24 (master)$ ./Main<br>
> Welcome to LTL24 v0.1.0.<br>
> LTL24$ help<br>
> Available commands: quit, help, about, load, status, verify, drop.<br>
> LTL24$ help load<br>
> load -- add games or specs to the active environment.<br>
> USAGE: load [game | spec] \<filename><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;load spec inline \<name>:\<formula><br>
> LTL24$ load spec specs.txt<br>
> 0 game(s).<br> 
> 2 spec(s): halves, saves.<br>
> LTL24$ load game f24sample.xml<br>
> 1 game(s): 476843.<br>
> 2 spec(s): halves, saves.<br>
> LTL24$ verify<br>
> Verifying game 476843.<br>
> halves: passed<br>
> saves: failed<br>
> LTL24$ quit<br>
> Really quit [y/n]? y<br>
> Bye.

As we see, the `halves` requirement is satisfied by this data file, but `saves`
is not. There must be a shot on target in the Event stream that is not 
immediately followed by a save. In the future versions (if any) of the software,
the actual instance of the Event violating the requirement (known as "counterexample") 
should be produced, enabling the user to search for patterns of interest by attempting to prove their negation.

