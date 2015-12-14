## Synopsis

LTL24 is a software tool for querying and verification of event-level football 
data collected and marketed by 
[Opta](http://www.optasportspro.com) 
in the form of "F24 feeds". The tool uses a formal specification of queries and 
requirements inspired by 
[Linear Temporal Logic](https://en.wikipedia.org/wiki/Linear_temporal_logic).
This modal logic design enables the user to make context-sensitive queries 
which are all but impossible with SQL, and consequently ensure F24 data 
integrity at the cross-event level.
 
## Example
Consider the following requirement:

> A shot on target (Event type 15) must be immediately followed by a save (Event type 10).

This requirement cannot be checked against given match data if that data 
resides in an unannotated SQL database because the Event objects carry no 
information about what precedes or follows them. Moreover, even if the database 
is annotated with temporal ordering of Events, the appropriate SQL query is 
still cumbersome and inefficient. By contrast, LTL24 expresses this requirement
in an entirely natural fashion as:

> `always(type_id=15 implies (next type_id=10))`

using LTL's temporal modalities `always` and `next`.

## Copyright notice
Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
Unlicensed use and distribution prohibited.
