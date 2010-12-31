Datalog Notes
=============

As facts are inserted into the database, track the ranges of each
variable.  This is necessary to record the number of bits required
to track each predicate.

Read up on techniques for avoiding useless computations in datalog;
hash consing, magic sets.
