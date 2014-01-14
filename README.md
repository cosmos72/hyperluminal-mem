Hyperluminal-DB
===============

Summary
-------
Hyperluminal-DB is a high-performance, memory-mapped object database for Common Lisp.

It is designed and optimized for the following scenarios:
- adding persistence and transactions to Lisp objects through an easy-to-use
  API, without an external relational database.
- suitable for very large datasets that can exceed available RAM.
- perfectly suited for fast storage as RAID or Solid State Disks (SSD).
- designed to allow high concurrency, i.e. hundreds or thousands of threads
  that simultaneously access the same dataset.
- designed and optimized for extremely fast transactions - as of 2013,
  theoretical peak is approx. **400 millions** concurrent transactions
  per second on a fast desktop computer (Intel Core i7 4770, which
  supports hardware memory transactions) running 64 bit SBCL.
- optimized for 64 bit systems, where dataset is limited only by `mmap()`
  maximum size (on Linux 3.x, the limit is about 128 terabytes). 
- usable on 32 bit systems, with the following limitations:
  - 100 user-defined persistent classes
  - 16 millions instances per user-defined persistent class
  - 256 megabytes storage per user-defined persistent class
  - also limited by `mmap()` maximum size, usually around 1 gigabyte

Implementation
--------------
Hyperluminal-DB is loosely inspired by some techniques used by
[manardb](http://cl-www.msi.co.jp/projects/manardb/index.html)
but it is a completely separate and independent project.

It is based on [STMX](https://github.com/cosmos72/stmx),
a high-performance hybrid transactional memory library from the same
author.

Hyperluminal-DB uses a (supposedly) clever trick in order to overcome Intel
claims that hardware memory transactions (specifically, Intel TSX) cannot
perform input/output.

The result is that Hyperluminal-DB is able to perform **transactional**
input/output while running hardware memory transactions - an apparent
paradox - in an extremely specific but significant case: reading and
writing mmap() memory.

This allows reaching extremely high transaction speeds: the only hard limit
is the hardware - an Intel Core i7 4770 peaks at **400 millions** transactions
per second when all the 4 cores and hyperthreading are exploited.


Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/hyperluminal-db/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.

Status
------

As of January 2014, Hyperluminal-DB is being written by Massimiliano Ghilardi
and is in the early-implementation stage, not yet ready for general use.

Legal
-----

Hyperluminal-DB is released under the terms of the [GNU General Public
License v3.0](http://www.gnu.org/licenses/gpl-3.0.html), known
as the GPLv3.
