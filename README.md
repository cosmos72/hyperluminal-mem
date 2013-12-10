Hyperluminal-DB
===============

Summary
-------
Hyperluminal-DB is a high-performance, memory-mapped database for Common Lisp.

It is designed and optimized for the following scenarios:
- adding persistence and transactions to Lisp objects through an easy-to-use
  API, without an external relational database.
- suitable for very large datasets that can exceed available RAM.
- perfectly suited for fast storage as RAID or Solid State Disks (SSD).
- designed to allow high concurrency, i.e. hundreds or thousands of threads
  that simultaneously access the same dataset.
- designed and optimized for extremely fast transactions - as of 2013,
  theoretical peak is approx. **400 millions** transactions per second
  on a fast desktop computer (Intel Core i7 4770, which supports
  hardware memory transactions) running 64 bit SBCL.
- optimized for 64 bit systems, where dataset is limited only by `mmap()`
  maximum size (on Linux 3.x, the limit is approx. 128 terabytes). 
- usable on 32 bit systems, with restrictions on the dataset size
  (approx. 256 megabytes). 

Hyperluminal-DB is loosely inspired by some techniques used by
[manardb](http://cl-www.msi.co.jp/projects/manardb/index.html)
but it is a completely separate and independent project.

It is based on [STMX](https://github.com/cosmos72/stmx), a high-performance
hybrid transactional memory library from the same author.

Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/hyperluminal-db/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.

Status
------

As of December 2013, Hyperluminal-DB is being written by Massimiliano Ghilardi
and is in the early-implementation stage, not yet ready for general use.

Legal
-----

Hyperluminal-DB is released under the terms of the [GNU General Public
License v3.0](http://www.gnu.org/licenses/gpl-3.0.html), known
as the GPLv3.
