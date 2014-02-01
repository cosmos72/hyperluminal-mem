Hyperluminal-DB
===============

Summary
-------
Hyperluminal-DB is a high-performance, memory-mapped object database for Common Lisp.

It can also be used as a serialization/deserialization library.

Features
--------
Hyperluminal-DB is designed and optimized for the following scenarios:
- adding persistence and transactions to Lisp objects through an easy-to-use
  API, without an external relational database.
- suitable for very large datasets that can exceed available RAM.
- perfectly suited for fast storage as RAID or Solid State Disks (SSD).
- designed to allow high concurrency, i.e. hundreds or thousands of threads
  that simultaneously access the same dataset.
- designed and optimized for extremely fast transactions - as of 2013,
  theoretical peak is approx. **400 millions** concurrent transactions
  per second, on a fast desktop computer (Intel Core i7 4770, which
  supports hardware memory transactions) running 64 bit SBCL.
  - on the same hardware, micro-benchmarks exceeding **200 millions**
    concurrent transactions per second have been already measured in
    practice.
    The speed difference is largely due to the need to make hardware
    transactions compatible with software ones.
- fairly portable file format, independent from the Lisp implementation.
  File format only depends on endianity (small or big endian)
  and on user's choice between 32 and 64 bit formats.
- optimized for 64 bit systems, where dataset is limited only by `mmap()`
  maximum size (on Linux 3.x, the limit is about 128 terabytes). 
- usable on 32 bit systems, either retaining 64 bit file format
  (with some performance loss), or using native 32 bit file format - fast,
  but has the following limitations:
  - 100 user-defined persistent classes
  - 16 millions instances per user-defined persistent class
  - 256 megabytes storage per user-defined persistent class
  In both cases, dataset size is limited by `mmap()` maximum size,
  usually around 1 gigabyte

### Latest news, 1st February 2014

Released version 0.1.0. The serialization library works and is in BETA status.
The memory-mapped database - built on top of the serialization -
is the early-implementation, not yet ready for general use.

Supported systems
-----------------
Hyperluminal-DB is currently tested on the following Common Lisp implementations:

* SBCL  version 1.1.14       (x86_64) on Debian GNU/Linux 7.0  (x86_64)
* SBCL  version 1.0.57       (x86)    on Debian GNU/Linux 7.0  (x86)
* CCL   version 1.9-r15769   (x86_64) on Debian GNU/Linux 7.0  (x86_64)
* CCL   version 1.9-r15769M  (x86)    on Debian GNU/Linux 7.0  (x86_64)
* CCL   version 1.9-dev-r15475M-trunk (LinuxARM32) on Raspbian GNU/Linux (armhf) Raspberry Pi
* CMUCL version 20d Unicode  (x86)    on Debian GNU/Linux 7.0  (x86_64)
* CMUCL version 20c Unicode  (x86)    on Debian GNU/Linux 7.0  (x86)

CMUCL needs to be started with option "-fpu x87" to run Hyperluminal-DB reliably, see 
[STMX documentation](https://github.com/cosmos72/stmx/blob/master/doc/supported-systems.md)
for details.

### Unsupported systems

* [ABCL](http://abcl.org/) does not yet fully support CFFI and OSICAT, two libraries
  required by Hyperluminal-DB. Once support for these two libraries improves,
  Hyperluminal-DB can be tested on it too.

* [ECL](http://ecls.sourceforge.net/) has some known issues with CFFI, OSICAT and STMX,
  three libraries required by Hyperluminal-DB. Once support for these three libraries improves,
  Hyperluminal-DB can be tested on it too.

### Other systems

Hyperluminal-DB requires several libraries to work: LOG4CL, CLOSER-MOP, TRIVIAL-GARBAGE,
BORDEAUX-THREADS, CFFI, OSICAT and STMX. The last four, while reasonably portable,
exploit features well beyond ANSI Common Lisp and their support for the various Common Lisp
implementations varies widely.

For this reason no general guarantees can be given: Hyperluminal-DB
may or **may not** work on other, untested Common Lisp implementations.

Installation and loading
------------------------

Hyperluminal-DB is available from [GitHub](https://github.com/cosmos72/hyperluminal-db).
The simplest way to obtain it is to first install [Quicklisp](http://www.quicklisp.org)
then download Hyperluminal-DB into your Quicklisp local-projects folder.
Open a shell and run the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/hyperluminal-db.git

then open a REPL and run:

    CL-USER> (ql:quickload "hyperluminal-db")
    ;; lots of output...
    CL-USER> (use-package :hldb)
     
If all goes well, this will load Hyperluminal-DB and its dependencies:

- `log4cl`
- `closer-mop`
- `trivial-garbage`
- `bordeaux-threads`
- `cffi`
- `osicat`
- `stmx`


### Troubleshooting

In case you get errors:

- check that Quicklisp is installed correctly, for example by
  executing at REPL:

        CL-USER> (ql:quickload "closer-mop")

- check that you downloaded Hyperluminal-DB creating an `hyperluminal-db/` folder inside
  your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`


### Testing that it works

After loading Hyperluminal-DB for the first time, it is recommended to run both
STMX and Hyperluminal-DB test suites to check that everything works as expected.
From the REPL, run:

    CL-USER> (ql:quickload "stmx.test")
    ;; lots of output...

    CL-USER> (fiveam:run! 'stmx.test:suite)
    ;; even more output...
     Did 7133 checks.
        Pass: 7133 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)

    CL-USER> (ql:quickload "hyperluminal-db.test")
    ;; lots of output...

    CL-USER> (fiveam:run! 'hyperluminal-db.test:suite)
    ;; even more output...
     Did 75 checks.
        Pass: 75 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)
        
Note: `(ql:quickload "stmx.test")` and `(ql:quickload "hyperluminal-db.test")`
intentionally work only **after** `(ql:quickload "hyperluminal-db")`
has completed successfuly.

Both test suites should report zero Skip and zero Fail; the number of Pass may vary.
You are welcome to report any failure you get while running the test suites,
please include in the report:
- operating system name and version (example: Debian GNU/Linux x86_64 version 7.0)
- Common Lisp implementation and version (example: SBCL 1.0.57.0.debian, x86_64)
- EXACT output produced by the test suite
  (remember to specify if the error is in STMX test suite or in Hyperluminal-DB test suite)
- any other relevant information

See "Contacts, help, discussion" below for the preferred method to send the report.


  



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

Quite clearly, the speed also strongly depends on the amount (and type) of
data read and written during each transaction.


Status
------
As of February 2014, Hyperluminal-DB is being written by Massimiliano Ghilardi.

The serialization/deserialization library is in BETA status,
i.e. usable but not yet polished nor thoroughly tested.

The memory-mapped database (built on top of the serialization library)
is in the early-implementation stage, not yet ready for general use.


Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/hyperluminal-db/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.


Legal
-----

Hyperluminal-DB is released under the terms of the [GNU General Public
License v3.0](http://www.gnu.org/licenses/gpl-3.0.html), known
as the GPLv3.
