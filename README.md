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
- designed for safety: it can be used on untrusted and possibly malicious data,
  as for example serialized packets or files received from the internet.
- fairly portable file format, independent from the Lisp implementation.
  File format only depends on endianity (small or big endian)
  and on user's choice between 32 and 64 bit formats.
  Conversion between small and big endian file format is trivial.
- optimized for 64 bit systems, where dataset is limited only by `mmap()`
  maximum size (on Linux 3.x, the limit is about 128 terabytes).
- usable on 32 bit systems, either retaining 64 bit file format
  (with some performance loss), or using native 32 bit file format - fast,
  but has the following limitations:
  - 100 user-defined persistent classes
  - 16 millions instances per user-defined persistent class
  - 256 megabytes storage per user-defined persistent class
  
  In any case, on 32 bit systems the dataset size is limited by `mmap()`
  maximum size, usually around 1 gigabyte

### Latest news, 9th February 2014

Released version 0.5.0.

The serialization library is tested, documented and ready to use.
It may still contain some rough edges and small bugs.

### News, 1st February 2014

The serialization library works and is in BETA status.

The memory-mapped database (built on top of the serialization library)
is in the early-implementation stage, not yet ready for general use.

Supported systems
-----------------
Hyperluminal-DB is currently tested on the following Common Lisp implementations:

* [SBCL](http://sbcl.org/)
  * version 1.1.15       (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.1.14       (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.1.14       (powerpc)  on Debian GNU/Linux jessie (powerpc) inside Qemu
  * version 1.0.57       (x86)      on Debian Ubuntu Linux 12.04LTS (x86)

* [CCL](http://ccl.clozure.com/)
  * version 1.9-r15769   (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.9-r15769M  (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.9-r15761   (linuxppc) on Debian GNU/Linux jessie (powerpc) inside Qemu
  * version 1.9-dev-r15475M-trunk (LinuxARM32) on Raspbian GNU/Linux (armhf) Raspberry Pi

* [CMUCL](http://www.cons.org/cmucl/)
  * version 20d Unicode  (x86)      on Debian GNU/Linux jessie  (x86_64)
  * version 20c Unicode  (x86)      on Debian GNU/Linux jessie  (x86_64)

CMUCL needs to be started with the option `-fpu x87` to run Hyperluminal-DB reliably, see 
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
    CL-USER> (use-package :hlmem)
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
     Did 2505 checks.
        Pass: 2505 (100%)
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
- **exact** output produced by the test suite
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
writing memory-mapped files.

This allows reaching extremely high transaction speeds: the only hard limit
is the hardware - an Intel Core i7 4770 peaks at **400 millions** transactions
per second when all the 4 cores and hyperthreading are exploited.

Quite clearly, the speed also strongly depends on the amount (and type) of
data read and written during each transaction.


Basic usage
-----------

Hyperluminal-DB offers the following Lisp types, constants, macros and functions,
also documented in the sources - remember `(describe 'some-symbol)` at REPL.

- `MADDRESS` is the type of raw memory pointers.
   It is currently an alias for the type `cffi-sys:foreign-pointer`

- `MEM-WORD` is the type of a word of raw memory.
   It is normally autodetected to match the underlying CPU registers,
   i.e. `mem-word` is normally `(unsigned-byte 32)` on 32-bit systems,
   `(unsigned-byte 64)` on 64-bit systems, and so on... but it is also possible
   to override such autodetection and configure it manually.
   See the section **File Format and ABI** below for details.

- `+MSIZEOF-WORD+` is a constant equal to the number of bytes in a word.
   It is autodetected to match the definition of `mem-word`.

- `MEM-SIZE` is a type: it represents the length of a raw memory block,
   counted in words (not in bytes). Used by all the functions that manipulate
   raw memory in units of `mem-words` - which means most Hyperluminal-DB functions.

   For the curious, in practice it is `(unsigned-byte 30)` on 32-bit systems,
   `(unsigned-byte 61)` on 64-bit systems, and so on...

- `(MALLOC n-bytes)` is a function, it allocates raw memory and returns a raw pointer to it.

   It is actually a simple alias for the function `(cffi-sys:%foreign-alloc n-bytes)`
   and it is equivalent to the function `void * malloc(size_t n_bytes)` found in C/C++ languages.

   Definition:

        (defun malloc (n-bytes)
          (declare (type unsigned-byte n-bytes))

          (the maddress (cffi-sys:%foreign-alloc n-bytes)))

   Remember that, as in C/C++, the memory returned by `malloc` must be deallocated manually:
   call `mfree` on it when no longer needed.

- `(MALLOC-WORDS n-words)` is a function, it allocates raw memory and returns
   a raw pointer to it just like `malloc`.
   It is usually more handy than `malloc` since almost all Hyperluminal-DB functions
   count and expect memory length in words, not in bytes.

   Definition:

        (defun malloc-words (n-words)
          (declare (type mem-size n-words))

          (the maddress #| ...implementation... |# ))

- `(MFREE ptr)` deallocates raw memory previously obtained with `malloc-words`, `malloc`
   or `cffi-sys:%foreign-alloc`. It is actually a simple alias for the function
   `cffi-sys:foreign-free` and it is equivalent to the function `void free(void * ptr)`
   found in C/C++ languages.

   Definition:

        (defun mfree (ptr)
          (declare (type maddress ptr))

          (cffi-sys:foreign-free ptr))

- `(WITH-MEM-WORDS (ptr n-words [n-words-var]) &body body)`
   is a macro that binds PTR to N-WORDS words of raw memory while executing BODY.
   The raw memory is automatically deallocated when BODY terminates.

   `with-mem-words` is an alternative to `malloc` and `malloc-words`,
   useful if you know in advance that the raw memory can be deallocated after BODY finishes.
   It is a wrapper around the CFFI macro
   `(cffi-sys:with-foreign-pointer (var size &optional size-var) &body body)`
   which performs the same task but counts memory size in bytes, not in words.

- `(MSIZE value [index])` is a function that examines a Lisp value, and tells
   how many words of raw memory are needed to serialize it.

   It is useful to know how large a raw memory block must be
   in order to write a serialized value into it. It is defined as:

        (defun msize (value &optional (index 0))
          (declare (type t value)
                   (type mem-size index))
          (the mem-size (+ index
                           #| ...implementation... |#)))

   The optional argument INDEX is useful to compute the total size of composite values,
   as for example lists, arrays, hash-tables and objects: the value returned by `msize`
   is increased by the value of `index`, so the following three code snippets are equivalent

        (+ (msize "foo") (msize 'bar))

        (let ((index (msize "foo")))
          (msize 'bar index))

        (msize 'bar (msize "foo"))

   with the advantage that the second and third versions automatically check
   for length overflows and can exploit tail-call optimizations.

   `msize` supports the same types as `MWRITE` below, and can be extended similarly
   to support arbitrary types, see `MSIZE-OBJECT` and `MWRITE-OBJECT` for details.
   
- `(MWRITE ptr index end-index value)` serializes a Lisp value, writing it into raw memory.
   It is defined as:
  
        (defun mwrite (ptr index end-index value)
          (declare (type maddress ptr)
                   (type mem-size index end-index)
                   (type t value))

            (the mem-size #| ...implementation... |#))

   To use it, you need three things beyond the value to serialize:
   * a pointer to raw memory, obtained for example with one of
     `malloc-words`, `malloc` or `with-mem-words` described above.
   * the offset (in words) where you want to write the serialized value.
     It must be passed as the `index` argument
   * the available length (in words) of the raw memory.
     It must be passed as the `end-index` argument
   
   `mwrite` returns an offset pointing immediately after the serialized value.
   This allows to easily write consecutive serialized values into the raw memory.
   
   Any kind of raw memory is supported, thus it is also possible to call `mwrite`
   on memory-mapped files. This is actually the mechanism that allows Hyperluminal-DB
   to implement an object store backed by memory-mapped files.

   `mwrite` supports the following standard Lisp types:
   * integers - both fixnums and bignums
   * ratios, as for example 2/3
   * single-floats, double-floats and complexes
   * characters - full Unicode range is supported
   * symbols and keywords
   * cons cells and their aggregates: lists, alists, plists, trees
   * vectors and arrays of any rank (i.e. any number of dimensions)
   * strings
   * pathnames
   * hash-tables
  
   It also supports the following types implemented by STMX (requires latest STMX from GitHub):
   * tcell - simple transactional variable
   * tstack - transactional first-in last-out stack
   * tmap and rbmap - sorted maps, both transactional (tmap) or non-transactional (rbmap)
   * thash-table and ghash-table - hash tables, both transactional (thash-table)
     and non-transactional (ghash-table)

   Finally, it can be easily extended to support arbitrary types,
   see `MWRITE-OBJECT` for details.

- `(MREAD ptr index end-index)` deserializes a Lisp value, reading it from raw memory.
   It is defined as:
  
        (defun mread (ptr index end-index)
          (declare (type maddress ptr)
                   (type mem-size index end-index))

            (the (values t mem-size)
                 #| ...implementation... |#))

   It returns two values: the value itself, and an offset pointing immediately after
   the serialized value inside raw memory. This allows to easily read consecutive
   serialized values from the raw memory.

   `mread` supports the same types as `mwrite` and it can be extended similarly,
   see `MREAD-OBJECT` and `MWRITE-OBJECT` for details.

- `(MSIZE-OBJECT object index)` is a generic function that examines a user-defined
   Lisp object and tells how many words of raw memory are needed to serialize it.

   Programmers can extend Hyperluminal-DB by defining specialized methods for it,
   see `MWRITE-OBJECT` for details.

- `(MREAD-OBJECT ptr index end-index &key)` is a generic function that reads
   a serialized user-defined object from raw memory, deserializes and returns it.

   Programmers can extend Hyperluminal-DB by defining specialized methods for it,
   see `MWRITE-OBJECT` for details.

- `(MWRITE-OBJECT object ptr index end-index)` is a generic function
   that serializes a user-defined Lisp object, writing it into raw memory.

   Programmers can extend Hyperluminal-DB by defining specialized methods for
   `msize-object`, `mwrite-object` and `mread-object`. Such methods are invoked
   automatically by `msize`, `mwrite` and `mread` when they encounter a user-defined object,
   i.e. an instance of structure-object or standard-object or their subclasses.

   The task of `msize-object`, `mwrite-object` and `mread-object` is relatively
   straightforward: they are supposed to cycle through the relevant instance slots
   (or accessors) and recursively call `msize`, `mwrite` or `mread` on each slot.
   
   For example, if a `POINT3D` class is defined as

        (defclass point3d ()
          ((x :initarg :x :initform 0.0 :accessor point3d-x)
           (y :initarg :y :initform 0.0 :accessor point3d-y)
           (z :initarg :z :initform 0.0 :accessor point3d-z)))

   then a reasonable specialization of `msize-object` is:
    
        (defmethod msize-object ((p point3d) index)
          (let* ((index-x (msize (point3d-x p) index))
                 (index-y (msize (point3d-y p) index-x))
                 (index-z (msize (point3d-z p) index-y)))
             index-z))
   
   note how the result of each `msize` call is passed to the next call - this ensures
   that the sum of the sizes is computed automatically, and also takes care of signalling
   an error in case of overflow.

   A shorter, slightly automagic alternative is to use the macro `msize*`
   which expands to multiple calls of `msize` and correctly passes around the
   intermediate `index` values:

        (defmethod msize-object ((p point3d) index)
          (msize* index (point3d-x p) (point3d-y p) (point3d-z p)))

   Note how `index` is now the first argument, not the last.

   Similarly, `mwrite-object` can be specialized as:

        (defmethod mwrite-object ((p point3d) ptr index end-index)
          (let* ((index-x (mwrite (point3d-x p) ptr index   end-index))
                 (index-y (mwrite (point3d-y p) ptr index-x end-index))
                 (index-z (mwrite (point3d-z p) ptr index-y end-index)))
             index-z))

   which uses the same `index`-passing mechanism to compute the serialized value total size.
   Again a shorter, slightly automagic alternative is to use the macro `mwrite*`
   which expands to multiple calls of `mwrite` and correctly passes around the
   intermediate `index` values:

        (defmethod mwrite-object ((p point3d) ptr index end-index)
          (mwrite* ptr index end-index (point3d-x p) (point3d-y p) (point3d-z p)))

   Defining `mread-object` specialization is slightly more complicated, for two reasons:
   first, it also needs to instantiate an appropriate object and fill its slots
   and second, `mread` and `mread-object` return multiple values.

   The result is a painstaking nest of `multiple-value-bind`:
   
        (defmethod mread-object ((type (eql 'point3d)) ptr index end-index &key)
          (multiple-value-bind (x index-x) (mread ptr index end-index)
            (multiple-value-bind (y index-y) (mread ptr index-x end-index)
              (multiple-value-bind (z index-z) (mread ptr index-y end-index)
                (values
                  (make-instance 'point3d :x x :y y :z z)
                   index-z)))))
   
   The `with-mread*` macro comes to the rescue, removing all the boilerplate:

        (defmethod mread-object ((type (eql 'point3d)) ptr index end-index &key)
          (with-mread* (x y z new-index) (ptr index end-index)
            (values
              (make-instance 'point3d :x x :y y :z z)
              new-index)))))

- `MWRITE-MAGIC` to be documented...

- `MREAD-MAGIC` to be documented...

- `(HLMEM-VERSION)` is a function that returns the current version of
  the serialization library, implemented by the `HYPERLUMINAL-MEM` package.
  The returned value is a list having the form `'(major minor patch)`
  as for example `'(0 5 0)`

- `(HLDB-VERSION)` is a function that returns the current version of
  the memory-mapped database library, implemented by the `HYPERLUMINAL-DB`
  package. The returned value is a list having the form `'(major minor patch)`
  as for example `'(0 5 0)`


File format and ABI
-------------------
  
By default, Hyperluminal-DB file format and ABI is autodetected to match
Lisp idea of CFFI-SYS pointers:
* 32 bit when CFFI-SYS pointers are 32 bit,
* 64 bit when CFFI-SYS pointers are 64 bit,
* and so on...

In other words, `mem-word` is normally autodetected to match the width
of underlying CPU registers (exposed through CFFI-SYS foreign-type :pointer)
and `+msizeof-word+` is set accordingly.

It is possible to override such autodetection by adding an appropriate entry
in the global variable `*FEATURES*` **before** compiling and loading Hyperluminal-DB.
Doing so disables autodetection and either tells Hyperluminal-DB the desired size
of `mem-word` or, in alternative, the CFFI-SYS type it should use for `mem-word`.

For example, to force 64 bit (= 8 bytes) file format and ABI,
execute the following form before compiling and loading Hyperluminal-DB:

    (pushnew :hyperluminal-db/word-size/8 *features*)

on the other hand, to force 32 bit (= 4 bytes) file format and ABI,
execute the form

    (pushnew :hyperluminal-db/word-size/4 *features*)

in both cases, the Hyperluminal-DB internal function `(choose-word-type)`
will recognize the override and define `mem-word` and `+msizeof-word+`
to match a CFFI-SYS unsigned integer type having the specified size
among the following candidates:
* :unsigned-char
* :unsigned-short
* :unsigned-int
* :unsigned-long
* :unsigned-long-long

In case it does not find a type with the requested size, it will raise an error.

Forcing the same value that would be autodetected is fine and harmless.
Also, the chosen type must be at least 32 bits wide, but there is no upper limit:
Hyperluminal-DB is designed to automatically support 64 bits systems,
128 bit systems, and anything else that will exist in the future.
It even supports 'unusual' configurations where the size of `mem-word`
is not a power of two (ever heard of 36-bit CPUs?).

For the far future (which arrives surprisingly quickly in software)
where CFFI-SYS will know about further unsigned integer types,
it is also possible to explicitly specify the type to use
by executing a form like

    (pushnew :hyperluminal-db/word-type/<SOME-CFFI-SYS-TYPE> *features*)

as for example:

    (pushnew :hyperluminal-db/word-type/unsigned-long-long *features*)

Hyperluminal-DB will honour such override, intern the type name
to convert it to a keyword, use it as the definition of `mem-word`,
and derive `+msizeof-word+` from it.


Status
------
As of February 2014, Hyperluminal-DB is being written by Massimiliano Ghilardi
and it is considered by the author to be in BETA status.

The serialization/deserialization library is usable, tested and documented,
but it may still contain some rough edges and minor bugs.

On the other hand, the memory-mapped database (built on top of the serialization library)
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
