Hyperluminal-mem
================

Summary
-------
Hyperluminal-mem is a high-performance serialization/deserialization library
for Common Lisp, designed for untrusted data.

Features
--------
Hyperluminal-mem is designed and optimized for the following objectives:
- speed: serializing and deserializing data can have sustained rates
  exceeding 1GB/s on a single CPU core.
- safety: it can be used on untrusted and possibly malicious data,
  as for example serialized packets or files received from the internet.
- portability: the serialization format is fairly portable.
  It is independent from the Lisp implementation,
  and only depends on user's choice between little or big endian (default is little endian)
  and between 32 and 64 bit formats (default is CPU native width)
  Conversion between little and big endian formats is trivial.
- ease of use: adding support for user-defined types is usually
  straightforward.

### Latest news, 2nd March 2015

Hyperluminal-mem 0.6.1 is included in the newest Quicklisp distribution.
You can now load it with: `(ql:quickload "hyperluminal-mem")`

### News, 24th January 2015

Released version 0.5.2. License change from GPLv3 to LLGPL!

Older versions were bundled together with Hyperluminal-DB in a single GPLv3 package.
Hyperluminal-DB is now a separate project, still under GPLv3.

### Older news
 
See [doc/NEWS.md](doc/news.md)

Supported systems
-----------------
Hyperluminal-mem is currently tested on the following Common Lisp implementations:

* [SBCL](http://sbcl.org/)
  * version 1.2.6        (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.1.15       (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.1.14       (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.2.8        (armhf)    on Debian GNU/Linux wheezy (armhf) inside Qemu
  * version 1.1.15       (powerpc)  on Debian GNU/Linux jessie (powerpc) inside Qemu
  * version 1.2.8        (sparc)    on Debian GNU/Linux wheezy (sparc) inside Qemu
  
* [ABCL](http://www.abcl.org/)
  * version 1.3.1 on OpenJDK 1.7.0_65 (x86_64) on Debian GNU/Linux jessie (x86_64)
  
  Note: on ABCL, memory buffers are implemented using java.nio.ByteBuffer instead of CFFI-SYS
  raw memory pointers due to currently limited compatibility between ABCL and CFFI/OSICAT libraries.
  Memory-mapped files are supported, and internally use `java.nio.channels.FileChannel.map()`
  instead of OSICAT-POSIX `(mmap)`.
  
  Futhermore, hyperluminal-mem test suite fails on ABCL versions up to 1.3.1
  due to a bug in EQUALP implementation. The author contributed a fix to ABCL,
  which should included in the next release.

* [CCL](http://ccl.clozure.com/)
  * version 1.10         (x86_64)   on Debian GNU/Linux jessie (x86_64)
  * version 1.10         (x86)      on Debian GNU/Linux jessie (x86_64)
  * version 1.10         (linuxarm) on Debian GNU/Linux wheezy (armhf) inside Qemu
  * version 1.9-r15761   (linuxppc) on Debian GNU/Linux wheezy (powerpc) inside Qemu

* [CLISP](http://www.clisp.org/)
  * version 2.49         (x86_64)   on Debian GNU/Linux jessie (x86_64)
  
* [CMUCL](http://www.cons.org/cmucl/)
  * version 20d Unicode  (x86)      on Debian GNU/Linux jessie  (x86_64)

### Unsupported systems

* [ECL](http://ecls.sourceforge.net/) versions 13.5.1 and 15.2.1 have some known issues
  with CFFI, OSICAT and STMX, three libraries required by Hyperluminal-mem.
  Once support for these three libraries improves, Hyperluminal-mem can be tested on it too.

### Other systems

Hyperluminal-mem requires CFFI, OSICAT and STMX libraries to work. While reasonably portable,
they exploit features well beyond ANSI Common Lisp and their support for the various Common Lisp
implementations varies widely.

For this reason no general guarantees can be given: Hyperluminal-mem
may or **may not** work on other, untested Common Lisp implementations.

Installation and loading
------------------------

### From [Quicklisp](http://www.quicklisp.org)

Since 2nd March 2015, hyperluminal-mem is available from Quicklisp.
The simplest way to obtain it is to first install
[Quicklisp](http://www.quicklisp.org) then run these commands from REPL:

    CL-USER> (ql:quickload "hyperluminal-mem")
    ;; lots of output...
    CL-USER> (use-package :hlmem)
     
If all goes well, this will load Hyperluminal-mem and its dependencies,
CFFI, OSICAT and STMX.

Since hyperluminal-mem was added to QuickLisp quite recently (2 March 2015),
it may happen that your Quicklisp installation can't find it.
In such case, you need to first update your QuickLisp installation as described
[here](http://www.quicklisp.org/beta) - search for "To get updated software" in the page.


### Latest version - from [GitHub](https://github.com/cosmos72/hyperluminal-mem)

In case you want to use the "latest and greatest" version directly
from the author, in order to get the newest features, improvements, bug fixes,
and occasionally new bugs, you need to download it into your Quicklisp
local-projects folder. Open a shell and run the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/hyperluminal-mem.git

then open a REPL and run:

    CL-USER> (ql:quickload "hyperluminal-mem")
    ;; lots of output...
    CL-USER> (use-package :hlmem)
     
If all goes well, this will load Hyperluminal-mem and its dependencies,
CFFI, OSICAT and STMX.

### Troubleshooting

In case you get errors:

- check that Quicklisp is installed correctly, for example by
  executing at REPL:

        CL-USER> (ql:quickload "osicat")
        CL-USER> (ql:quickload "stmx")

- if you tried to download the stable version from Quicklisp,
  check that your quicklisp is updated and knows about hyperluminal-mem:

        CL-USER> (ql:system-apropos "hyperluminal-mem")
        
  should print something like

        #<SYSTEM hyperluminal-mem / hyperluminal-mem-20150302-git / quicklisp 2015-03-02>
        #<SYSTEM hyperluminal-mem-test / hyperluminal-mem-20150302-git / quicklisp 2015-03-02>
     
  If it doesn't, you need to update Quicklisp as described
  [here](http://www.quicklisp.org/beta) - search for "To get updated
  software" in the page.
  
- if you tried to download the latest version from Github,
  check that you downloaded hyperluminal-mem creating an `hyperluminal-mem/` folder
  inside your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`


### Testing that it works

After loading Hyperluminal-mem for the first time, it is recommended to run
the test suite to check that everything works as expected. From the REPL, run:

    CL-USER> (ql:quickload "hyperluminal-mem-test")
    ;; lots of output...

    CL-USER> (fiveam:run! 'hyperluminal-mem-test:suite)
    ;; even more output...
     Did 3364 checks.
        Pass: 3364 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)
        
Note: `(ql:quickload "hyperluminal-mem-test")` intentionally works only
**after** `(ql:quickload "hyperluminal-mem")` has completed successfuly.

The test suite should report zero Skip and zero Fail; the number of Pass may vary.
You are welcome to report any failure you get while running the test suites,
please include in the report:
- operating system name and version (example: Debian GNU/Linux x86_64 version 7.0)
- Common Lisp implementation and version (example: SBCL 1.0.57.0.debian, x86_64)
- **exact** output produced by the test suite
- any other relevant information

See "Contacts, help, discussion" below for the preferred method to send the report.


Implementation
--------------
Hyperluminal-mem reads and writes serialized data to raw memory,
using CFFI foreign pointers - equivalent to C/C++ pointers.

The most direct way to save serialized data to disk, and to load it back,
is to open a file then map it to memory with the POSIX mmap() system call
provided by OSICAT library.

An alternative, suitable both for files and network sockets,
is to allocate a raw memory buffer with `(hlmem:malloc-words)`
then use the POSIX read() and write() calls provided by OSICAT library.

Basic usage
-----------

Hyperluminal-mem offers the following Lisp types, constants, macros and functions,
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
   raw memory in units of `mem-words` - which means most Hyperluminal-MEM functions.

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
   It is usually more handy than `malloc` since almost all Hyperluminal-mem functions
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

   Other alternatives to obtain raw memory include at least:
   * using the functions `make-static-vector` and `static-vector-pointer`
     from STATIC-VECTORS library (remember to call `free-static-vector` when done)
   * using memory-mapped files, for example with the function `mmap`
     from OSICAT library (remember to call `munmap` when done)

- `(MSIZE index value)` is a function that examines a Lisp value, and tells
   how many words of raw memory are needed to serialize it.

   It is useful to know how large a raw memory block must be
   in order to write a serialized value into it. It is defined as:

        (defun msize (index value)
          (declare (type t value)
                   (type mem-size index))
          (the mem-size (+ index
                           #| ...implementation... |#)))

   The argument INDEX is useful to compute the total size of composite values,
   as for example lists, arrays, hash-tables and objects: the value returned by `msize`
   is increased by the value of `index`, so the following three code snippets are equivalent

        (+ (msize 0 "foo") (msize 0 'bar))

        (let ((index (msize 0 "foo")))
          (msize index 'bar))

        (msize (msize 0 "foo") 'bar)

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
   on memory-mapped files. This is actually the mechanism that allows Hyperluminal-mem
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

   Programmers can extend Hyperluminal-mem by defining specialized methods for it,
   see `MWRITE-OBJECT` for details.

- `(MREAD-OBJECT type ptr index end-index &key)` is a generic function that reads
   a serialized user-defined object from raw memory, deserializes and returns it.

   Programmers can extend Hyperluminal-mem by defining specialized methods for it,
   see `MWRITE-OBJECT` for details.

- `(MWRITE-OBJECT object ptr index end-index)` is a generic function
   that serializes a user-defined Lisp object, writing it into raw memory.

   Programmers can extend Hyperluminal-mem by defining specialized methods for
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
          (let* ((index-x (msize index   (point3d-x p)))
                 (index-y (msize index-x (point3d-y p)))
                 (index-z (msize index-y (point3d-z p))))
             index-z))
   
   note how the result of each `msize` call is passed to the next call - this ensures
   that the sum of the sizes is computed automatically, and also takes care of signalling
   an error in case of overflow.

   A shorter, slightly automagic alternative is to use the macro `msize*`
   which expands to multiple calls of `msize` and correctly passes around the
   intermediate `index` values:

        (defmethod msize-object ((p point3d) index)
          (msize* index (point3d-x p) (point3d-y p) (point3d-z p)))

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
   
   The `with-mread*` macro comes to the rescue, removing most boilerplate code:

        (defmethod mread-object ((type (eql 'point3d)) ptr index end-index &key)
          (with-mread* (x y z new-index) (ptr index end-index)
            (values
              (make-instance 'point3d :x x :y y :z z)
              new-index)))))

   An alternative approach to implement `msize-object`, `mread-object` and
   `mwrite-object` for standard-objects is to take advantage of the generic
   functions `msize-object-slots`, `mread-object-slots` and `mwrite-object-slots`.
   See `MWRITE-OBJECT-SLOTS` for details.

- `MSIZE-OBJECT-SLOTS` is a generic function, useful to implement `msize-object`
   when extending Hyperluminal-mem. See `MWRITE-OBJECT-SLOTS` for details.

- `MREAD-OBJECT-SLOTS` is a generic function, useful to implement `mread-object`
   when extending Hyperluminal-mem. See `MWRITE-OBJECT-SLOTS` for details.

- `MWRITE-OBJECT-SLOTS` is a generic function, useful to implement `mwrite-object`
   when extending Hyperluminal-mem. Details:
  
   The mechanism described in `MWRITE-OBJECT` above is very powerful and general,
   but sometimes all you need is to serialize/deserialize the slots of a standard-object:
   in this case it surely feels overcomplicated.
  
   For such purpose, Hyperluminal-mem provides the functions
   `msize-object-slots`, `mread-object-slots` and `mwrite-object-slots`
   which, given the slot names of an object, call the appropriate functions
   on each slot.
  
   This allows programmers to implement `msize-object`, `mread-object`
   and `mwrite-object` with the following six lines of code:
  
        (defmethod msize-object ((object point3d) index)
          (msize-object-slots object index '(x y z))

        (defmethod mwrite-object ((object point3d) ptr index end-index)
          (mwrite-object-slots object ptr index end-index) '(x y z))

        (defmethod mread-object ((type (eql 'point3d) ptr index end-index &key)
          (mread-object-slots (make-instance 'point3d) ptr index end-index '(x y z)))

   This simplified approach has some limitations:
  
   1. it only works on standard-objects, i.e. on classes defined with (defclass ...)
      (on SBCL it also appears to work on structs defined with (defstruct ...))
  
   2. it can only serialize/deserialize (some or all) the object slots
      with plain `msize`, `mread` and `mwrite`, i.e. it is **not** possible to
      specify a customized logic to serialize/deserialize the slots.
 
   3. it must be possible to construct the object with some initial, dummy
      slot values in order to pass it to `mread-object-slots`.
      This function will then set the actual slot values.
   
   4. the slots to be serialized/deserialized must be listed manually.
  

- `(MREAD-WORD PTR INDEX)` read a single word at ptr+index. Useful for debugging.

- `(MWRITE-WORD PTR INDEX VALUE)` writes a single word at ptr+index. Useful for debugging.

- `MWRITE-MAGIC` to be documented...

- `MREAD-MAGIC` to be documented...

- `(HLMEM-VERSION)` is a function that returns the current version of
  Hyperluminal-mem. The returned value is a list having the form
  `'(major minor patch)` as for example `'(0 5 2)`


Serialization format and ABI
-------------------
  
By default, Hyperluminal-mem serialization format and ABI is autodetected to match
Lisp idea of CFFI-SYS pointers:
* 32 bit when CFFI-SYS pointers are 32 bit,
* 64 bit when CFFI-SYS pointers are 64 bit,
* and so on...

In other words, `mem-word` is normally autodetected to match the width
of underlying CPU registers (exposed through CFFI-SYS foreign-type :pointer)
and `+msizeof-word+` is set accordingly.

It is possible to override such autodetection by adding an appropriate entry
in the global variable `*FEATURES*` **before** compiling and loading Hyperluminal-mem.
Doing so disables autodetection and either tells Hyperluminal-mem the desired size
of `mem-word` or, in alternative, the CFFI-SYS type it should use for `mem-word`.

For example, to force 64 bit (= 8 bytes) file format and ABI,
execute the following form before compiling and loading Hyperluminal-mem:

    (pushnew :hyperluminal-mem/word-size/8 *features*)

on the other hand, to force 32 bit (= 4 bytes) file format and ABI,
execute the form

    (pushnew :hyperluminal-mem/word-size/4 *features*)

in both cases, the Hyperluminal-mem internal function `(choose-word-type)`
will recognize the override and define `mem-word` and `+msizeof-word+`
to match a CFFI-SYS unsigned integer type having the specified size
among the following candidates:
* :unsigned-char
* :unsigned-short
* :unsigned-int
* :unsigned-long
* :unsigned-long-long

In case it does not find a type with the requested size, it will signal an error.

Forcing the same value that would be autodetected is fine and harmless.
Also, the chosen type must be at least 32 bits wide, but there is no upper limit:
Hyperluminal-mem is designed to automatically support 64 bits systems,
128 bit systems, and anything else that will exist in the future.
It even supports 'unusual' configurations where the size of `mem-word`
is not a power of two (ever heard of 36-bit CPUs?).

For the far future (which arrives surprisingly quickly in software)
where CFFI-SYS will know about further unsigned integer types,
it is also possible to explicitly specify the type to use
by executing a form like

    (pushnew :hyperluminal-mem/word-type/<SOME-CFFI-SYS-TYPE> *features*)

as for example:

    (pushnew :hyperluminal-mem/word-type/unsigned-long-long *features*)

Hyperluminal-mem will honour such override, intern the type name
to convert it to a keyword, use it as the definition of `mem-word`,
and derive `+msizeof-word+` from it.


Status
------
As of February 2015, Hyperluminal-mem is being written by Massimiliano Ghilardi
and it is considered by the author to be fairly stable, tested and documented.
It may still contain some rough edges and minor bugs.


Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/hyperluminal-mem/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.


Legal
-----
Hyperluminal-mem is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
