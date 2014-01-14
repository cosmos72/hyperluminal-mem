Hyperluminal-db is a high-performance Common Lisp persistence library.

It is based on transactional memory (provided by STMX library)
and it implements persistence through a memory-mapped Lisp object store.

Design:

1. having a different mmap area for each (primitive) type simplifies tremendously
   type management (i.e. implementing "values know their type")
   but it is a performance killer: objects in store can only contain pointers to values,
   not directly the values.
   Optimizing at least FIXNUMs, CHARs, T and NIL to be stored in-place would greatly help.

2. ABI. assign a small non-negative number to each persistent type.
   Reserve type 0 for "unallocated mmap area", used to keep chained
   lists (in a future version, btrees) of unallocated mmapped areas.

3. ABI. primitive types fitting a CPU register (= CPU word)
   are stored in mmapped area in the following format:

   ----------------------------------------------------------------------------------
   64bit ABI uses 63 bits for both pointers and fixnums.
   pointer offset occupies lowest 48 bits, allowing 256T instances per type.
   pointer tag occupies upper 15 bits, allowing 32768 types.

   unallocated:   0  ; uses tag 0
   unbound:       1  ; uses tag 0; it means the object's slot is unbound
   nil:           2  ; uses tag 0
   t:             3  ; uses tag 0

   characters:    n | #x0001000000000000 ; uses tag 1; only 21 bits actually used, as needed by unicode. 

   single-floats: n | #x0002000000000000 ; uses tag 2
                  tag in the most significant 32 bits
                  then IEEE float value in the least significant 32 bits

   double-floats: not usable by 64-bit ABI, must be stored as boxed values instead

   pointers:      n ; #b0.... ; top bit: always = 0
                              ; next 15 bits: tag, explained in paragraph 4. "pointer tags" 
                              ; lowest 48 bits: offset in units of a CPU word

   fixnums:       n | #x8000000000000000 ; top bit: always = 1 to distinguish from other types
                                         ; next 1 bit: sign. 0 if positive or zero, 1 if negative
                                         ; lowest 62 bits: value in two's complement representation

   ----------------------------------------------------------------------------------
   32bit ABI uses 31 bits for both pointers and fixnums.
   pointer offset occupies lowest 24 bits, allowing 16M elements per type.
   pointer type occupies upper 7 bits, allowing 128 types.

   values using reserved pointer type 0:
   unallocated: #x00000000     ; to exploit sparse files; uses tag 0 
   unbound:     #x00000001     ; means that the object's slot is unbound; uses tag 0
   nil:         #x00000002     ; uses tag 0
   t:           #x00000003     ; uses tag 0

   characters:  n | #x01000000 ; top bit: always = 0
                               ; next 7 bits: tag, always = 1
                               ; next 3 bits: reserved, must be zero
                               ; lowest 21 bits (x): Unicode character code 

   single-floats: not usable by this ABI, must be stored as boxed values instead
   double-floats: not usable by this ABI, must be stored as boxed values instead

   pointers:    n              ; #b0...
                               ; top bit: always = 0
                               ; next 7 bits: tag, explained in paragraph 4. "pointer tags" 
                               ; lowest 24 bits: offset in units of a CPU word

   fixnums:     n | #x80000000 ; #b1...
                               ; top bit: always = 1 to distinguish from other types
                               ; next 1 bit (s): sign. 0 if positive or zero, 1 if negative
                               ; lowest 20 bits (x): value in two's complement representation

4. pointer tags

   0 = symbol, keyword or "unallocated". Includes nil and t.
   1 = inline character
   2 = inline float (cannot be used by 32bit ABI)
   3 = inline double (cannot be used by 32bit and 64bit ABIs)
  
   4..19 = boxed values:
   4 = bignum
   5 = ratio
   6 = single-float
   7 = double-float
   8 = complex of single-float
   9 = complex of double-float
  10 = complex of rationals (fixnums, bignums or ratios)
  11 = pathname
  12 = hash-table with 'eq or 'eql test
  13 = hash-table with 'equal or 'equalp test
  14 = cons or list
  15 = multi-dimensional array
  16 = vector, i.e. 1-dimensional array
  17 = string, i.e. vector of character
  18 = base-string, i.e. vector of base-char
  19 = bit-vector, i.e. vector of bit
  
  20..27 = reserved for future use
  
  28.. user-defined persistent class

5. unallocated areas ABI

   Applies to both unallocated boxed values and to unallocated user-defined persistent types.

   word 0...N-3 : not used, can have any value

   word N-2: tag = always 0, means unallocated area
           value = pointer to next unallocated area
           
   word N-1: tag = always 0, means unallocated area
           value = number of allocated words /4. also counts the footer (i.e. words N-2 and N-1)
   
6. boxed values

   word 0: tag = same coding as pointer tags.
           value = number of allocated words /4. also counts the header (i.e. word 0)
   
   word 1... : payload. depends on boxed type

6.1. boxed bignum

   word 0: as boxed values
   word 1: mem-int = number of words in the bignum (may be less than allocated words)
           if negative, it means the bignum is negative.
   word 2... : array of words, in two's complement representation, of bignum value

6.2. boxed ratio

   word 0: as boxed values
   word 1: numerator, in general representation. must be either a fixnum or a bignum
   word 2: denominator, in general representation. must be either a fixnum or a bignum
   
6.3. boxed single-float (only needed by 32bit ABI)

   word 0: as boxed values
   word 1: single-float value in IEEE format.

6.4. boxed double-float

   word 0: as boxed values
   word 1: double-float value in IEEE format.
   word 2: if needed by ABI, continuation of word 2.

6.5. boxed complex of single-float

   word 0: as boxed values

   32bit ABI:
     word 1: real part. single-float value in IEEE format.
     word 2: imag part. single-float value in IEEE format.

   64bit ABI or larger:
     word 1: / lowest  32 bits: real part. single-float value in IEEE format.
             \ next    32 bits: imag part. single-float value in IEEE format.

6.6. boxed complex of double-float

   word 0: as boxed values

   32bit ABI:
     word 1: real part. double-float value in IEEE format.
     word 2:            continuation of word 2
     word 3: imag part. double-float value in IEEE format.
     word 4:            continuation of word 4

   64bit ABI or larger:
     word 1: real part. double-float value in IEEE format.
     word 2: imag part. double-float value in IEEE format.


6.7. boxed complex of rationals (fixnums, bignums or ratios)

   word 0: as boxed values

   word 1..N: real part. uses general format, i.e. either an inline fixnum or a boxed value
   word N+1..M: imag part. uses general format, i.e. either an inline fixnum or a boxed value

6.8. boxed cons or list

   word 0: as boxed values
   word 2: list length. may be smaller than allocated length. if negative,
           it means "improper list", i.e. last two element form a dotted pair
   word 3... : elements of the list, stored in general format

6.9. boxed vector, i.e. 1-dimensional array

   word 0: as boxed values
   word 1: array length. may be smaller than allocated length
   word 3... : elements of the array, stored in general format

6.10. boxed string, i.e. 1-dimensional array of character

   TODO document this!
   
6.11. boxed base-string, i.e. 1-dimensional array of base-char

   TODO document this!
   
6.12. boxed N-dimensional array

   FIXME obsolete doc!

   word 0: as boxed values
   word 1: as boxed values
           tag is used as element type (see paragraph 7 "array element type")
  
   word 2: array rank (R) i.e. dimensionality
   word 3...R+2 : dimensions
   word R+3... : elements of the array.
                 stored in general format if element type = 0,
                 otherwise may be stored in compact forms specific to the element type

6.13. boxed hash-table

   FIXME obsolete doc!
   
   word 0: as boxed values
   word 1: as boxed values
           tag is used as hash-key comparator: 0 = eq, 1 = eql, 2 = equal, 3 = equalp
  
   word 2: size of hash-table (N). may be smaller than half allocated length
   word 3...2+N*2 : sequence of entries: key1, value1, key2, value2 ... keyN, valueN
                    stored in general format
