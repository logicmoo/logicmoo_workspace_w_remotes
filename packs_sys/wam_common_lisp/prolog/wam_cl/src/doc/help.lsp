;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;   setdoc.lsp
;;;;
;;;;                    Sets doc-strings for built-in symbols.

(in-package 'compiler)			; in case it does not exist
(in-package 'system)

(defvar *previous* nil)

#-MANUAL
(progn
(setq *stream* (open "help.doc" :direction :output))

(defmacro docfun (symbol kind args doc)
  `(format *stream* "~AF~A~%~A in ~A package:~%~A: ~A~A~%"
    ,(if (eq symbol *previous*) #\  #\)
    ',(setq *previous* symbol)
    ,(cond ((special-form-p symbol) "Special Form")
	   ((functionp symbol) "Function")
	   ((macro-function symbol) "Macro")
	   (t ""))
    ,(package-name (symbol-package symbol))
    ,(if (functionp symbol) "Args" "Syntax") ',args ,doc))

(defmacro docvar (symbol kind doc)
  `(format *stream* "~AV~A~%~@(~A~) in ~A package:~A~%"
    ,(if (eq symbol *previous*) #\  #\)
    ',(setq *previous* symbol)
    ',kind
    ,(package-name (symbol-package symbol)) ,doc))

(defmacro doctype (symbol doc)
  `(format *stream* "~AT~A~A~%"
    ,(if (eq symbol *previous*) #\  #\)
    ',(setq *previous* symbol)
    ,doc))
)
#+MANUAL
(progn
;;; for producing manual:

(setq *stream* (open "summary.tex" :direction :output))

(defun true-package (symbol)
  (let ((pkg (symbol-package symbol)))
    (unless (eq pkg (find-package 'LISP)) (package-name pkg))))

(defmacro docfun (symbol kind args doc)
  (if (functionp symbol)
      `(let ((*print-pretty* nil))
	 (format *stream* "\\defunx{~(~A~)}~@[[~(~A~)]~]{"
		 ',symbol
		 ,(true-package symbol))
	 (dolist
	  (arg ',args)
	  (case arg
		(&KEY (princ "\\&key" *stream*))
		(&REST (princ "\\&rest" *stream*))
		(&OPTIONAL (princ "\\&optional" *stream*))
		(T
		 (if (listp arg)
		     (progn
		       (princ #\( *stream*)
		       (dolist (el arg)
			       (if (and (symbolp el) (not (boundp el)))
				   (format *stream* "\\var{~(~A~)} " el)
				 (if (and (consp el)
					  (eq 'QUOTE (car el)))
				     (format *stream* "~(~A~) "
					     (second el))
				   (format *stream* "~(~A~) " el))))
		       (princ #\) *stream*))
		   (format *stream* "\\var{~(~A~)}" arg))))
	  (princ #\space *stream*))
	 (format *stream* "}~%\\enddefun~%"))
    `(let ((*print-pretty* nil)
	   (kind ,(cond ((special-form-p symbol) "spec")
			((macro-function symbol) "mac")
			(t "un"))))
       (format *stream* "\\def~Ax{~(~A~)}~@[[~(~A~)]~]{~A}~%\\enddef~A~%"
	       kind
	       ',symbol
	       ,(true-package symbol)
	       ',args
	       kind))))

(defmacro docvar (symbol kind doc)
  `(format *stream* "\\defvarx{~(~A~)}~@[[~(~A~)]~]~%\\enddefvar~%"
    ',symbol
    ,(true-package symbol)))

(defmacro doctype (symbol doc)
  `(format *stream* "\\deftypex{~(~A~)}~%\\enddeftype~%"
    ',symbol))
)
;;;----------------------------------------------------------------------
;;;	Ordered alphabetically for binary search
;;;----------------------------------------------------------------------

(docfun * function (&rest numbers) "
Returns the product of the args.  With no args, returns 1.")

(docvar * variable "
The value of the last top-level form.")

(docvar ** variable "
The value of the last-but-one top-level form.")

(docvar *** variable "
The value of the last-but-two top-level form.")

(docvar *applyhook* variable "
EVAL normally uses APPLY to call functions, but if the value of this variable
is non-NIL, EVAL uses that value instead.  The value of this variable must be
a function that receives a function, an arguments list, and an environment.")

(docvar *break-enable* variable "
ECL specific.
When an error is signaled, control enters a break loop only if the value of
this variable is non-NIL.  The initial value is T, but ECL automatically
rebinds this variable to NIL when control enters a break loop.")

(docvar *break-on-warnings* variable "
When the function WARN is called, control enters to a break loop only if the
value of this variable is non-NIL.")

(docvar compiler::*cc* variable "
This variable controls how the C compiler is invoked by ECL.
The default value is \"cc -I. -I/usr/local/include/\".
The second -I option names the directory where the file ECL.h has been installed.
One can set the variable appropriately adding for instance flags which the 
C compiler may need to exploit special hardware features (e.g. a floating point
coprocessor).")

(docvar compiler::*compile-print* variable "
This variable controls whether the compiler displays messages about
each form it processes. The default value is NIL.")

(docvar compiler::*compile-verbose* variable "
This variable controls whether the compiler should display messages about its
progress. The default value is T.")

(docvar sys:*gc-verbose* variable "
ECL specific.
If the value of SYS:*GC-VERBOSE* is non-NIL, the garbage collector prints some
information on the terminal.  Usually SYS:*GC-VERBOSE* is set T.")

(docvar *debug-io* variable "
The stream used by the ECL debugger.  The initial value is a synonym stream to
*TERMINAL-IO*.")

(docvar *default-pathname-defaults* variable "
The default pathname used by some pathname-handling functions such as ENOUGH-
NAMESTRING.")

(docvar *error-output* variable "
The output stream to which error messages are output.  The initial value is an
synonym stream to *TERMINAL-IO*.")

(docvar *eval-when-compile* variable "
ECL specific.
The ECL compiler evaluates all top-level forms if the value of this variable
is non-NIL.  If the value is NIL, the compiler evaluates as few top-level
forms as necessary.  Note the compiler always evaluates DEFMACRO forms.")

(docvar *evalhook* variable "
Specifies a function to be used instead of EVAL.  If the value of this
variable is non-NIL, it must be a function that can receive two arguments: a
form to evaluate and an environment.  This function does the evaluation
instead of EVAL.")

(docvar *features* variable "
List of symbols that name features of the current version of ECL.  These
features are used in connection with the read macros #+ and #-.  When the
reader encounters
	#+ feature-spec form
it reads FORM in the usual manner if FEATURE-SPEC is satisfied.  Otherwise,
the reader just skips FORM.
	#- feature-spec form
is equivalent to
	#- (not feature-spec) form
A feature-spec may be a symbol, in which case the spec is satisfied iff the
symbol is an element of *FEATURES*.  Or else, a feature-spec must be one of
the following forms.
	(and {feature-spec}*)
		Satisfied iff all FEATURE-SPECs are satisfied
	(or {feature-spec}*)
		Satisfied iff at least one of FEATURE-SPECs is satisfied
	(not feature-spec)
		Satisfied iff FEATURE-SPEC is not satisfied")

(docvar sys:*ignore-eof-on-terminal-io* variable "
ECL specific.
If the value of this variable is non-NIL, ECL ignores the EOF-character
(usually ^D) on the terminal.  The initial value is NIL.")

(docvar *ignore-maximum-pages* variable "
ECL specific.
Tells the ECL memory manager whether (non-NIL) or not (NIL) it should expand
memory when the maximum allocatable pages have been used up.  The initial
value is T.")

(docvar sys:*indent-formatted-output* variable "
ECL specific.
The FORMAT directive ~~% indents the next line, if the value of this variable
is non-NIL.  If NIL, ~~% simply does Newline.")

(docvar sys:*interrupt-enable* variable "
ECL specific.
If the value of SYS:*INTERRUPT-ENABLE* is non-NIL, ECL signals an error on the
terminal interrupt (this is the default case).  If it is NIL, ECL ignores the
interrupt and assigns T to SYS:*INTERRUPT-ENABLE*.")

(docvar sys:*lisp-init-file-list* variable "
List of files automatically loaded when ECL is invoked.")

(docvar sys:*lisp-maxpages* variable "
ECL specific.
The current maximum number of pages (1 page = 2048 bytes) for the ECL process.
The result of changing the value of SYS:*LISP-MAXPAGES* is unpredictable.")

(docvar *load-verbose* variable "
The default value for the :VERBOSE parameter of LOAD.
It initial value is T.")

(docvar *macroexpand-hook* variable "
The value of this variable must be a three-argument function object.
Each time a macro form is expanded, ECL calls that function with
	1. the macro expansion function (see MACRO-FUNCTION)
	2. the macro form to expand
	3. an environment (NIL in most case)
as three arguments, and uses the returned value as the expanded form.
The initial value of this variable is the function FUNCALL.")

(docfun sys:*make-constant function (symbol value) "
ECL specific.
Declares that the global variable named by SYMBOL is a constant with VALUE as
its constant value.")

(docfun sys:*make-special function (symbol) ")
ECL specific.
Declares the variable named by NAME as a special variable.")

(docvar *modules* variable "
List of module names that have been loaded into ECL.
See PROVIDE and REQUIRE.")

(docvar sys:*gc-verbose* variable "
ECL specific.
If the value of this variable is non-NIL, then the garbage collector notifies
that it begins to run whenever it is invoked.  Otherwise, garbage collection
begins silently.")

(docvar *package* variable "
The current package.  The initial value is the USER package.")

(docvar *print-array* variable "
Specifies whether ECL should print elements when it prints arrays other than
strings.  ECL uses the following abbreviation notations.
	#<bit-vector n>		for bit-vectors
	#<vector n>		for vectors other than strings and bit-vectors
	#<array n>		for arrays other than vectors
where N is a number that identifies the array.")

(docvar *print-base* variable "
The radix used to print integers and ratios.  The value must be an integer
from 2 to 36, inclusive.  The initial value is 10.")

(docvar *print-case* variable "
Specifies how to print ordinary symbols.  Possible values are:
	:UPCASE		in upper case
	:DOWNCASE	in lower case
	:CAPITALIZE	the first character in upper case, the rest in lower
The initial value is :UPCASE.")

(docvar *print-circle* variable "
Specifies whether the ECL printer should take care of circular lists.")

(docvar *print-escape* variable "
Specifies whether the ECL printer should output objects in the way that they
can be reread later if possible.")

(docvar *print-gensym* variable "
Specifies whether the ECL printer should prefix uninterned symbols with \"#:\".")

(docvar *print-length* variable "
Specifies how many elements the ECL printer should print when it prints a
list.  ECL printer prints all elements if the value of this variable is NIL.")

(docvar *print-level* variable "
Specifies how many levels of depth the ECL printer should print when it prints
a list.  ECL printer prints all levels if the value of this variable is NIL.")

(docvar *print-pretty* variable "
Specifies whether the ECL printer should pretty-print.  See PPRINT for more
information about pretty-printing.")

(docvar *print-radix* variable "
Specifies whether the ECL printer should print the radix when it prints
integers and ratios.")

(docvar *query-io* variable "
The query I/O stream.  The initial value is a synonym stream to *TERMINAL-IO*.")

(docvar *random-state* variable "
The default random-state object used by RANDOM.")

(docvar *read-base* variable "
The radix used to read numbers.  The initial value is 10.")

(docvar *read-default-float-format* variable "
The default float format the ECL reader uses when reading floats.  Must be one
of the symbols SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, and LONG-FLOAT.")

(docvar *read-suppress* variable "
When the value of this variable is non-NIL, the ECL reader parses input
characters without most of the ordinary processings such as interning.  Used
to skip over forms.")

(docvar *readtable* variable "
The current readtable.")

(docvar *standard-input* variable "
The default input stream used by the ECL reader.  The initial value is a
synonym stream to *TERMINAL-IO*.")

(docvar *standard-output* variable "
The default output stream used by the ECL printer.  The initial value is a
synonym stream to *TERMINAL-IO*.")

(docvar compiler::*suppress-compiler-notes* variable "
This variable controls whether the compiler displays compilation notices.
The default value is NIL.")

(docvar compiler::*suppress-compiler-warnings* variable "
This variable controls whether the compiler should issue warnings.
The default value is NIL.")

(docvar sys:*system-directory* variable "
ECL specific.
The name of the ECL system directory.")

(docvar *terminal-io* variable "
The terminal I/O stream.")

(docvar *trace-output* variable "
The stream used for trace output.  The initial value is a synonym stream to
*TERMINAL-IO*.")

(docfun + function (&rest numbers) "
Returns the sum of the args.  With no args, returns 0.")

(docvar + variable "
The last top-level form.")

(docvar ++ variable "
The last-but-one top-level form.")

(docvar +++ variable "
The last-but-two top-level form.")

(docfun - function (number &rest more-numbers) "
Returns the first arg subtracted by the rest of args.  With one arg, returns
- NUMBER.")

(docvar - variable "
The top-level form ECL is currently evaluating.")

(docfun / function (number &rest more-numbers) "
Returns the first arg divided by the rest of args.  With one arg, returns
1/NUMBER.")

(docvar / variable "
The list of all values of the last top-level form.")

(docvar // variable "
The list of all values of the last-but-one top-level form.")

(docvar /// variable "
The list of all values of the last-but-two top-level form.")

(docfun /= function (number &rest more-numbers) "
Returns T if no two of the args are numerically equal; NIL otherwise.")

(docfun 1+ function (number) "
Returns NUMBER plus one.")

(docfun 1- function (number) "
Returns NUMBER minus one.")

(docfun < function (number &rest more-numbers) "
Returns T if the args are in increasing order; NIL otherwise.")

(docfun <= function (number &rest more-numbers) "
Returns T if the args are in non-decreasing order; NIL otherwise.")

(docfun = function (number &rest more-numbers) "
Returns T if all args are numerically equal; NIL otherwise.")

(docfun > function (number &rest more-numbers) "
Returns T if the args are in decreasing order; NIL otherwise.")

(docfun >= function (number &rest more-numbers) "
Returns T if the args are in non-increasing order; NIL otherwise.")

(docfun abs function (number) "
Returns the absolute value of the arg.")

(docfun acons function (key datum alist) "
Equivalent to (CONS (CONS KEY DATUM) ALIST).")

(docfun acos function (number) "
Returns the arc cosine of NUMBER.")

(docfun acosh function (number) "
Returns the hyperbolic arc cosine of NUMBER.")

(docfun adjoin function (item list &key (key '#'identity) (test '#'eql) test-not) "
Returns cons of ITEM and LIST unless ITEM is already an element of LIST.
Otherwise, returns LIST.")

(docfun adjust-array function (array dimensions
       &key (element-type (array-element-type array))
            initial-element (initial-contents nil) (fill-pointer nil)
            (displaced-to nil) (displaced-index-offset 0)) "
Adjusts the dimensions of ARRAY to the given DIMENSIONS.  ARRAY must be an
adjustable array.")

(docfun adjustable-array-p function (array) "
Returns T if ARRAY is adjustable; NIL otherwise.")

(docfun allocate function (type number &optional (really-allocate nil)) "
ECL specific.
Sets the maximum number of pages for the type class of the ECL implementation
type TYPE to NUMBER.  If REALLY-ALLOCATE is non-NIL, then the specified number
of pages will be allocated immediately.")

(docfun sys:allocate-contiguous-pages function (number &optional (really-allocate nil)) "
ECL specific.
Sets the maximum number of pages for contiguous blocks to NUMBER.  If REALLY-
ALLOCATE is non-NIL, then the specified number of pages will be allocated
immediately.")

#+clos
(docfun sys:allocate-gfun function (name arity hash-table) "
ECL/CLOS specific.
Allocates a gfun object in which NAME is the generic function name, ARITY
is the number of arguments and HASH-TABLE is the hashtable for cashing
methods.")

#+clos
(docfun sys:allocate-instance function (class length) "
ECL/CLOS specific.
Allocates an istance of CLASS with LENGTH slots.")

(docfun sys:allocate-relocatable-pages function (number) "
ECL specific.
Sets the maximum number of pages for relocatable blocks to NUMBER.")

(docfun sys:allocated-contiguous-pages function () "
ECL specific.
Returns the number of pages currently allocated for contiguous blocks.")

(docfun sys:allocated-pages function (type) "
ECL specific.
Returns the number of pages currently allocated for the type class of the ECL
implementation type TYPE.")

(docfun sys:allocated-relocatable-pages function () "
ECL specific.
Returns the number of pages currently allocated for relocatable blocks.")

(docfun alpha-char-p function (char) "
Returns T if CHAR is alphabetic; NIL otherwise.")

(docfun alphanumericp function (char) "
Returns T if CHAR is either numeric or alphabetic; NIL otherwise.")

(docfun and macro "(and {form}*)" "
Evaluates FORMs in order.  If any FORM evaluates to NIL, returns immediately
with the value NIL.  Otherwise, returns all values of the last FORM.")

(docfun append function (&rest lists) "
Constructs and returns a new list by concatenating the args.")

(docfun apply function (function arg &rest more-args) "
Calls FUNCTION with all ARGs except the last and all elements of the last ARG
as the arguments to FUNCTION.  Returns all values that FUNCTION returns.")

(docfun applyhook function (function list evalhookfn applyhookfn &optional (env nil)) "
Calls FUNCTION with all elements of LIST as the arguments and with *EVALHOOK*
and *APPLYHOOK* bound to EVALHOOKFN and APPLYHOOKFN respectively.  Returns all
values that FUNCTION returns.")

(docfun apropos function (string &optional (package nil)) "
Prints those symbols whose print-names contain STRING as substring.  If
PACKAGE is non-NIL, then only the specified PACKAGE is searched.")

(docfun apropos-list function (string &optional (package nil)) "
Returns a list of all symbols whose print-names contain STRING as substring.
If PACKAGE is non-NIL, then only the specified PACKAGE is searched.")

(docfun aref function (array &rest indexes) "
Returns the element of ARRAY specified by INDEXES.")

(docfun sys:argc function () "
ECL specific.
Returns the number of arguments given in the command line that invoked ECL.")

(docfun arglist function (symbol) "
Returns the lambda-list of function named by SYMBOL.")

(docfun sys:argv function (n) "
ECL specific.
Returns the N-th argument given in the command line that invoked ECL.")

(doctype array "
An array is a compound object whose elements are referenced by indexing.  One-
dimensional arrays are called vectors.  Other arrays are notated as
	#?a( ... )	or	#?A( ... )
where '?' is actually the rank of the array.
Arrays may be displaced to another array, may have a fill-pointer, or may be
adjustable.  Other arrays are called simple-arrays.  Only simple-arrays can be
input in the above format.")

(docfun array-dimension function (array n) "
Returns the length of the N-th dimension of ARRAY.")

(docvar array-dimension-limit constant "
The upper bound of the length of an array dimension.")

(docfun array-dimensions function (array) "
Returns a list whose N-th element is the length of the N-th dimension of ARRAY.")

(docfun array-element-type function (array) "
Returns the element type ARRAY.")

(docfun array-has-fill-pointer-p function (array) "
Returns T if ARRAY has a fill-pointer; NIL otherwise.")

(docfun array-in-bounds-p function (array &rest indexes) "
Returns T if INDEXes are valid indexes of ARRAY; NIL otherwise.  The number of
INDEXes must be equal to the rank of ARRAY.")

(docfun array-rank function (array) "
Returns the rank of ARRAY.")

(docvar array-rank-limit constant "
The upper bound of the rank of an array.")

(docfun array-row-major-index function (array &rest indexes) "
Returns the non-negative integer that represents the location of the element
of ARRAY specified by INDEXes, assuming all elements of ARRAY are aligned in
row-major order.")

(docfun array-total-size function (array) "
Returns the total number of elements of ARRAY.")

(docvar array-total-size-limit constant "
The upper bound of the total number of elements of an array.")

(docfun arrayp function (x) "
Returns T if X is an array; NIL otherwise.")

(docfun ash function (integer count) "
Returns the integer obtained by shifting the bits that represent INTEGER as
specified by COUNT.  Shifts left in COUNT bits if COUNT is positive.  Shifts
right in -COUNT bits if COUNT is negative.")

(docfun asin function (number) "
Returns the arc sine of NUMBER.")

(docfun asinh function (number) "
Returns the hyperbolic arc sine of NUMBER.")

(docfun assert macro "(assert form [({place}*) [string {arg}*]])" "
Evaluates FORM and signals a continuable error if the value is NIL.  Before
continuing, receives new values of PLACEs from user.  Repeats this process
until FORM returns a non-NIL value.  Returns NIL.  STRING is the format string
for the error message and ARGs are arguments to the format string.")

(docfun assoc function (item alist &key (test '#'eql) test-not (key '#'identity)) "
Returns the first pair in ALIST whose car is equal (in the sense of TEST) to
ITEM.  Returns NIL if no such pair exists.
The function KEY is applied to extract the key for comparison.")

(docfun assoc-if function (test alist) "
Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no
such pair exists.")

(docfun assoc-if-not function (test alist) "
Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL
if no such pair exists.")

(docfun atan function (x &optional (y 1)) "
Returns the arc tangent of X/Y.")

(docfun atanh function (number) "
Returns the hyperbolic arc tangent of NUMBER.")

(doctype atom "
An atom is an object that is not a cons.")

(docfun atom function (x) "
Returns T if X is not a cons; NIL otherwise.")

(docfun sys:bds-val function (n) "
ECL specific.
Returns the value of the N-th entity in the bind stack.")

(docfun sys:bds-var function (n) "
ECL specific.
Returns the symbol of the N-th entity in the bind stack.")

(doctype bignum "
A bignum is an integer that is not a fixnum.")

(doctype bit "
A bit is either integer 0 or 1.")

(docfun bit function (bit-array &rest indexes) "
Returns the bit of BIT-ARRAY specified by INDEXes.")

(docfun bit-and function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise AND of BIT-ARRAY1 and BIT-ARRAY2.  Puts the results
into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T, or into
RESULT if RESULT is a bit-array.")

(docfun bit-andc1 function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise AND of {the element-wise NOT of BIT-ARRAY1} and BIT-
ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(docfun bit-andc2 function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise AND of BIT-ARRAY1 and {the element-wise NOT of BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(docfun bit-eqv function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise EQUIVALENCE of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array.")

(docfun bit-ior function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array.")

(docfun bit-nand function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise NOT of {the element-wise AND of BIT-ARRAY1 and BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(docfun bit-nor function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise NOT of {the element-wise INCLUSIVE OR of BIT-ARRAY1
and BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(docfun bit-not function (bit-array &optional (result nil)) "
Returns the element-wise NOT of BIT-ARRAY.  Puts the results into a new bit-
array if RESULT is NIL, into BIT-ARRAY if RESULT is T, or into RESULT if
RESULT is a bit-array.")

(docfun bit-orc1 function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise INCLUSIVE OR of {the element-wise NOT of BIT-ARRAY1}
and BIT-ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(docfun bit-orc2 function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and {the element-wise NOT
of BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array.")

(doctype bit-vector "
A bit-vector is a vector of bits.  A bit-vector is notated by '#*' followed
by its elements (0 or 1).  Bit-vectors may be displaced to another array, may
have a fill-pointer, or may be adjustable.  Other bit-vectors are called
simple-bit-vectors.  Only simple-bit-vectors can be input in the above format
using '#*'.")

(docfun bit-vector-p function (x) "
Returns T if X is a bit-vector; NIL otherwise.")

(docfun bit-xor function (bit-array1 bit-array2 &optional (result nil)) "
Returns the element-wise EXCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array.")

(docfun block special "(block name {form}*)" "
Establishes a block named by NAME, evaluates FORMs in order, and returns all
values of the last FORM.  Returns NIL if no FORMs are given.
The scope of the established block is the body (i.e. the FORMs) of the BLOCK
form.  If (return-from name value-form) is evaluated within the scope, the
execution of the BLOCK form terminates immediately and all values of
VALUE-FORM will be returned as the values of the terminated BLOCK form.")

(docfun boole function (op integer1 integer2) "
Returns the integer produced by the logical operation specified by OP on the
two integers.  OP must be the value of one of the following constants.
	BOOLE-CLR	BOOLE-C1	BOOLE-XOR	BOOLE-ANDC1
	BOOLE-SET	BOOLE-C2	BOOLE-EQV	BOOLE-ANDC2
	BOOLE-1		BOOLE-AND	BOOLE-NAND	BOOLE-ORC1
	BOOLE-2		BOOLE-IOR	BOOLE-NOR	BOOLE-ORC2
Each logical operation on integers produces an integer represented by the bit
sequence obtained by a bit-wise logical operation on the bit sequences that
represent the integers.  Two's-complement representation is assumed to obtain
the bit sequence that represents an integer.  For example,
	 2:  ...010
	 1:  ...001
	 0:  ...000
	-1:  ...111
	-2:  ...110
where each '...' represents either an infinite sequence of 0's (for non-
negative integers) or an infinite sequence of 1's (for negative integers).")

(docvar boole-1 constant "
Makes BOOLE return INTEGER1.")

(docvar boole-2 constant "
Makes BOOLE return INTEGER2.")

(docvar boole-and constant "
Makes BOOLE return the AND of INTEGER1 and INTEGER2.")

(docvar boole-andc1 constant "
Makes BOOLE return the AND of {the NOT of INTEGER1} and INTEGER2.")

(docvar boole-andc2 constant "
Makes BOOLE return the AND of INTEGER1 and {the NOT of INTEGER2}.")

(docvar boole-c1 constant "
Makes BOOLE return the NOT of INTEGER1.")

(docvar boole-c2 constant "
Makes BOOLE return the NOT of INTEGER2.")

(docvar boole-clr constant "
Makes BOOLE return 0.")

(docvar boole-eqv constant "
Makes BOOLE return the EQUIVALENCE of INTEGER1 and INTEGER2.")

(docvar boole-ior constant "
Makes BOOLE return the INCLUSIVE OR of INTEGER1 and INTEGER2.")

(docvar boole-nand constant "
Makes BOOLE return the NOT of {the AND of INTEGER1 and INTEGER2}.")

(docvar boole-nor constant "
Makes BOOLE return the NOT of {the INCLUSIVE OR of INTEGER1 and INTEGER2}.")

(docvar boole-orc1 constant "
Makes BOOLE return the INCLUSIVE OR of {the NOT of INTEGER1} and INTEGER2.")

(docvar boole-orc2 constant "
Makes BOOLE return the INCLUSIVE OR of INTEGER1 and {the NOT of INTEGER2}.")

(docvar boole-set constant "
Makes BOOLE return -1.")

(docvar boole-xor constant "
Makes BOOLE return the EXCLUSIVE OR of INTEGER1 and INTEGER2.")

(docfun both-case-p function (char) "
Returns T if CHAR is an alphabetic character; NIL otherwise.  Equivalent to
ALPHA-CHAR-P.")

(docfun boundp function (symbol) "
Returns T if the global variable named SYMBOL has a value; NIL otherwise.")

(docfun break function (&optional (format-string nil) &rest args) "
Enters a break loop.  The execution of the program can be resumed by typing
:RESUME at the break loop.  Type :HELP to see the break-loop commands list.
If FORMAT-STRING is non-NIL, it is used as the format string to be output to
*ERROR-OUTPUT* before entering the break loop.  ARGs are arguments to the
format string.")

(docfun butlast function (list &optional (n 1)) "
Returns a copy of LIST with the last N elements removed.")

(docfun quit function () "
ECL specific.
Exits from ECL.")

(docfun byte function (size position) "
Returns a byte specifier of integers.  The value specifies the SIZE-bits byte
starting the least-significant-bit but POSITION bits of integers.  In ECL, a
byte specifier is represented by a dotted pair (SIZE . POSITION).")

(docfun byte-position function (byte) "
Returns the position part (in ECL, the cdr part) of the byte specifier BYTE.")

(docfun byte-size function (byte) "
Returns the size part (in ECL, the car part) of the byte specifier BYTE.")

(docfun caaaar function (x) "
Equivalent to (CAR (CAR (CAR (CAR X)))).")

(docfun caaadr function (x) "
Equivalent to (CAR (CAR (CAR (CDR X)))).")

(docfun caaar function (x) "
Equivalent to (CAR (CAR (CAR X))).")

(docfun caadar function (x) "
Equivalent to (CAR (CAR (CDR (CAR X)))).")

(docfun caaddr function (x) "
Equivalent to (CAR (CAR (CDR (CDR X)))).")

(docfun caadr function (x) "
Equivalent to (CAR (CAR (CDR X))).")

(docfun caar function (x) "
Equivalent to (CAR (CAR X)).")

(docfun cadaar function (x) "
Equivalent to (CAR (CDR (CAR (CAR X)))).")

(docfun cadadr function (x) "
Equivalent to (CAR (CDR (CAR (CDR X)))).")

(docfun cadar function (x) "
Equivalent to (CAR (CDR (CAR X))).")

(docfun caddar function (x) "
Equivalent to (CAR (CDR (CDR (CAR X)))).")

(docfun cadddr function (x) "
Equivalent to (CAR (CDR (CDR (CDR X)))).")

(docfun caddr function (x) "
Equivalent to (CAR (CDR (CDR X))).")

(docfun cadr function (x) "
Equivalent to (CAR (CDR X)).")

(docvar call-arguments-limit constant "
The upper bound of the number of arguments to a function.  Ignore this value
since there is no such logical upper bound in ECL.")

(docfun car function (x) "
Returns the car of X if X is a cons.  Returns NIL if X is NIL.")

(docfun case macro "(case keyform {({key | ({key}*)} {form}*)}*)" "
Evaluates KEYFORM and searches a KEY that is EQL to the value of KEYFORM.  If
found, then evaluates FORMs in order that follow the KEY (or the key list that
contains the KEY) and returns all values of the last FORM.  Returns NIL if no
such key is found.  The symbols T and OTHERWISE may be used at the place of a
key list to specify the default case.")

(docfun catch special "(catch tag-form {form}*)" "
Sets up a catcher whose catch tag is the value of TAG-FORM.  Then evaluates
FORMs in order and returns all values of the last FORM.  During the evaluation
of FORMs, if a THROW form is evaluated that specifies a catch tag EQ to the
value of the TAG-FORM, then the execution of the CATCH form terminates
immediately and the values specified by the THROW form are returned as the
value of the CATCH form.")

          ;; 5549.3
          5390

#+unix
(docfun sys:catch-bad-signals function () "
ECL/UNIX specific.
Installs a signal catcher for bad signals:
	SIGILL, SIGIOT, SIGEMT, SIGBUS, SIGSEGV, SIGSYS.
The signal catcher, upon catching the signal, signals an error.  Since the
internal memory of ECL may have been broken, the user is recommended to exit
from ECL after checking the signal.  When the signal is caught during garbage
collection, ECL terminates immediately.")

(docfun ccase macro "(ccase place {({key | ({key}*)} {form}*)}*)" "
Searches a KEY that is EQL to the value of PLACE.  If found, then evaluates
FORMs in order that follow the KEY (or the key list that contains the KEY) and
returns all values of the last FORM.  If no such KEY is found, signals a
continuable error.  Before continuing, receives a new value of PLACE from
user and searches a KEY again.  Repeats this process until the value of PLACE
becomes EQL to one of the KEYs.")

(docfun cdaaar function (x) "
Equivalent to (CDR (CAR (CAR (CAR X)))).")

(docfun cdaadr function (x) "
Equivalent to (CDR (CAR (CAR (CDR X)))).")

(docfun cdaar function (x) "
Equivalent to (CDR (CAR (CAR X))).")

(docfun cdadar function (x) "
Equivalent to (CDR (CAR (CDR (CAR X)))).")

(docfun cdaddr function (x) "
Equivalent to (CDR (CAR (CDR (CDR X)))).")

(docfun cdadr function (x) "
Equivalent to (CDR (CAR (CDR X))).")

(docfun cdar function (x) "
Equivalent to (CDR (CAR X)).")

(docfun cddaar function (x) "
Equivalent to (CDR (CDR (CAR (CAR X)))).")

(docfun cddadr function (x) "
Equivalent to (CDR (CDR (CAR (CDR X)))).")

(docfun cddar function (x) "
Equivalent to (CDR (CDR (CAR X))).")

(docfun cdddar function (x) "
Equivalent to (CDR (CDR (CDR (CAR X)))).")

(docfun cddddr function (x) "
Equivalent to (CDR (CDR (CDR (CDR X)))).")

(docfun cdddr function (x) "
Equivalent to (CDR (CDR (CDR X))).")

(docfun cddr function (x) "
Equivalent to (CDR (CDR X)).")

(docfun cdr function (x) "
Returns the cdr of X if X is a cons.  Returns NIL if X is NIL.")

(docfun ceiling function (number &optional (divisor 1)) "
Returns the smallest integer not less than NUMBER/DIVISOR.  Returns the value
of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun cerror function (continue-format-string error-format-string &rest args) "
Signals a continuable error.")

#+clos
(docfun sys:change-instance function (instance class length) "
ECL/CLOS specific.
Updates INSTANCE making it an instance of CLASS with LENGTH slots. These
have a null value.")

(docfun char function (string index) "
Returns the INDEX-th character in STRING.")

(docfun char-bit function (char name) "
Returns T if the specified bit attribute of CHAR is 'on'; NIL otherwise.
In ECL the bit-attributes handled are :control :meta :super and :hyper")

(docfun char-bits function (char) "
Returns the bit attributes of CHAR as an integer. In ECL it returns a value
between 0 and 16, since ECL handle 4 bit attributes.")

(docvar char-bits-limit constant "
The upper bound of values returned by CHAR-BITS.  16 in ECL.")

(docfun char-code function (char) "
Returns the character code of CHAR as a fixnum.")

(docvar char-code-limit constant "
The upper bound of values returned by CHAR-CODE.")

(docvar char-control-bit constant "
The bit position indicating a control character.  1 in ECL.")

(docfun char-downcase function (char) "
Returns the lower-case character corresponding to CHAR, if CHAR is upper-case.
Otherwise, returns CHAR.")

(docfun char-equal function (char &rest more-chars) "
Returns T if all CHARs are the same; NIL otherwise.  Lower-case characters are
regarded the same as the corresponding upper-case characters.")

(docfun char-font function (char) "
Returns the font attribute of CHAR.  Returns always 0 in ECL, since ECL
characters have no font attributes.")

(docvar char-font-limit constant "
The upper bound of values returned by CHAR-FONT.  1 in ECL.")

(docfun char-greaterp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in decreasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-hyper-bit constant "
The bit position indicating a hyper character.  8 in ECL.")

(docfun char-int function (char) "
Returns the font, bits, and code attributes as an integer.  Equivalent to
CHAR-CODE in ECL.")

(docfun char-lessp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in increasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-meta-bit constant "
The bit position indicating a meta character.  2 in ECL.")

(docfun char-name function (char) "
Returns the 'character name' of CHAR as a string; NIL if CHAR has no character
name.  Only #\\Backspace, #\\Tab, #\\Newline (or #\\Linefeed), #\\Page,
#\\Return, and #\\Rubout have character names in ECL.")

(docfun char-not-equal function (char &rest more-chars) "
Returns T if no two of CHARs are the same; NIL otherwise.  Lower-case
characters are regarded the same as the corresponding upper-case characters.")

(docfun char-not-greaterp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-decreasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docfun char-not-lessp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-increasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-super-bit constant "
The bit position indicating a super character.  4 in ECL.")

(docfun char-upcase function (char) "
Returns the upper-case character of CHAR, if CHAR is lower-case.  Otherwise,
returns CHAR.")

(docfun char/= function (char &rest more-chars) "
Returns T if no two of CHARs are the same; NIL otherwise.")

(docfun char< function (char &rest more-chars) "
Returns T if the character codes of CHARs are in increasing order; NIL
otherwise.")

(docfun char<= function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-decreasing order; NIL
otherwise.")

(docfun char= function (char &rest more-chars) "
Returns T if all CHARs are the same; NIL otherwise.")

(docfun char> function (char &rest more-chars) "
Returns T if the character codes of CHARs are in decreasing order; NIL
otherwise.")

(docfun char>= function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-increasing order; NIL
otherwise.")

(doctype character "
A character represents a character that can be handled by the computer.
Characters have font, bits, and code attributes.  Font and bits attributes
are always 0 in ECL.  Most versions of ECL uses ASCII code:
  000 - 037	#\\^@  #\\^A  #^B ... #\\Z  #\\^[  #\\^\\  #\\^]  #\\^^  #\\^_
		except #\\Tab(011)     #\\Newline(012)     #\\Page(014)
		       #\\Return(015)  #\\Backspace(031)
  040 - 057	#\\Space  #\\!  #\\\"  #\\#  #\\$  #\\%  #\\&  #\\'  #\\(  #\\)  #\\*
		#\\+  #\\,  #\\-  #\\.  #\\/
  060 - 071	#\\0  #\\1  #\\2  #\\3  #\\4  #\\5  #\\6  #\\7  #\\8  #\\9
  072 - 100	#\\:  #\\;  #\\<  #\\=  #\\>  #\\?  #\\@
  101 - 132	#\\A ... #\\Z
  133 - 140	#\\[  #\\\\  #\\]  #\\^  #\\_  #\\`
  141 - 172	#\\a ... #\\z
  173 - 177	#\\{  #\\|  #\\}  #\\~~  #\\Rubout
Some versions of ECL support additional characters to represent Japanese
character set.")

(docfun character function (x) "
Coerces X into a character if possible.  Signals an error if not possible.")

(docfun characterp function (x) "
Returns T if X is a character; NIL otherwise.")

#+unix
(docfun sys:chdir function (filespec) "
ECL/UNIX specific.
Changes the current working directory to the one specified by FILESPEC.
FILESPEC may be a symbol, a string, or a pathname.")

(docfun check-type macro "(check-type place typespec [string-form])" "
Signals a continuable error, if the value of PLACE is not of the specified
type.  Before continuing, receives a new value of PLACE from the user and
checks the type again.  Repeats this process until the value of PLACE becomes
of the specified type.  STRING-FORM, if given, is evaluated only once and the
value is used to indicate the expected type in the error message.")

(docfun cis function (radians) "
Returns a complex number whose realpart and imagpart are the values of (COS
RADIANS) and (SIN RADIANS) respectively.")

(docfun clear-input function (&optional (stream *standard-input*)) "
Clears the input buffer of STREAM and returns NIL.  Contents of the buffer are
discarded.")

(docfun clear-output function (&optional (stream *standard-output*)) "
Clears the output buffer of STREAM and returns NIL.  Contents of the buffer
are discarded.")

(docfun clines macro "(clines {string}*)" "
ECL specific.
The ECL compiler embeds STRINGs into the intermediate C language code.  The
interpreter ignores this form.")

(docfun close function (stream &key (abort nil)) "
Closes STREAM.  Returns NIL if STREAM is closed successfully; non-NIL
otherwise.  A non-NIL value of ABORT indicates an abnormal termination but ECL
ignores it.")

(docfun clrhash function (hash-table) "
Removes all entries of HASH-TABLE and returns HASH-TABLE.")

(docfun code-char function (code &optional (bits 0) (font 0)) "
Returns a character with the specified character code, if any.  Returns NIL
if no such character exists.  BITS and FONT specify the bits and font
attributes of the returned character but are both ignored in ECL.")

(docfun coerce function (x type) "
Coerces X to an object of the specified type, if possible.  Signals an error
if not possible.")

(doctype common "
COMMON is the type of all Common Lisp data objects.")

(docfun commonp function (x) "
Returns T if X is a Common Lisp object; NIL otherwise.")

(docfun compile function (name &optional (definition nil)) "
If DEFINITION is NIL, NAME must be the name of a not-yet-compiled function.
In this case, COMPILE compiles the function, installs the compiled function as
the global function definition of NAME, and returns NAME.  If DEFINITION is
non-NIL, it must be a lambda expression and NAME must be a symbol.  COMPILE
compiles the lambda expression, installs the compiled function as the function
definition of NAME, and returns NAME.  There is only one exception for this:
If NAME is NIL, then the compiled function is not installed but is simply
returned as the value of COMPILE.  In any case, COMPILE creates temporary
files, whose filenames begin with \"gazonk\", which are automatically deleted
after compilation.")

(docfun compile-file function (input-pathname
       &key output-file (load nil)
            (o-file t) (c-file nil) (h-file nil) (data-file nil)) "
Compiles the file specified by INPUT-PATHNAME and generates a fasl file
specified by OUTPUT-FILE.  If the filetype is not specified in INPUT-PATHNAME,
then \".lsp\" is used as the default file type for the source file.
LOAD specifies whether to load the generated fasl file after compilation.
The :O-FILE, :C-FILE, :H-FILE, and :DATA-FILE keyword
parameters allow you to control the intermediate files generated by the
ECL compiler.
If the file was compiled successfully, returns the pathname of the compiled
file")

(doctype compiled-function "
A compiled function is an object that is created by compiling a function.  A
compiled function is notated in either of the following formats:
	#<compiled-function s>
	#<compiled-closure nil>
where S is actually the symbol that names the function.")

(docfun sys:compiled-function-name function (compiled-function) "
ECL specific.
Returns the function name associated with COMPILED-FUNCTION.")

(docfun compiled-function-p function (x) "
Returns T if X is a compiled function object; NIL otherwise.")

(docfun compiler-let special
	"(compiler-let ({var | (var [value])}*) {form}*)" "
When interpreted, this form works just like a LET form with all VARs declared
special.  When compiled, FORMs are processed with the VARs bound at compile
time, but no bindings occur when the compiled code is executed.")

(doctype complex "
A complex number represents a complex number in mathematical sense, consisting
of a real part and an imaginary part.  A complex number is notated as
	#c( realpart  imagpart )  or  #C( realpart  imagpart )
where REALPART and IMAGPART are non-complex numbers.")

(docfun complex function (realpart &optional (imagpart 0)) "
Returns a complex number with the given realpart and imagpart.  Returns
REALPART if it is a rational and IMAGPART is 0.")

(docfun complexp function (x) "
Returns T if X is a complex number; NIL otherwise.")

(docfun concatenate function (type &rest sequences) "
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs.")

(docfun cond macro "(cond {(test {form}*)}*)" "
Evaluates TESTs in order until one evaluates to non-NIL.  Then evaluates FORMs
in order that follow the TEST and returns all values of the last FORM.  If no
forms follow the TEST, then returns the value of the TEST.  Returns NIL, if no
TESTs evaluates to non-NIL.")

(docfun conjugate function (number) "
Returns the complex conjugate of NUMBER.  Returns NUMBER if it is not a
complex number.")

(doctype cons "
A cons is a compound object consisting of two components car and cdr.")

(docfun cons function (x y) "
Returns a new cons whose car and cdr are X and Y respectively.")

(docfun consp function (x) "
Returns T if X is a cons; NIL otherwise.")

(docfun constantp function (x) "
Returns T if ECL is sure that X, when given as a form, always evaluates to a
same value.  Returns NIL otherwise.  Typically used to check whether a symbol
names a constant variable.")

(docfun copy-alist function (alist) "
Returns a new list consisting of copies of all pairs in ALIST.")

(docfun copy-list function (list) "
Returns a new list consisting of all elements in LIST.")

(docfun copy-readtable function (&optional (readtable *readtable*) (to-readtable nil)) "
Returns a new copy of READTABLE.  If TO-READTABLE is non-NIL, then copies the
contents of READTABLE into TO-READTABLE and returns TO-READTABLE.")

(docfun copy-seq function (sequence) "
Returns a new copy of SEQUENCE.")

(docfun copy-symbol function (symbol &optional (flag nil)) "
Returns a new uninterned symbol with the same print name as SYMBOL.  If FLAG
is NIL, the symbol property of the new symbol is empty.  Otherwise, the new
symbol gets a copy of the property list of SYMBOL.")

(docfun copy-tree function (tree) "
Returns a copy of TREE.  Defined as:
	(defun copy-tree (tree)
	  (if (atom tree)
	      tree
	      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))")

(docfun cos function (radians) "
Returns the cosine of RADIANS.")

(docfun cosh function (number) "
Returns the hyperbolic cosine of NUMBER.")

(docfun count function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE satisfying TEST with ITEM as the
first argument.")

(docfun count-if function (test sequence
       &key (key '#'identity)
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE satisfying TEST.")

(docfun count-if-not function (test sequence
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE not satisfying TEST.")

(docfun ctypecase macro "(ctypecase place {(type {form}*)}*)" "
Searches a TYPE to which the value of PLACE belongs.  If found, then evaluates
FORMs that follow the TYPE and returns all values of the last FORM.  If no
such TYPE is found, signals a continuable error.  Before continuing, receives
a new value of PLACE from the user and searches an appropriate TYPE again.
Repeats this process until the value of PLACE becomes of one of the TYPEs.")

(docfun sys::daylight-saving-time-p function () "
ECL specific.
Returns T if Daylight Saving Time applies to the local time zone.")

(docfun decf macro "(decf place [form])" "
Decrements the value of PLACE by the value of FORM.  FORM defaults to 1.")

(docfun declare special "(declare {decl-spec}*)" "
Gives declarations.  Possible DECL-SPECs are:
  (SPECIAL {var}*)
  (TYPE type {var}*)
  (type {var}*) where 'type' is one of the following symbols
	array		fixnum		package		simple-string
	atom		float		pathname	simple-vector
	bignum		function	random-state	single-float
	bit		hash-table	ratio		standard-char
	bit-vector	integer		rational	stream
	character	keyword		readtable	string
	common		list		sequence	string-char
	compiled-function  long-float	short-float	symbol
	complex		nil		signed-byte	t
	cons		null		simple-array	unsigned-byte
	double-float	number		simple-bit-vector  vector
  (OBJECT {var}*)
  (FTYPE type {function-name}*)
  (FUNCTION function-name ({arg-type}*) {return-type}*)
  (INLINE {function-name}*)
  (NOTINLINE {function-name}*)
  (IGNORE {var}*)
  (OPTIMIZE {({SPEED | SPACE | SAFETY | COMPILATION-SPEED} {0 | 1 | 2 | 3})}*)
  (DECLARATION {non-standard-decl-name}*)
  (:READ-ONLY {variable-name}*).")

(docfun decode-float function (float) "
Returns the significand F, the exponent E, and the sign S of FLOAT.  These
values satisfy
	1/B <= F < 1
and			 E
	FLOAT = S * F * B
where B is the radix used to represent FLOAT.  S and F are floats of the same
float format as FLOAT, and E is an integer.")

(docfun decode-universal-time function (integer &optional (timezone (sys:get-local-time-zone))) "
Returns as nine values the day-and-time represented by INTEGER.  See GET-
DECODED-TIME.")

(docfun defcbody macro "(defcbody symbol ({arg-type}*) value-type body)" "
ECL specific.
The compiler defines a Lisp function named by SYMBOL whose body consists of the
C code of the string BODY. In the BODY one can reference the arguments of the
function as \"#0\", \"#1\", etc.
The interpreter ignores this form.  ARG-TYPEs are argument types of the
defined Lisp function and VALUE-TYPE is its the return type.")

(docfun defcfun macro "(defcfun header n {element}*)" "
ECL specific.
Defines a C-language function which calls Lisp functions and/or handles Lisp
objects.  HEADER gives the header of the C function as a string.  N is the
number of the main stack entries used by the C function, primarily for
protecting Lisp objects from being garbage-collected.  Each ELEMENT may give
a C code fragment as a string, or it may be a list
	((symbol {arg}*) {place}*)
which, when executed, calls the Lisp function named by SYMBOL with the
specified arguments and saves all values to the specified places.  The DEFCFUN
form has the above meanings only after compiled;  The ECL interpreter simply
ignores this form.")

(docfun defconstant macro "(defconstant symbol form [doc])" "
Declares that the global variable named by SYMBOL is a constant with the value
of FORM as its constant value.  The doc-string DOC, if supplied, is saved as a
VARIABLE doc and can be retrieved by (DOCUMENTATION 'SYMBOL 'VARIABLE).")

(docfun defentry macro "(defentry symbol ({arg-type}*) (value-type function-name))" "
ECL specific.
The compiler defines a Lisp function named by SYMBOL whose body consists of a
calling sequence to the C language function named by FUNCTION-NAME.  The
interpreter ignores this form.  ARG-TYPEs are argument types of the C function
and VALUE-TYPE is the return type of the C function.  Symbols OBJECT, INT,
CHAR, CHAR*, FLOAT, DOUBLE are allowed for these types.")

(docfun define-modify-macro macro "(define-modify-macro symbol lambda-list function-name [doc])" "
Defines a read-modify-write macro like INCF.  The defined macro will expand
a form (SYMBOL place form1 ... formn) into a form that in effect SETFs the
value of (FUNCTION-NAME place arg1 ... argm) into PLACE, where ARG1 ... ARGm
are parameters in LAMBDA-LIST which are bound to FORM1 ... FORMn.  For
example, INCF could be defined as
	(define-modify-macro incf (&optional (x 1)) +)
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (DOCUMENTATION 'SYMBOL 'FUNCTION).")

(docfun define-setf-method macro
	"(define-setf-method symbol defmacro-lambda-list {decl | doc}*
          {form}*)" "
Defines the SETF-method for generalized-variables (SYMBOL ...).
When a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs
given in the DEFINE-SETF-METHOD are evaluated in order with the parameters in
DEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five
values
	(var1 ... vark)
	(form1 ... formk)
	(value-var)
	storing-form
	access-form
in order.  These values are collectively called the five gangs of the
generalized variable (SYMBOL arg1 ... argn).  The whole SETF form is then
expanded into
	(let* ((var1 from1) ... (vark formk)
	       (value-var value-form))
	  storing-form)
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (DOCUMENTATION 'SYMBOL 'SETF).")

(docfun definline macro "(definline symbol ({arg-type}*) value-type body)" "
ECL specific.
DEFINLINE behaves like a DEFCBODY (see), but also instructs the LISP compiler
to expand inline any call to function SYMBOL into code corresponding
to the C language expression BODY, whenever it can determine that
the actual arguments are of the specified type.")

(docfun defla macro "(defla name lambda-list {decl | doc}* {form}*)" "
ECL specific.
Used to DEFine Lisp Alternative.  For the interpreter, DEFLA is equivalent to
DEFUN, but the compiler ignores this form.")

(docfun defmacro macro "(defmacro name defmacro-lambda-list {decl | doc}* {form}*)" "
Defines a global macro named by NAME.  The complete syntax of DEFMACRO-LAMBDA-
LIST is:
	( [&whole var] [&environment var] . pvar )
where PVAR may be a symbol,
	( {pvar}* [&optional {var | (pvar [init [pvar]])}*] . var )
or
	( {pvar}*
	  [&optional {var | (pvar [init [pvar]])}*]
	  [{&rest | &body} pvar]
	  [&key {var | ({var | (keyword pvar)} [init [pvar]])}*
	        [&allow-other-keys]]
	  [&aux {var | (pvar [init])}*] )
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).  See LIST for the backquote
macro useful for defining macros.")

(docfun defparameter macro "(defparameter name form [doc])" "
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable).")

(docfun defsetf macro "(defsetf symbol update-fun [doc] )" "
	or
	(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)
Defines an expansion
	(setf (SYMBOL arg1 ... argn) value)
	=> (UPDATE-FUN arg1 ... argn value)
	   or
	   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)
where REST is the value of the last FORM with parameters in LAMBDA-LIST bound
to the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (documentation 'SYMBOL 'setf).")

(docfun defstruct macro
	"(defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )" "
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure).")

(docfun deftype macro "(deftype name lambda-list {decl | doc}* {form}*)" "
Defines a new type-specifier abbreviation in terms of an 'expansion' function
	(lambda lambda-list1 {DECL}* {FORM}*)
where LAMBDA-LIST1 is identical to LAMBDA-LIST except that all optional
parameters with no default value specified in LAMBDA-LIST defaults to the
symbol '*', but not to NIL.  When the type system of ECL encounters a type
specifier (NAME arg1 ... argn), it calls the expansion function with the
arguments ARG1 ... ARGn, and uses the returned value instead of the original
type specifier.  When the symbol NAME is used as a type specifier, the
expansion function is called with no argument.
The doc-string DOC, if supplied, is saved as a TYPE doc and can be retrieved
by (documentation 'NAME 'type).")

(docfun defun macro "(defun name lambda-list {decl | doc}* {form}*)" "
Defines a global function named by NAME.
The complete syntax of a lambda-list is:
	({var}*
	 [&optional {var | (var [init [svar]])}*]
	 [&rest var]
	 [&key {var | ({var | (keyword var)} [init [svar]])}*
	       [&allow-other-keys]]
	 [&aux {var | (var [init])}*])
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).")

(docfun defunc macro "(defunc symbol lambda-list {string}*)" "
ECL specific.
The compiler defines a Lisp function named by SYMBOL whose body consists of the
C code from STRINGs. LAMBDA-LIST is similar to the lambda list of DEFUN, except
that default values for optional, keyword or auxiliary variables are discarded.
Lowercase symbols from LAMBDA-LIST can be used to refer to the arguments of
the function within STRINGs. Therefore symbols should not contain characters
which are considered as delimiters in C (-, +, *, etc.).
Results are returned by storing them in vs_base[0], ..., vs_base[n] and setting
vs_top=vs_base+n.
The interpreter ignores this form.")

(docfun defvar macro "(defvar name [form [doc]])" "
Declares the variable named by NAME as a special variable.  If the variable
does not have a value, then evaluates FORM and assigns the value to the
variable.  FORM defaults to NIL.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable).")

(docfun delete function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE.  SEQUENCE may be destroyed.")

(docfun delete-duplicates function (sequence &key (key '#'identity) (test '#'eql) test-not
                     (start 0) (end (length sequence)) (from-end nil)) "
Destructive REMOVE-DUPLICATES.  SEQUENCE may be destroyed.")

(docfun delete-file function (filespec) "
Deletes the specified file.  FILESPEC may be a symbol, a string, a pathname,
or a file stream.")

(docfun delete-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE-IF.  SEQUENCE may be destroyed")

(docfun delete-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE-IF-NOT.  SEQUENCE may be destroyed")

(docfun denominator function (rational) "
Returns the denominator of RATIONAL as a positive integer, if RATIONAL is a
ratio.  Returns RATIONAL if it is an integer.")

(docfun deposit-field function (integer1 bytespec integer2) "
Returns an integer represented by the bit sequence obtained by replacing the
specified bits of INTEGER2 with the specified bits of INTEGER1.")

(docfun describe function (x) "
Prints information about X to the standard output.")

(docfun digit-char function (digit &optional (n 10) (font 0)) "
Returns a character that represents the DIGIT in radix N.  Returns NIL if no
such character exists.")

(docfun digit-char-p function (char &optional (n 10)) "
If CHAR represents a digit in radix N, then returns an integer represented by
that digit.  Otherwise, returns NIL.")

(docfun directory function (filespec) "
Returns a list of full pathnames of all those files that match FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun directory-namestring function (filespec) "
Returns as a string the directory part of the pathname specified by FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun disassemble function (&optional (thing nil) &key (h-file nil) (data-file nil)) "
Compiles the form specified by THING and prints the intermediate C language
code for that form.  But does not install the result of compilation.  If THING
is NIL, then the previously DISASSEMBLEd form is re-DISASSEMBLEd.  If THING is
a symbol that names a function not yet compiled, the function definition is
disassembled.  If THING is a lambda expression, it is disassembled as a
function definition.  Otherwise, THING itself is disassembled as a top-level
form.
H-FILE and DATA-FILE specify intermediate files to build a fasl file from the
C language code.  NIL means \"do not create the file\".")

(docfun sys:displaced-array-p function (array) ")
ECL specific.
Returns T if the ARRAY is displaced to another array; NIL otherwise.")

(docfun do macro
	"(do ({(var [init [step]])}*) (test {result}*)
          {decl}* {tag | statement}*)" "
Establishes a NIL block, binds each VAR to the value of the corresponding INIT
(which defaults to NIL), and then executes STATEMENTs repeatedly until TEST is
satisfied.  After each iteration, evaluates STEP and assigns the value to the
corresponding VAR.  No assignment occurs for those VARs to which STEP is not
given.  When TEST is satisfied, evaluates RESULTs as a PROGN and returns all
values of the last RESULT.  Performs variable bindings and assignments in
parallel, just as LET and PSETQ do.")

(docfun do* macro
	"(do* ({(var [init [step]])}*) (test {result}*)
          {decl}* {tag | statement}*)" "
Similar to DO, but performs variable bindings and assignments in serial, just
as LET* and SETQ do.")

(docfun do-all-symbols macro
	"(do-all-symbols (var [result]) {decl}* {tag | statement}*)" "
Establishes a NIL block and executes STATEMENTs once for each symbol in each
package, with VAR bound to the symbol.  Then evaluates RESULT (which defaults
to NIL) and returns all values.")

(docfun do-external-symbols macro
	"(do-external-symbols (var [package [result]])
          {decl}* {tag | statement}*)" "
Establishes a NIL block and executes STATEMENTs once for each external symbol
in PACKAGE (which defaults to the current package), with VAR bound to the
variable.  Then evaluates RESULT (which defaults to NIL) and returns all
values.")

(docfun do-symbols macro
	"(do-symbols (var [package [result]])
          {decl}* {tag | statement}*)" "
Executes STATEMENTs once for each symbol in PACKAGE (which defaults to the
current package), with VAR bound to the symbol.  Then evaluates RESULT (which
defaults to NIL) and returns all values.")

(docfun documentation function (symbol doc-type) "
Returns the DOC-TYPE doc-string of SYMBOL; NIL if none exists.  Possible doc-
types are:
	FUNCTION  (special forms, macros, and functions)
	VARIABLE  (global variables)
	TYPE      (type specifiers)
	STRUCTURE (structures)
	SETF      (SETF methods)
All built-in special forms, macros, functions, and variables have their doc-
strings.")

(docfun dolist macro
	"(dolist (var form [result])
          {decl}* {tag | statement}*)" "
Establishes a NIL block and executes STATEMENTs once for each member of the
list value of FORM, with VAR bound to the member.  Then evaluates RESULT
(which defaults to NIL) and returns all values.")

(doctype double-float "
A double-float is a double-precision floating point number.
DOUBLE-FLOAT as a type specifier is equivalent to LONG-FLOAT in ECL.")

(docfun dotimes macro
	"(dotimes (var form [result])
          {decl}* {tag | statement}*)" "
Establishes a NIL block and executes STATEMENTs once for each integer between
0 (inclusive) and the value of FORM (exclusive), with VAR bound to the
integer.  Then evaluates RESULT (which defaults to NIL) and returns all
values.")

(docvar double-float-epsilon constant "
Same as LONG-FLOAT-EPSILON.")

(docvar double-float-negative-epsilon constant "
Same as LONG-FLOAT-NEGATIVE-EPSILON.")

(docfun dpb function (newbyte bytespec integer) "
Replaces the specified byte of INTEGER with NEWBYTE (an integer) and returns
the result.")

(docfun dribble function (&optional filespec) "
If FILESPEC is given, starts recording the interaction to the specified file.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
is not given, ends the recording.")

(docfun ecase macro
	"(ecase keyform {({key | ({key}*)} {form}*)}*)" "
Evaluates KEYFORM and tries to find the KEY that is EQL to the value of
KEYFORM.  If found, then evaluates FORMs that follow the KEY (or the key list
that contains the KEY) and returns all values of the last FORM.  If not,
signals an error.")

(docfun ed function (&optional x) "
Invokes the editor.  The action depends on the version of ECL.  See the ECL
Report for details.")

(docfun eighth function (x) "
Equivalent to (CADDDR (CDDDDR X)).")

(docfun elt function (sequence n) "
Returns the N-th element of SEQUENCE.")

(docfun encode-universal-time function (second minute hour date month year
       &optional (timezone (sys:get-local-time-zone))) "
Returns an integer that represents the given day-and-time.  See GET-DECODE-
TIME.")

(docfun endp function (x) "
Returns T if X is NIL.  Returns NIL if X is a cons.  Otherwise, signals an
error.")

(docfun enough-namestring function (filespec &optional (defaults *default-pathname-defaults*)) "
Returns a string which uniquely identifies the file specified by FILESPEC,
with respect to DEFAULTS.  FILESPEC and DEFAULTS may be a symbol, a string, a
pathname, or a file stream.")

(docfun eq function (x y) "
Returns T if the args are identical; NIL otherwise.")

(docfun eql function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. identical
	2. are numbers of the same type with the same value
	3. are characters that represent the same character
Returns NIL otherwise.")

(docfun equal function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. EQL
	2. are conses with EQUAL cars and EQUAL cdrs
	3. are strings of the same length and element-wise EQL
	4. are bit-vectors of the same length and element-wise EQL
	5. are pathnames with EQUAL slots
Returns NIL otherwise.")

(docfun equalp function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. EQUAL
	2. are characters that satisfy CHARACTER-EQUAL
	3. are numbers that satisfy =
	4. are conses with EQUALP cars and EQUALP cdrs
	5. are arrays of the same dimensions and element-wise EQUALP
Returns NIL otherwise.")

(docfun error function (format-string &rest args) "
Signals an error.  The args are FORMATed to *error-output*.")

(docfun sys:error-set function (form) "
ECL specific.
Evaluates the FORM in the null environment.  If the evaluation is successfully
completed, SYS:ERROR-SET returns NIL as the first value and the results of the
evaluation as the rest of the values.  If, in the course of the evaluation, a
non-local jump from the FORM is attempted, SYS:ERROR-SET traps the jump and
returns the corresponding jump tag as its value.")

(docfun etypecase macro
	"(etypecase keyform {(type {form}*)}*)" "
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, signals an error.")

(docfun eval function (form) "
Evaluates FORM and returns all values.")

(docfun eval-when special "(eval-when ({situation}*) {form}*)" "
Specifies when to evaluate FORMs.  Each SITUATION must be one of the following
symbols.
	COMPILE	(compile-time)
	LOAD	(load-time of the fasl file)
	EVAL	(load-time of the source file)")

(docfun evalhook function (form fun1 fun2 &optional (env nil)) "
Evaluates FORM with *EVALHOOK* bound to FUN1 and *APPLYHOOK* bound to FUN2,
and returns all the values.")

(docfun evenp function (integer) "
Returns T if INTEGER is an even number; NIL otherwise.")

(docfun every function (predicate sequence &rest more-sequences) "
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise.")

(docfun exp function (number) "
Returns E raised to the power NUMBER, where E is the base of natural
logarithms.")

(docfun export function (symbol &optional (package *package*)) "
Register SYMBOL as an external symbol of PACKAGE.  SYMBOL may be a list of
symbols.")

(docfun expt function (number1 number2) "
Returns NUMBER1 raised to the power NUMBER2.")

(docfun sys:faslink function (filespec string) "
ECL specific.
Loads the specified fasl file while linking the object files and libraries as
specified by STRING.  For example,
	(faslink \"foo.o\" \"bar.o boo.o -lpixrect\")
loads foo.o while linking two object files (bar.o and boo.o) and the library
pixrect.  Usually, foo.o consists of the C language interface for the
functions defined in the object files or the libraries.  FILESPEC may be a
symbol, a string, a pathname, or a file stream.")

(docfun fboundp function (symbol) "
Returns T if SYMBOL names a special form, a global macro, or a global
function.  Returns NIL otherwise.")

(docfun fceiling function (number &optional (divisor 1)) "
Same as CEILING, but returns a float as the first value.")

(docfun ffloor function (number &optional (divisor 1)) "
Same as FLOOR, but returns a float as the first value.")

(docfun fifth function (x) "
Equivalent to (CAR (CDDDDR X)).")

(docfun file-author function (filespec) "
Returns the author of the specified file, as a string.  Returns NIL if the
author is unknown.  FILESPEC may be a symbol, a string, a pathname, or a file
stream.")

(docfun file-length function (file-stream) "
Returns the length of the specified FILE-STREAM.  Returns NIL if the length is
unknown.")

(docfun file-namestring function (filespec) "
Returns as a string the name, type, and version parts of the specified
pathname.  FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun file-position function (file-stream &optional file-position) "
With one arg, returns the current position of FILE-STREAM's file pointer as a
non-negative integer.  Returns NIL if the position is unknown.  With two args,
resets the file pointer and returns T.  Returns NIL if the file pointer cannot
be reset.  FILE-POSITION may be a non-negative integer, :START, or :END.")

(docfun file-write-date function (filespec) "
Returns an integer that represents the last write day-and-time of the
specified file (See GET-DECODED-TIME).   Returns NIL if the last write day-
and-time is unknown.  FILESPEC may be a symbol, a string, a pathname, or a
file stream.")

(docfun fill function (sequence item &key (start 0) (end (length sequence))) "
Replaces the specified elements of SEQUENCE with ITEM.")

(docfun fill-pointer function (vector) "
Returns the fill-pointer of VECTOR as an integer.  VECTOR must have a fill-
pointer.")

(docfun find function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the first element in SEQUENCE satisfying TEST with ITEM.  Returns NIL
if no such element exists.")

(docfun find-all-symbols function (string) "
Returns a list of all symbols that have the specified print name.  STRING may
be a symbol, in which case the print name of the symbol is used.")

(docfun find-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index of the first element in SEQUENCE that satisfies TEST.
Returns NIL if no such element exists.")

(docfun find-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index of the first element in SEQUENCE that does not satisfy TEST.
Returns NIL if no such element exists.")

(docfun find-package function (name) "
Returns the package whose package name or nickname is NAME.  Returns NIL if no
such package exists.  NAME may be a string or a symbol.")

(docfun find-symbol function (string &optional (package *package*)) "
Searches PACKAGE for a symbol whose print name is NAME.  If such a symbol is
found, then returns the symbol as the first value and returns one of the
following symbols as the second value.
	:INTERNAL (internal symbol in PACKAGE)
	:EXTERNAL (external symbol in PACKAGE)
	:INHERITED (external symbol of a package that PACKAGE is using)
If no such symbol is found, returns NIL as the first and second values.")

(docfun finish-output function (&optional (stream *standard-output*)) "
Sends the contents of the output buffer for STREAM to the destination.  Waits
until the buffer becomes empty and then returns NIL.")

(docfun first function (x) "
Equivalent to CAR.")

(docfun sys:fixnump function (x) "
ECL specific.
Returns T if the X is a fixnum; NIL otherwise.")

(doctype fixnum "
A fixnum is an integer between MOST-NEGATIVE-FIXNUM (= - 2^29 in ECL) and
MOST-POSITIVE-FIXNUM (= 2^29 - 1 in ECL) inclusive.  Other integers are
bignums.")

(docfun flet special
	"(flet ({(name lambda-list {decl | doc}* {form}*)}*) . body)" "
Introduces local functions and evaluates BODY as a PROGN.  BODY is the scope
of each local function but the local function definitions are not.  Thus each
local function can reference externally defined functions of the same name as
local functions.  Doc-strings for local functions are simply ignored.")

(doctype float "
A float (floating-point number) represents a real number or its approximation.
ECL supports two formats for floats.  One format is called SHORT-FLOAT and the
other format is called SINGLE-FLOAT, DOUBLE-FLOAT, or LONG-FLOAT.  Precisions
and exponent sizes of floats depends on the version of ECL.  See the ECL
Report at your hand for details.
The following syntax is used to notate a float.
	[+ | -] {digit}* . {digit}+ [exp]
	[+ | -] {digit}+ [. {digit}*}] exp
where DIGIT is a decimal digit (0,..,9) and EXP is
	marker [+ | -] {digit}+
with one of the following marker.
	e or E	the default float format
	s or S	short-float
	f or F	single-float
	d or D	double-float
	l or L	long-float
The default float format is single-float normally, but may be any other float
format.  See *READ-DEFAULT-FLOAT-FORMAT*.")

(docfun float function (number &optional float) "
With one arg, converts NUMBER to a single-float.  With two args, converts
NUMBER to a float of the same float format as FLOAT.")

(docfun float-digits function (float) "
Returns the number of radix-B digits used to represent the significand of
FLOAT, where B is the base number used in the representation of FLOAT.")

(docfun float-precision function (float) "
Returns the number of effective radix-B digits in the representation of the
significand of FLOAT, where B is the base number used in the representation
of FLOAT.")

(docfun float-radix function (float) "
Returns the base number used in the representation of FLOAT.")

(docfun float-sign function (float1 &optional (float2 (float 1 float1))) "
Returns a float with the same sign as FLOAT1 and with the same absolute value
as FLOAT2.")

(docfun floatp function (x) "
Returns T if X is a float; NIL otherwise.")

(docfun floor function (number &optional (divisor 1)) "
Returns the largest integer not larger than the NUMBER divided by DIVISOR.
Returns the value of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun fmakunbound function (symbol) "
Removes the global function definition associated with SYMBOL.  Returns SYMBOL.")

(docfun force-output function (&optional (stream *standard-output*)) "
Sends the contents of the output buffer for STREAM to the destination.
Returns NIL without waiting until the buffer becomes empty.")

(docfun format function (destination format-string &rest args) "
Outputs ARGs to DESTINATION in the format specified by FORMAT-STRING.  FORMAT-
STRING is a string consisting of characters to output and format directives
which begin with '~~'.  Outputs to DESTINATION if it is a stream and to the
standard output if DESTINATION is T.  If DESTINATION is NIL, does not output
actually but returns the output as a string.  Here are the format directives:
	~~A	PRINCs one arg
	~~S	PRIN1s one arg
	~~D	Prints one integer in decimal
	~~B	Prints one integer in binary
	~~O	Prints one integer in octal
	~~X	Prints one integer in hexa
	~~nR	Prints arg in radix n
	~~P	Prints 's' if args is not 1
	~~C	Prints one character
	~~F	Prints a floating point number in fixed-format
	~~E	Prints a floating point number in exponential notation
	~~G	Prints a floating point number in either ~~F or ~~E notation
	~~$	Prints a floating point number in dollars notation
	~~%	Does TERPRI
	~~&	Does FRESH-LINE
	~~|	Outputs #\\Page
	~~~~	Outputs '~~'
	~~<newline>	ignores the <newline> and any following whitespace
	~~T	Spaces over to a given column
	~~n*	Ignores the next n arguments
	~~?	Indirection: the next string is used as a format directive
	~~(str~~)	Case conversion
	~~[str0~~;str1~~;...~~;strn~~]	Conditional expression
	~~;	separates clauses in ~~[ and ~~< constructions
	~~]	terminates a ~~[
	~~{str~~}	Iteration
	~~}	terminates a ~~{
	~mincol,colinc,minpad,padchar<str~~>	Justification")

(docfun fourth function (x) "
Equivalent to CADDDR.")

(docfun fresh-line function (&optional (stream *standard-output*)) "
Outputs a newline character only if the current position of STREAM is not at
the beginning of a line.  Returns T if it outputs a newline; NIL otherwise.")

(docfun fround function (number &optional (divisor 1)) "
Same as ROUND, but returns a float as the first value.")

(docfun sys:frs-bds function (n) "
ECL specific.
Returns the bind stack index of the N-th entity in the frame stack.")

(docfun sys:frs-ihs function (n) "
ECL specific.
Returns the invocation history stack index of the N-th entity in the frame
stack.")

(docfun ftruncate function (number &optional (divisor 1)) "
Same as TRUNCATE, but returns a float as the first value.")

(docfun funcall function (function &rest args) "
Calls FUNCTION with the ARGs as the arguments and returns all values that the
call returns.")

(doctype function "
A function object specifies a function to be invoked by function-calling
functions such as FUNCALL or APPLY.  A function is either:
	1. a compiled function
	2. a list of one of the following form
		(lambda lambda-list . body)
		(lambda-block block-name lambda-list . body)
		(lambda-closure env1 env2 env3 lambda-list . body)
		(lambda-block-closure env1 env2 env3 block-name lambda-list
		                      . body)
	   where ENV1, ENV2, and ENV3 respectively represent the variable
	   environment, the function/macro environment, and the block/tagbody
	   environment at the time of the function creation.
	3. a symbol that names a global function.")

(docfun function special "(function x) | #'x" "
If X is a lambda expression, creates and returns a lexical closure of X in the
current lexical environment.  If X is a symbol that names a function, returns
that function definition.")

(docfun functionp function (x) "
Returns T if X is an object that can be used to specify a function to be
invoked by function-calling functions such as FUNCALL or APPLY.  Returns NIL
otherwise.")

(docfun gc function (x) "
ECL specific.
Starts garbage collection with the specified collection level.  If X is NIL,
collects only cells.  If X is T, collects everything.")

(docfun sys:gc-time function () "
ECL specific.
Returns the amount of time (in 1/100 seconds) spent during garbage collection.")

(docfun gcd function (&rest integers) "
Returns the greatest common divisor of the args.")

(docfun gensym function (&optional (x nil)) "
Creates and returns a new uninterned symbol whose print name begins with some
prefix (initially \"G\"), followed by a generation number.  The generation
number is incremented by one at each call to GENSYM.  If X is an integer, it
becomes the new generation number.  If X is a string, it becomes the new
prefix.")

(docfun gentemp function (&optional (string \"T\") (package *package*)) "
Creates a new symbol interned in PACKAGE with PREFIX and returns the symbol.
The symbol is given a print name beginning with PREFIX followed by some
generation number.")

(docfun get function (symbol property &optional (default nil)) "
Searches the symbol property of SYMBOL for a property that is EQ to PROPERTY.
If found, returns the value of the property.  Otherwise, returns DEFAULT.")

(docfun get-decoded-time function () "
Returns the current day-and-time as nine values:
	second (0 - 59)
	minute (0 - 59)
	hour (0 - 23)
	date (1 - 31)
	month (1 - 12)
	year (Christian, not Japanese long-live-Emperor)
	day of week (0 for Mon, .. 6 for Sun)
	summer time or not (T or NIL)
	time zone (-9 in Japan)
Sunday is the *last* day of the week!!")

(docfun get-dispatch-macro-character function (char subchar &optional (readtable *readtable*)) "
Returns the read macro for SUBCHAR associated with the dispatch macro
character CHAR in READTABLE.")

(docfun sys:get-hole-size function () "
ECL specific.
Returns as a fixnum the size of the memory hole (in pages).")

(docfun get-internal-real-time function () "
Returns the time (in 1/100 seconds) since the invocation of ECL.")

(docfun get-internal-run-time function () "
Returns the CPU time (in 1/100 seconds) since the invocation of ECL.")

(docfun sys::get-local-time-zone function () "
ECL specific.
Returns a number corresponding to the local time zone.")

(docfun get-macro-character function (char &optional (readtable *readtable*)) "
Returns the read macro associated with the macro character CHAR in READTABLE.
Returns the non-terminating-p flag (see READTABLE) as the second value.
Returns NIL if CHAR is not a macro character.")

(docfun get-output-stream-string function (string-output-stream) "
Returns as a string all outputs to STRING-OUTPUT-STREAM since the last call of
GET-OUTPUT-STREAM-STRING for the same stream.")

(docfun get-properties function (plist list) "
Searches PLIST for a property that is EQ to one of the members of LIST.
Returns three values.  If such a property if found, returns the property, the
value of the property, and the rest of LIST.  If not, returns three NILs.")

(docfun get-setf-method function (place) "
Returns the 'five gangs' (see DEFINE-SETF-METHOD) for PLACE as five values.
Checks if the third gang is a single-element list.")

(docfun get-setf-method-multiple-value function (form) "
Returns the 'five gangs' (see DEFINE-SETF-METHOD) for PLACE as five values.
Does not check if the third gang is a single-element list.")

(docfun sys:get-string-input-stream-index function (string-input-stream) "
ECL specific.
Returns the current index of STRING-INPUT-STREAM.")

(docfun get-universal-time function () "
Returns the current day-and-time as an integer.  See DECODE-UNIVERSAL-TIME.")

#+unix
(docfun sys:getenv function (string) "
ECL/UNIX specific.
Returns the environment with the name STRING as a string.  Returns NIL, if the
specified environment is not found.")

(docfun getf function (plist property &optional (default nil)) "
Searches PLIST for a property that is EQ to PROPERTY.  If one is found,
returns the value of the property.  If not, returns DEFAULT.
The SETF form
	(setf (getf place property-form) value-form)
replaces the property value of the plist stored in PLACE, or adds a new
property if the plist does not have the property yet.")

(docfun gethash function (key hash-table &optional (default nil)) "
Searches HASH-TABLE for the entry of KEY.  If found, returns the value of the
entry and T, as two values.  If not, returns DEFAULT and NIL.")

#+clos
(docfun sys:gfun-instance function (gfun) "
ECL/CLOS specific.
Returns the generic function instance associated with the GFUN
generic function object.")

#+clos
(docfun sys:gfun-instance-set function (gfun instance) "
ECL/CLOS specific.
Sets to INSTANCE the generic function instance associated with the
FUN generic function object.")

#+clos
(docfun sys:gfun-name function (gfun) "
ECL/CLOS specific.
Returns the name of the GFUN generic function object.")

#+clos
(docfun sys:gfun-name-set function (gfun name) "
ECL/CLOS specific.
Sets to NAME the name of the GFUN generic function object.")

#+clos
(docfun sys:gfun-method-ht function (gfun) "
ECL/CLOS specific.
Returns the hashtable for caching methods associated with the GFUN
generic function object.")

#+clos
(docfun sys:gfun-method-ht-set function (gfun hash-table) "
ECL/CLOS specific.
Sets to HASH-TABLE the hashtable for caching methods associated with the
GFUN generic function object.")

#+clos
(docfun sys:gfun-spec-how-ref function (gfun index) "
ECL/CLOS specific.
Returns the INDEX-th element of specialization list associated  with the
GFUN generic function object. The first element has INDEX equal to zero.")

#+clos
(docfun sys:gfun-spec-how-set function (gfun index specializer) "
ECL/CLOS specific.
Sets to SPECIALIZER the INDEX-th element of specialization list associated
with the GFUN generic function object. The first element has INDEX
equal to zero.")

#+clos
(docfun sys:gfunp function (object) "
ECL/CLOS specific.
Returns T if OBJECT is of gfun type.")

(docfun go special "(go tag)" "
Jumps to TAG.  See TAGBODY.")

(docfun graphic-char-p function (char) "
Returns T if CHAR is a printing character, i.e., a standard character other
than #\\Newline.  Returns NIL otherwise.")

(doctype hash-table "
A hash-table is a table used to map from objects to objects efficiently by the
hashing technique.  A hash-table is notated as
	#<hash-table n>
where N is actually a number that identifies the hash-table.")

(docfun hash-table-count function (hash-table) "
Returns the number of entries in HASH-TABLE.")

(docfun hash-table-p function (x) "
Returns T if X is a hash-table object; NIL otherwise.")

(docfun help function (&optional symbol) "
ECL specific.
Prints the documentation associated with SYMBOL.  With no args, prints the
greeting message to ECL beginners.")

(docfun help* function (string &optional (package-spec 'lisp)) "
ECL specific.
Prints the documentation associated with those symbols in the specified
package whose print names contain STRING as substring.  STRING may be a
symbol, in which case the print-name of that symbol is used.  If PACKAGE is
NIL, then all packages are searched.")

(docfun host-namestring function (filespec) "
Returns as a string the host part of the pathname specified by FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun identity function (x) "
Returns X.")

(docfun if special "(if test form1 [form2])" "
If TEST evaluates to non-NIL, then evaluates FORM1 and returns all values.
Otherwise, evaluates FORM2 (which defaults to NIL) and returns all values.")

(docfun sys:ihs-fun function (n) "
ECL specific.
Returns the function value of the N-th entity in the invocation history stack.")

(docfun imagpart function (number) "
Returns the imagpart of NUMBER if it is a complex.  Otherwise, returns zero of
the same type as NUMBER.")

(docfun import function (symbol &optional (package *package*)) "
Registers SYMBOL to PACKAGE as an internal symbol.  Does nothing if SYMBOL is
already registered in PACKAGE.  SYMBOL may be a list of symbols.")

(docfun in-package function (package-name &key (nicknames nil) (use '(lisp))) "
Makes the package named PACKAGE-NAME as the current package.  If such a
package does not exist, then creates one by passing all args to MAKE-PACKAGE.
Otherwise, adds the specified nicknames and packages to the nickname list and
use list of the package.  NICKNAMES must be a list consisting of strings and
symbols.  USE must be a list consisting of package objects and package names
(either string or symbol).")

(docfun incf macro "(incf place [form])" "
Increments the value of PLACE by the value of FORM.  FORM defaults to 1.")

(docfun input-stream-p function (stream) "
Returns T if STREAM can handle input operations; NIL otherwise.")

(docfun inspect function (x) "
Shows the information about X interactively.  See the ECL Report for the
inspect commands, or type '?' to the inspector.")

#+clos
(docfun sys:instancep function (object) "
ECL/CLOS specific.
Returns T if OBJECT is of instance type.")

#+clos
(docfun sys:instance-ref function (instance index) "
ECL/CLOS specific.
Returns the value of the INDEX-th slot of INSTANCE. The first slot has
INDEX equal to zero.")

#+clos
(docfun sys:instance-set function (instance index value) "
ECL/CLOS specific.
Sets to VALUE the value of INDEX-th slot of INSTANCE. The first slot has
INDEX equal to zero.")

#+clos
(docfun sys:instance-class function (instance) "
ECL/CLOS specific.
Returns the class of which the given INSTANCE is an instance.")

#+clos
(docfun sys:instance-class-set function (instance class) "
ECL/CLOS specific.
Makes INSTANCE an instance of CLASS class.")

(docfun int-char function (integer) "
Equivalent to CODE-CHAR.")

(doctype integer "
An integer object represents an integer in mathematical sense.  An integer may
be a fixnum, or else it is a bignum.  Normally, an integer is notated in radix
10 (see *PRINT-BASE* and *READ-BASE*) as
	[sign] {digit}+
where DIGIT is a decimal digit ('0', ..., '9') and SIGN is either '+' or '-'.
Also, the following syntax is used to notate the radix explicitly.
	# radix {r | R} [sign] {digit}+
where RADIX is one of '2', '3', ..., '36' and DIGIT is a digit in radix RADIX:
	Digits in radix 2 are '0' and '1'
	Digits in radix 8 are '0', ..., '7'
	Digits in radix 16 are '0', ..., '9', 'a', ..., 'f', and 'A', ..., 'F'
The following syntax is also available for radix 2, 8, 10, and 16.
	# {b | B} [sign] {digit}+
	# {o | O} [sign] {digit}+
		  [sign] {digit}+ .
	# {x | X} [sign] {digit}+")

(docfun integer-decode-float function (float) "
Returns, as three values, the integer interpretation of significand F, the
exponent E, and the sign S of FLOAT, such that
	FLOAT = S * F * B^E
where B = (float-radix FLOAT).  F is a non-negative integer, E is an integer,
and S is either 1 or -1.")

(docfun integer-length function (integer) "
Returns the number of \"significant bits\" in the representation of INTEGER.
With positive arg, returns one plus the position of the most significant bit
that is 'on'.  With negative arg other than -1, returns one plus the position
of the most significant bit that is 'off'.  For 0 and -1, returns 0.")

(docfun integerp function (x) "
Returns T if X is an integer; NIL otherwise.")

(docfun intern function (string &optional (package *package*)) "
Searches PACKAGE for a symbol whose print name is STRING.  If such a symbol is
found, returns the symbol and, as the second value, one of the keywords
:INTERNAL, :EXTERNAL, and :INHERITED.  Otherwise, creates and returns a new
symbol and, as the second value, returns NIL.")

(docvar internal-time-units-per-second constant "
Gives the time unit used by GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.
1000 in ECL.")

(docfun intersection function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Returns a list consisting of those objects that are elements of both LIST1 and
LIST2.")

(docfun isqrt function (integer) "
Returns the integer square root of INTEGER.")

(doctype keyword "
A keyword is a symbol in the keyword package.")

(docfun keywordp function (x) "
Returns T if X is a symbol that belongs to the KEYWORD package; NIL otherwise.")

(docfun labels special
	"(labels ({(name lambda-list {decl | doc}* {form}*)}*) . body)" "
Introduces local functions and evaluates BODY as a PROGN.  The scope of each
local function include the local function definitions.  Thus self- and mutual-
recursive local functions can be defined.  Doc-strings for local functions are
simply ignored.")

(docvar lambda-list-keywords constant "
List of all lambda-list keywords, including
	&optional	&rest		&key
	&allow-other-keys		&aux
	&whole		&environment	&body")

(docvar lambda-parameters-limit constant "
The upper bound of the number of parameters specified by a lambda list.
Ignore this number; there is no such upper bound in ECL.")

(docfun last function (list) "
Returns the last cons that constitute LIST.  Returns NIL if LIST is NIL.")

(docfun lcm function (integer &rest more-integers) "
Returns the least common multiple of the args.  Returns 0 if at least one of
the args is 0.")

(docfun ldb function (bytespec integer) "
Extracts a byte from INTEGER at the specified byte position, right-justifies
the byte, and returns the result as an integer.")

(docfun ldb-test function (bytespec integer) "
Returns T if at least one bit of the specified byte is 1; NIL otherwise.")

(docfun ldiff function (list x) "
If X is a cons that constitutes LIST, then returns a new list consisting of
those elements of LIST that appear before X.  Otherwise, returns a copy of
LIST.")

(docvar least-negative-double-float constant "
Same as LEAST-NEGATIVE-LONG-FLOAT.")

(docvar least-negative-long-float constant "
The negative long-float with the smallest absolute value.")

(docvar least-negative-short-float constant "
The negative short-float with the smallest absolute value.")

(docvar least-negative-single-float constant "
Same as LEAST-NEGATIVE-LONG-FLOAT.")

(docvar least-positive-double-float constant "
Same as LEAST-POSITIVE-LONG-FLOAT.")

(docvar least-positive-long-float constant "
The smallest positive long-float.")

(docvar least-positive-short-float constant "
The smallest positive short-float.")

(docvar least-positive-single-float constant "
Same as LEAST-POSITIVE-LONG-FLOAT.")

(docfun length function (sequence) "
Returns the length of SEQUENCE.")

(docfun let special "(let ({var | (var [init])}*) {decl}* {form}*)" "
Evaluates all INITs (which defaults to NIL), binds the value of each INIT to
the corresponding VAR, evaluates FORMs, and returns all values of the last
FORM.  Returns NIL if no FORM is given.")

(docfun let* special "(let* ({var | (var [init])}*) {decl}* {form}*)" "
Evaluates INIT (which defaults to NIL) and binds the value to the
corresponding VAR, one by one for each pair of VAR and INIT.  Then evaluates
FORMs and returns all values of the last FORM.  Returns NIL if no FORM is
given.")

(docfun lisp-implementation-type function () "
Returns the string \"Eco Common Lisp\".")

(docfun lisp-implementation-version function () "
Returns the version of your ECL as a string.")

(doctype list "
As a type specifier, 'list' is used to specify the type consisting of NIL and
cons objects.  In our ordinary life with Lisp, however, a list is either NIL
or a cons whose cdr is a list, and is notated by its elements surrounded with
parentheses.
The backquote macro is sometimes useful to construct a complicated list
structure.  When evaluating `(...)
	,form embeds the value of FORM,
	,@form and ,.form embed all elements of the list value of FORM,
	and other things embed itself
into the structure at their position.  For example,
	`(a b ,c d e) expands to (list* 'a 'b c '(d e))
	`(a b ,@c d e) expands to (list* 'a 'b (append c '(d e)))
	`(a b ,.c d e) expands to (list* 'a 'b (nconc c '(d e)))")

(docfun list function (&rest args) "
Returns a list of the args.")

(docfun list* function (arg &rest more-args) "
With one arg, simply returns it.  With n args (n > 1), conses the first arg to
the LIST* of the rest of args.")

(docfun list-all-packages function () "
Returns a list of all packages.")

(docfun list-length function (list) "
Returns the length of LIST.  Returns NIL if LIST is circular.")

(docfun listen function (&optional (stream *standard-input*)) "
Returns T if STREAM is ready to input a character from; NIL otherwise.  In
some versions of ECL, this function does not work correctly because the
underlying OS does not support such a mechanism.")

(docfun listp function (x) "
Returns T if X is either a cons or NIL.  Otherwise, returns NIL.")

(docfun load function (filespec
       &key (verbose *load-verbose*) (print nil) (if-does-not-exist :error)) "
Loads the contents of the specified file into ECL.
If the filetype is not specified, ECL first tries to load the fasl file with
filetype \".fasl\", then tries to load the source file with filetype \".lsp\",
and then tries to load the source file with no filetype.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  VERBOSE
specifies whether or not the loader prints a loading message.  PRINT specifies
whether or not the loader prints the values of the top-level forms.
IF-DOES-NOT-EXIST specifies the behavior of the loader when the specified file
is not found.  It may be :ERROR or NIL.
If the file was loaded successfully, returns the pathname of the file actually
loaded")

(docfun locally macro "(locally {decl}* {form}*)" "
Gives DECLs locally while evaluating FORMs, and returns all values of the last
FORM.  Returns NIL if no FORM is given.")

(docfun log function (number1 &optional number2) "
With two args, returns the logarithm of NUMBER1 in base NUMBER2.  With one
arg, returns the natural logarithm of the arg.")

(docfun logand function (&rest integers) "
Returns the bit-wise AND of the args.")

(docfun logandc1 function (integer1 integer2) "
Equivalent to (LOGAND (LOGNOT INTEGER1) INTEGER2).")

(docfun logandc2 function (integer1 integer2) "
Equivalent to (LOGAND INTEGER1 (LOGNOT INTEGER2)).")

(docfun logbitp function (bit-position integer) "
Returns T if the specified bit of INTEGER is 1; NIL otherwise.  BIT-POSITION
must be a non-negative integer, with 0 representing the least significant bit.")

(docfun logcount function (integer) "
If INTEGER is negative, returns the number of 0 bits.  Otherwise, returns the
number of 1 bits.")

(docfun logeqv function (&rest integers) "
Returns the bit-wise EQUIVALENCE of the args.")

(docfun logior function (&rest integers) "
Returns the bit-wise INCLUSIVE OR of the args.")

(docfun lognand function (integer1 integer2) "
Equivalent to (LOGNOT (LOGAND INTEGER1 INTEGER2)).")

(docfun lognor function (integer1 integer2) "
Equivalent to (LOGNOT (LOGIOR INTEGER1 INTEGER2)).")

(docfun lognot function (integer) "
Returns the bit-wise logical NOT of the arg.")

(docfun logorc1 function (integer1 integer2) "
Equivalent to (LOGIOR (LOGNOT INTEGER1) INTEGER2).")

(docfun logorc2 function (integer1 integer2) "
Equivalent to (LOGIOR INTEGER1 (LOGNOT INTEGER2)).")

(docfun logtest function (integer1 integer2) "
Equivalent to (NOT (ZEROP (LOGAND INTEGER1 INTEGER2))).")

(docfun logxor function (&rest integers) "
Returns the bit-wise EXCLUSIVE OR of the args.")

(doctype long-float "
A long-float is a long-precision floating point number.")

(docvar long-float-epsilon constant "
The smallest positive long-float E that satisfies
	(not (= (float 1 E) (+ (float 1 E) E)))")

(docvar long-float-negative-epsilon constant "
The smallest positive long-float E that satisfies
	(not (= (float 1 E) (- (float 1 E) E)))")

(docfun long-site-name function () "
Returns, as a string, the location of the machine on which ECL runs.")

(docfun loop macro "(loop {form}*)" "
Establishes a NIL block and executes FORMs repeatedly.  The loop is normally
terminated by a non-local exit.
The MIT-style extended version of LOOP, which is upper compatible with this
LOOP, is available:  Load loop.lsp or its object file (loop.o or loop.fasl) in
the \"lsp\" subdirectory of your ECL system directory and then see the doc of
LOOP again.")

(docfun lower-case-p function (char) "
Returns T if CHAR is a lower-case character; NIL otherwise.")

(docfun machine-instance function () "
Returns, as a string, the identifier of the machine on which ECL runs.")

(docfun machine-type function () "
Returns, as a string, the type of the machine on which ECL runs.")

(docfun machine-version function () "
Returns, as a string, the version of the machine on which ECL runs.")

(docfun macro-function function (symbol) "
Returns the expansion function of the global macro named SYMBOL.  Returns NIL
if no such macro exists.  The expansion function receives a macro form and an
environment, and returns the expanded form.")

(docfun macroexpand function (form &optional (env nil)) "
If FORM is a macro form, then expands it repeatedly until the result is not a
macro any more, and returns the result as the first value and T as the second
value.  Otherwise, returns FORM and NIL as two values.")

(docfun macroexpand-1 function (form &optional (env nil)) "
If FORM is a macro form, then expands it once and returns the result as the
first value and T as the second value.  Otherwise, returns FORM and NIL as two
values.")

(docfun macrolet special
	"(macrolet ({(name defmacro-lambda-list {decl | doc}* {form}*)}*)
          . body)" "
Introduces local macros and evaluates BODY as a PROGN.  See DEFMACRO for the
complete syntax of defmacro-lambda-list.  Doc-strings for local macros are
simply ignored.")

(docfun make-array function
	(dimensions &key (element-type t) initial-element (initial-contents nil)
		    (adjustable nil) (fill-pointer nil) (displaced-to nil)
		    (displaced-index-offset 0) (static nil)) "
Creates an array of the specified DIMENSIONS.  DIMENSIONS is a list of
non-negative integers each representing the length of the corresponding
dimension.  It may be an integer for vectors, i.e., one-dimensional arrays.
ELEMENT-TYPE specifies the type of array elements.  INITIAL-ELEMENT specifies
the initial value for all elements.  Its default value depends on ELEMENT-
TYPE.  INITIAL-CONTENTS specifies each element in terms of sequences.
ADJUSTABLE specifies whether or not the array is adjustable (see ADJUST-
ARRAY).  FILL-POINTER is meaningful only for vectors.  It specifies whether
the vector has fill-pointer or not, and if it has, the initial value of the
fill-pointer.  Possible values are NIL (no fill-pointer), T (the length of the
vector), or an integer.  See VECTOR-PUSH and VECTOR-POP.  DISPLACED-TO, if
non-NIL, must be an array and specifies that the new array is displaced to the
given array.  DISPLACED-INDEX-OFFSET is meaningful only when DISPLACED-TO is
non-NIL and specifies that the reference to the I-th element of the new array
in raw-major indexing is actually the reference to the (I + DISPLACED-INDEX-
OFFSET)th element of the given array.If the STATIC argument is supplied
with a non-nil value, then the body of the array is allocated as a
contiguous block.")

(docfun make-broadcast-stream function (&rest streams) "
Creates and returns a broadcast stream.  Outputs to this stream are output to
all STREAMs.  A broadcast stream is notated as
	#<broadcast stream n>
where N is a number that identify the stream.")

(docfun make-char function (char &optional (bits 0) (font 0)) "
Returns a character object with the same code as CHAR and with the specified
BITS and FONT attributes.  Returns NIL if no such character exists.")

(docfun make-concatenated-stream function (&rest streams) "
Creates and returns a concatenated stream.  Inputs from this stream are first
obtained from the first STREAM.  When the end of the first STREAM is reached,
then inputs are obtained from the second STREAM.  And so forth.
A concatenated stream is notated as
	#<concatenated stream n>
where N is a number that identifies the stream.")

(docfun make-dispatch-macro-character function (char &optional (non-terminating-p nil) (readtable *readtable*)) "
Register CHAR as a dispatch macro character in READTABLE.  NON-TERMINATING-P
specifies whether CHAR is non-terminating (see READTABLE).")

(docfun make-echo-stream function (stream1 stream2) "
Creates and returns an echo stream.  Inputs from this stream are obtained from
STREAM1 and outputs to this stream are output to STREAM2.  In addition, all
inputs from STREAM1 are output to STREAM2.
An echo stream is notated as
	#<echo stream n>
where N is a number that identifies the stream.")

(docfun make-hash-table function (&key (test 'eql) (size 1024) (rehash-size 1.5) (rehash-threshold 0.7)) "
Creates and returns a hash-table.
TEST specifies which predicate should be used to access hash-table entries.
It must be EQ, EQL, or EQUAL.  SIZE specifies the number of entries in the
hash-table.  REHASH-SIZE, if an integer, specifies how many entries should be
added when the hash-table becomes 'almost full'.  REHASH-SIZE, if a float,
specifies the ratio of the new size and the old size.  REHASH-THRESHOLD
specifies when to expand the hash-table.  If an integer, the hash-table is
expanded when REHASH-THRESHOLD / REHASH-SIZE entries have been used.  If a
float, the hash-table is expanded when REHASH-THRESHOLD times the whole
entries have been used.")

(docfun make-list function (length &key (initial-element nil)) "
Creates and returns a list of the specified LENGTH, whose elements are all the
value of INITIAL-ELEMENT.")

(docfun make-package function (package-name &key (nicknames nil) (use '(lisp))) "
Creates and returns a new package named PACKAGE-NAME.  PACKAGE-NAME must be a
string or a symbol.  The print name is used if PACKAGE-NAME is a symbol.
NICKNAMES gives the nicknames of the package.  It must be a list of strings
and symbols.  USE specifies the packages used by the created package.  It must
be a list of package objects, strings, and symbols.")

(docfun make-pathname function (&key (defaults (parse-namestring \"\"
                        (pathname-host *default-pathname-defaults*)))
            (host (pathname-host defaults))
            (device (pathname-device defaults))
            (directory (pathname-directory defaults))
            (name (pathname-name defaults))
            (type (pathname-type defaults))
            (version (pathname-version defaults))) "
Creates a pathname object with the slot values specified by HOST, DEVICE,
DIRECTORY, NAME, TYPE, and VERSION.")

(docfun make-random-state function (&optional (random-state nil)) "
Creates and returns a random-state object.  If RANDOM-STATE is NIL, copies the
value of *RANDOM-STATE*.  If RANDOM-STATE is a random-state, copies it.  If
RANDOM-STATE is T, creates a random-state randomly.")

(docfun make-sequence function (type length &key initial-element) "
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE.")

(docfun make-string function (length &key (initial-element #\Space)) "
Creates and returns a new string of the given LENGTH, whose elements are all
INITIAL-ELEMENT.")

(docfun make-string-input-stream function (string &optional (start 0) (end (length string))) "
Creates and returns a string-input stream.  Inputs from this stream are
obtained form STRING.  A string-input stream is notated as
	#<string-input stream from s>
where S is a string.")

(docfun make-string-output-stream function () "
Creates and returns a string-output stream.  Outputs to this stream are
obtained as a string by GET-OUTPUT-STREAM-STRING.  A string-output stream
is notated as
	#<string-output stream n>
where N is a number that identifies the stream.")

(docfun sys:make-string-output-stream-from-string function (string) ")
ECL specific.
Creates and returns a string-output-stream to STRING.  STRING must have a
fill-pointer.")

(docfun make-symbol function (string) "
Creates and returns a new uninterned symbol whose print name is STRING.")

(docfun make-synonym-stream function (symbol) "
Creates and returns a synonym stream to SYMBOL.  Inputs from this stream are
obtained from, and outputs to this stream are sent to the stream that is the
value of the global variable named SYMBOL.  A synonym stream is notated as
	#<synonym stream to s>
where S is a symbol.")

(docfun make-two-way-stream function (stream1 stream2) "
Creates and returns a two-way stream.  Inputs from this stream are obtained
from STREAM1 and outputs to this stream are sent to STREAM2.  A two-way stream
is notated as
	#<two-way stream n>
where N is a number that identifies the stream.")

(docfun makunbound function (symbol) "
Makes the global variable named SYMBOL have no value.  Returns SYMBOL.")

(docfun map function (type function sequence &rest more-sequences) "
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs.")

(docfun mapc function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th elements of the given
LISTs, where K is the minimum length of the given LISTs.  Returns the first
LIST.")

(docfun mapcan function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th elements of the given
LISTs, where K is the minimum length of the given LISTs.  Nconcs the values,
one for each call to FUNCTION, and returns the result.")

(docfun mapcar function (function list &rest more-lists) "
Creates and returns a list of K elements, with the N-th element being the
value of applying FUNCTION to the N-th elements of the given LISTs, where K
is the minimum length of the given LISTs.")

(docfun mapcon function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th cdrs of the given LISTs,
where K is the minimum length of the given LISTs.  Nconcs the values, one for
each call to FUNCTION, and returns the result.")

(docfun maphash function (function hash-table) "
For each entry of HASH-TABLE, applies FUNCTION to the key and the value of the
entry.  Returns NIL.")

(docfun mapl function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th cdrs of the given LISTs,
where K is the minimum length of the given LISTs.  Returns the first LIST.")

(docfun maplist function (function list &rest more-lists) "
Creates and returns a list of K elements, with the N-th element being the
value of applying FUNCTION to the N-th cdrs of the given LISTs, where K is the
minimum length of the given LISTs.")

(docfun mask-field function (bytespec integer) "
Extracts the specified byte from INTEGER and returns the result as an integer.")

(docfun max function (number &rest more-numbers) "
Returns the largest arg.  The args must be non-complex numbers.")

(docfun maximum-allocatable-pages function (type) "
ECL specific.
Returns the current maximum number of pages for the type class of the ECL
implementation type TYPE.")

(docfun sys:maximum-contiguous-pages function () "
ECL specific.
Returns the current maximum number of pages for contiguous blocks.")

(docfun member function (item list &key (key '#'identity) (test '#'eql) test-not) "
Searches LIST for an element that is equal to ITEM in the sense of the TEST.
If found, returns the sublist of LIST that begins with the element.
Otherwise, returns NIL.")

(docfun member-if function (test list &key (key '#'identity)) "
Searches LIST for an element that satisfies TEST.  If found, returns the
sublist of LIST that begins with the element.  If not found, returns NIL.")

(docfun member-if-not function (test list &key (key '#'identity)) "
Searches LIST for an element that does not satisfy TEST.  If found, returns
the sublist of LIST that begins with the element.  If not found, returns NIL.")

(docfun merge function (type sequence1 sequence2 test &key (key '#'identity)) "
Merges two sequences in the way specified by TEST and returns the result as a
sequence of TYPE.  Both SEQUENCEs may be destroyed.  If both SEQUENCE1 and
SEQUENCE2 are sorted in the sense of TEST, then the result is also sorted in
the sense of TEST.")

(docfun merge-pathnames function (filespec
       &optional (defaults *default-pathname-defaults*) default-version) "
Fills in unspecified slots of the pathname specified by FILESPEC from the
pathname specified by DEFAULTS, and returns the result pathname.  DEFAULT-
VERSION is simply ignored in ECL.  FILESPEC and DEFAULTS may be a symbol, a
string, a pathname, or a file stream.")

(docfun min function (number &rest more-numbers) "
Returns the smallest arg.  The args must be non-complex numbers.")

(docfun minusp function (number) "
Returns T if NUMBER is negative; NIL otherwise.")

(docfun mismatch function (sequence1 sequence2
       &key (key '#'identity) (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil)) "
Compares element-wise the specified subsequences of SEQUENCE1 and SEQUENCE2.
Returns NIL if they are of the same length and they have the same elements in
the sense of TEST.  Otherwise, returns the index of SEQUENCE1 to the first
element that does not match.")

(docfun mod function (number divisor) "
Returns the second result of (FLOOR NUMBER DIVISOR), i.e. the value of
	(- NUMBER (* (FLOOR NUMBER DIVISOR) DIVISOR))")

(docvar most-negative-double-float constant "
Same as MOST-NEGATIVE-LONG-FLOAT.")

(docvar most-negative-fixnum constant "
The negative fixnum with the largest absolute value.  - 2^29 in ECL.")

(docvar most-negative-long-float constant "
The long-float with the largest absolute value.")

(docvar most-negative-short-float constant "
The short-float with the largest absolute value.")

(docvar most-negative-single-float constant "
Same as MOST-NEGATIVE-LONG-FLOAT.")

(docvar most-positive-double-float constant "
Same as MOST-POSITIVE-LONG-FLOAT.")

(docvar most-positive-fixnum constant "
The largest positive fixnum.  2^29 - 1 in ECL.")

(docvar most-positive-long-float constant "
The largest positive long-float.")

(docvar most-positive-short-float constant "
The largest positive short-float.")

(docvar most-positive-single-float constant "
Same as MOST-POSITIVE-LONG-FLOAT.")

(docfun multiple-value-bind macro
	"(multiple-value-bind ({var}*) init {decl}* {form}*)" "
Evaluates INIT and binds the N-th VAR to the N-th value of INIT or, if INIT
returns less than N values, to NIL.  Then evaluates FORMs, and returns all
values of the last FORM.  If no FORM is given, returns NIL.")

(docfun multiple-value-call special
	"(multiple-value-call function-form {form}*)" "
Evaluates FUNCTION-FORM, whose value must be a function.  Then evaluates FORMs
and applies the function to all values of FORMs.  Unlike FUNCALL, all values
of each FORM are used as arguments.  Returns all values of the function.")

(docfun multiple-value-list macro "(multiple-value-list form)" "
Evaluates FORM and returns a list of all values FORM returns.")

(docfun multiple-value-prog1 special
	"(multiple-value-prog1 first-form {form}*)" "
Evaluates FIRST-FORM, saves all values it returns, and then evaluates FORMs.
Returns all the saved values of FIRST-FORM.")

(docfun multiple-value-setq macro "(multiple-value-setq {var}* form)" "
Evaluates FORM and binds the N-th VAR to the N-th value of FORM or, if FORM
returns less than N values, to NIL.  Returns the first value of FORM or, if
FORM returns no value, NIL.")

(docvar multiple-values-limit constant "
The upper bound on the number of values that a function can return.  Actually,
however, there is no such upper bound in ECL.")

(docfun sys:nani function (fixnum) "
ECL specific.
Returns the object at the address FIXNUM.  This function is the inverse of
SYS:POINTER.  Although SYS:POINTER is a harmless operation, SYS:NANI is quite
dangerous and should be used with care.")

(docfun name-char function (name) "
Given an argument acceptable to string,
Returns a character object with the specified character name (see CHARACTER).
Returns NIL if no such character object exists.  NAME is typically a string
but may be any object that can be coerced to string.")

(docfun namestring function (filespec) "
Returns as a string all slots of the pathname specified by FILESPEC.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.")

(docfun nbutlast function (list &optional (n 1)) "
Destructive BUTLAST.  LIST may be destroyed.")

(docfun nconc function (&rest lists) "
Destructive APPEND.  The args except for the last may be destroyed.")

(doctype nil "
The type NIL is a subtype of every type.  No object belongs to this type.")

(docvar nil constant "
The value of NIL is NIL.")

(docfun nintersection function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Destructive INTERSECTION.  Only LIST1 may be destroyed.")

(docfun ninth function (x) "
Equivalent to (CAR (CDDDDR (CDDDDR X))).")

(docfun not function (x) "
Returns T if X is NIL; NIL otherwise.")

(docfun notany function (predicate sequence &rest more-sequences) "
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise.")

(docfun notevery function (predicate sequence &rest more-sequences) "
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise.")

(docfun nreconc function (x y) "
Equivalent to (NCONC (NREVERSE X) Y).")

(docfun nreverse function (sequence) "
Destructive REVERSE.  The arg may be destroyed.")

(docfun nset-difference function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Destructive SET-DIFFERENCE.  Only LIST1 may be destroyed.")

(docfun nset-exclusive-or function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Destructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed.")

(docfun nstring-capitalize function (string &key (start 0) (end (length string))) "
Destructive STRING-CAPITALIZE.  STRING may be destroyed.")

(docfun nstring-downcase function (string &key (start 0) (end (length string))) "
Destructive STRING-DOWNCASE.  STRING may be destroyed.")

(docfun nstring-upcase function (string &key (start 0) (end (length string))) "
Destructive STRING-UPCASE.  STRING may be destroyed.")

(docfun nsublis function (alist tree &key (key '#'identity) (test '#'eql) test-not) "
Destructive SUBLIS.  TREE may be destroyed.")

(docfun nsubst function (new old tree &key (key '#'identity) (test '#'eql) test-not) "
Destructive SUBST.  TREE may be destroyed.")

(docfun nsubst-if function (new test tree &key (key '#'identity)) "
Destructive SUBST-IF.  TREE may be destroyed.")

(docfun nsubst-if-not function (new test tree &key (key '#'identity)) "
Destructive SUBST-IF-NOT.  TREE may be destroyed.")

(docfun nsubstitute function (new old sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE.  SEQUENCE may be destroyed.")

(docfun nsubstitute-if function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE-IF.  SEQUENCE may be destroyed.")

(docfun nsubstitute-if-not function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE-IF-NOT.  SEQUENCE may be destroyed.")

(docfun nth function (n list) "
Returns the N-th element of LIST, the first element of LIST being the zeroth.
Returns NIL if the length of LIST is less than N.  N must be a non-negative
integer.")

(docfun nthcdr function (n list) "
Returns the N-th cdr of LIST.  N must be a non-negative integer.")

(doctype null "
The type to which only NIL belongs.")

(docfun null function (x) "
Returns T if X is NIL; NIL otherwise.")

(doctype number "
A number is an integer, a ratio, a float, or a complex number.  Integers and
ratios are collectively called rationals.")

(docfun numberp function (x) "
Returns T if X is a number; NIL otherwise.")

(docfun numerator function (rational) "
Returns the numerator of RATIONAL as an integer, if RATIONAL is a ratio.
Returns RATIONAL if it is an integer.")

(docfun nunion function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Destructive UNION.  Both LIST1 and LIST2 may be destroyed.")

(docfun oddp function (integer) "
Returns T if INTEGER is an odd number; NIL otherwise.")

(docfun open function (filespec &key (direction :input) element-type
                     if-exists if-does-not-exist) "
Opens the specified file and returns a file stream to/from the file.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.  DIRECTION may be
:INPUT, :OUTPUT, :IO, or :PROBE.  ELEMENT-TYPE is simply ignored in ECL.  IF-
EXISTS specifies what to do when DIRECTION is either :OUTPUT or :IO and the
specified file exists already.  It may be :ERROR (the default), :NEW-VERSION,
:RENAME, :RENAME-AND-DELETE, :OVERWRITE, :APPEND, :SUPERSEDE, or NIL.  IF-
DOES-NOT-EXIST specifies what to do when the specified file does not exists.
It may be :ERROR (the default when DIRECTION is :INPUT), :CREATE (the default
when DIRECTION is either :OUTPUT or :IO), or NIL.
File streams are notated in one of the following ways:
	#<input stream f>
	#<output stream f>
	#<io stream f>
	#<probe stream f>
where F is the file name.")

(docfun sys::open-client-stream function (host port) "
ECL specific.
The string HOST indicates the name of the host,
while PORT is an integer identifies the port number to which to connect.
This function returns a two-way stream which can be used in any of the
stream operations.")

(docfun sys::open-server-stream function (host port) "
ECL specific.
A stream connected to port number PORT is created to which clients
can connect.
This function returns a two-way stream which can be used in any of the
stream operations.")

(docfun or macro "(or {form}*)" "
Evaluates FORMs in order from left to right.  If any FORM evaluates to non-
NIL, quits and returns that (single) value.  If the last FORM is reached,
returns whatever values it returns.")

(docfun output-stream-p function (stream) "
Returns T if STREAM can handle output operations; NIL otherwise.")

(docfun sys:output-stream-string function (string-output-stream) "
ECL specific.
Returns the string that is the destination of STRING-OUTPUT-STREAM.")

(doctype package "
A package object serves as a name space of symbols.  A package is notated as
#<s package> where S is actually the name of the package.  ECL provides five
built-in packages:
	lisp	 standard symbols of Common Lisp.
	user	 the package that the user uses by default.
	keyword	 keyword symbols.
	system	 system internal symbols.  Has nicknames SYS and SI.
	compiler system internal symbols for the ECL compiler.")

(docfun package-name function (package) "
Returns the name of PACKAGE as a string.")

(docfun package-nicknames function (package) "
Returns the nicknames of PACKAGE as a list of strings.")

(docfun package-shadowing-symbols function (package) "
Returns, as a list, those symbols in PACKAGE that are shadowing symbols in
other packages.")

(docfun package-use-list function (package) "
Returns, as a list, those packages that PACKAGE uses.")

(docfun package-used-by-list function (package) "
Returns, as a list, those packages that use PACKAGE.")

(docfun packagep function (x) "
Returns T if X is a package object; NIL otherwise.")

(docfun pairlis function (keys items &optional (alist nil)) "
Conses each KEY and the corresponding ITEM, adds them to ALIST, and returns
the result.  KEYS and ITEMS must be of the same length.")

(docfun parse-integer function (string
       &key (start 0) (end (length string)) (radix 10) (junk-allowed nil)) "
Parses STRING for an integer and returns it.  As the second value, returns the
index to the character next to the last character that is parsed.  If JUNK-
ALLOWED is non-NIL, ignores white spaces before and after the number
representation in STRING and returns NIL even if STRING is not parsed
successfully.")

(docfun parse-namestring function (string &optional host defaults &key (start 0) end (junk-allowed nil)) "
Parses STRING and returns a pathname.  As the second value, returns the index
to the character next to the last character that has been parsed.  STRING is
usually a string object but it may be a symbol, a pathname, or a file stream.
START and END are meaningful only when STRING is a string or a symbol.  They
default to 0 and (length (string FILESPEC)) respectively.  When the parsing is
failed, signals an error (if JUNK-ALLOWED is NIL) or simply returns NIL.  HOST
and DEFAULTS are simply ignored in ECL.")

(doctype pathname "
A pathname object identifies an external file or a collection of external
files.  A pathname object consists of six slots, HOST, DEVICE, DIRECTORY,
NAME, and TYPE.  HOST, DEVICE, and VERSION slots are meaningless in ECL,
though they are harmless at all.
A pathname is notated as #\\\"...\", where '...' is actually some information
on the pathname.  This depends on the version of ECL.  Refer to the ECL Report
for details.")

(docfun pathname function (filespec) "
Returns a pathname specified by FILESPEC.  FILESPEC may be a symbol, a string,
a pathname, or a file stream.")

(docfun pathname-device function (filespec) "
Returns the device slot of the pathname specified by FILESPEC.  FILESPEC may
be a symbol, a string, a pathname, or a file stream.")

(docfun pathname-directory function (filespec) "
Returns the directory slot of the pathname specified by FILESPEC.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.")

(docfun pathname-host function (filespec) "
Returns the host slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-name function (filespec) "
Returns the name slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-type function (filespec) "
Returns the type slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-version function (filespec) "
Returns the version slot of the pathname specified by FILESPEC.  FILESPEC may
be a symbol, a string, a pathname, or a file stream.")

(docfun pathnamep function (x) "
Returns T if X is a pathname object; NIL otherwise.")

(docfun peek-char function (&optional (char-spec nil) (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads characters from STREAM until the specified character is read.  Returns
the last character but leaves it in STREAM.  CHAR-SPEC may be a character
object, T (specifies non-whitespace characters), or NIL (specifies all
characters).")

(docfun phase function (number) "
Returns the angle part (in radians) of the polar representation of NUMBER.
Returns zero for non-complex numbers.")

(docvar pi constant "
The float that is approximately equal to the ratio of the circumference of the
circle to the diameter.")

(docfun sys:pointer function (object) "
ECL specific.
Returns the address of the OBJECT as a fixnum.")

(docfun plusp function (number) "
Returns T if NUMBER is positive; NIL otherwise.")

(docfun pop macro "(pop place)" "
Gets the cdr of the value stored in PLACE and makes it the new value of PLACE.
Returns the car of the old value in PLACE.")

(docfun position function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that is equal to ITEM in
the sense of TEST.  Returns NIL if no such element exists.")

(docfun position-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that satisfies TEST.
Returns NIL if no such element exists.")

(docfun position-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that does not satisfy TEST.
Returns NIL if no such element exists.")

(docfun pprint function (object &optional (stream *standard-output*)) "
Pretty-prints OBJECT.  Returns no values.  Equivalent to
	(PROGN (WRITE OBJECT :STREAM STREAM :PRETTY T :ESCAPE T)
	       (VALUES))
The SYS:PRETTY-PRINT-FORMAT property N (which must be a non-negative integer)
of a symbol SYMBOL controls the pretty-printing of form
	(SYMBOL f1 ... fN fN+1 ... fM)
in such a way that the subforms fN+1, ..., fM are regarded as the 'body' of
the entire form.  For instance, the property value of 2 is initially given to
the symbol DO.")

(docfun prin1 function (object &optional (stream *standard-output*)) "
Prints OBJECT in the way that the output can be reread later if possible.
Returns OBJECT.  Equivalent to (WRITE OBJECT :STREAM STREAM :ESCAPE T).")

(docfun prin1-to-string function (object) "
PRIN1s OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE T).")

(docfun princ function (object &optional (stream *standard-output*)) "
Prints OBJECT without escape characters.  Returns OBJECT.  Equivalent to
(WRITE OBJECT :STREAM STREAM :ESCAPE NIL).")

(docfun princ-to-string function (object) "
PRINCs OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE NIL).")

(docfun print function (object &optional (stream *standard-output*)) "
Outputs a newline character, and then PRIN1s OBJECT.  Returns OBJECT.
Equivalent to
	(PROGN (TERPRI STREAM)
	       (WRITE OBJECT :STREAM STREAM :ESCAPE T))")

(docfun probe-file function (filespec) "
Returns the full pathname of the specified file if it exists.  Returns NIL
otherwise.  FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun proclaim function (decl-spec) "
Gives a global declaration.  See DECLARE for possible DECL-SPECs.")

(docfun proclamation function (decl-spec) "
ECL specific.
Returns T if the specified declaration is globally in effect; NIL otherwise.
See DECLARE for possible DECL-SPECs.")

(docfun prog macro
	"(prog ({var | (var [init])}*) {decl}* {tag | statement}*)" "
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) in parallel, and executes STATEMENTs.  Returns NIL.")

(docfun prog* macro
	"(prog* ({var | (var [init])}*) {decl}* {tag | statement}*)" "
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) sequentially, and executes STATEMENTs.  Returns NIL.")

(docfun prog1 macro "(prog1 first-form {form}*)" "
Evaluates FIRST-FORM and FORMs in order.  Returns the value of FIRST-FORM.")

(docfun prog2 macro "(prog2 first-form second-form {forms}*)" "
Evaluates FIRST-FORM, SECOND-FORM, and FORMs in order.  Returns the value of
SECOND-FORM.")

(docfun progn special "(progn {form}*)" "
Evaluates FORMs in order, and returns all values of the last FORM.  Returns
NIL if no FORM is given.")

(docfun progv special "(progv symbols-form values-form {form}*)" "
Evaluates SYMBOLS-FORM and VALUES-FORM.  The value of SYMBOLS-FORM must be a
list of symbols (S1 ... Sn) and the value of VALUES-FORM must be a list
(V1 ... Vm).  Binds each Si to Vi or to NIL if i > m.  Then evaluates FORMs
and returns all values of the last FORM.  Returns NIL if no FORM is given.")

(docfun provide function (module-name) "
Declares the start of a program module.  Usually placed at the end of a
program file.  MODULE-NAME may be a string or a symbol.  If it is a string, it
is pushed onto *MODULES*.  If it is a symbol, its print name is pushed.  See
REQUIRE.")

(docfun psetf macro "(psetf {place form}*)" "
Similar to SETF, but evaluates all FORMs first, and then assigns each value to
the corresponding PLACE.  Returns NIL.")

(docfun psetq macro "(psetq {var form}*)" "
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL.")

(docfun push macro "(push form place)" "
Evaluates FORM, conses the value of FORM to the value stored in PLACE, and
makes it the new value of PLACE.  Returns the new value of PLACE.")

(docfun pushnew macro "(pushnew form place {keyword-form value-form}*)" "
Evaluates FORM first.  If the value is already in the list stored in PLACE,
does nothing.  Else, conses the value onto the list and makes the result the
new value of PLACE.  Returns NIL.  KEYWORD-FORMs and VALUE-FORMs are used to
check if the value of FORM is already in PLACE as if their values are passed
to MEMBER.")

(docfun quote special "(quote x) | 'x" "
Simply returns X without evaluating it.")

(docfun random function (number &optional (random-state *random-state*)) "
Creates and returns a random number by using RANDOM-STATE.  NUMBER must be
either a positive integer or a positive float.  If NUMBER is a positive
integer, returns a positive integer less than NUMBER.  If NUMBER is a positive
float, returns a positive float less than NUMBER in the same float format as
NUMBER.")

(doctype random-state "
A random-state object stores information used to generate random numbers.  A
random-state is notated as '#$' followed by a certain number.")

(docfun random-state-p function (x) "
Returns T if X is a random-state object; NIL otherwise.")

(docfun rassoc function (item alist &key (test '#'eql) test-not (key '#'identity)) "
Returns the first pair in ALIST whose cdr is equal (in the sense of TEST) to
ITEM.  Returns NIL if no such pair exists.
The function KEY is applied to extract the key for comparison.")

(docfun rassoc-if function (test alist) "
Returns the first pair in ALIST whose cdr satisfies TEST.  Returns NIL if no
such pair exists.")

(docfun rassoc-if-not function (test alist) "
Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL
if no such pair exists.")

(doctype ratio "
A ratio is notated by its numerator and denominator, separated by a slash '/'.
Normally, a ratio is notated in radix 10 (see *PRINT-BASE* and *READ-BASE*) as
	[sign] {digit}+ / {digit}+
where DIGIT is a decimal digit ('0', ..., '9') and SIGN is either '+' or '-'.
Also, the following syntax is used to notate the radix explicitly.
	# radix {r | R} [sign] {digit}+ / {digit}+
where RADIX is one of '2', '3', ..., '36' and DIGIT is a digit in radix RADIX:
	Digits in radix 2 are '0' and '1'
	Digits in radix 8 are '0', ..., '7'
	Digits in radix 16 are '0', ..., '9', 'a', ..., 'f', and 'A', ..., 'F'
The following syntax is also available for radix 2, 8, 10, and 16.
	# {b | B} [sign] {digit}+ / {digit}+
	# {o | O} [sign] {digit}+ / {digit}+
	# {x | X} [sign] {digit}+ / {digit}+")

(doctype rational "
A ratio is either an integer or a ratio.")

(docfun rational function (number) "
Converts NUMBER into rational accurately and returns the result.")

(docfun rationalize function (number) "
Converts NUMBER into rational approximately and returns the result.")

(docfun rationalp function (x) "
Returns T if X is an integer or a ratio; NIL otherwise.")

(docfun read function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursivep nil)) "
Reads an object from STREAM and returns the object.")

(docfun read-byte function (stream &optional (eof-error-p t) (eof-value nil)) "
Reads one byte from STREAM and returns it as an integer.")

(docfun sys:read-bytes function (stream string start end) "
ECL specific.
Reads from STREAM a series of bytes to be placed into STRING
starting from position START up to END (exclusive).
It returns the number of bytes actually transferred, or -1 if the
operation failed.
The stream is automatically flushed.")

(docfun read-char function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads a character from STREAM and returns it.")

(docfun read-char-no-hang function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Returns the next character from STREAM if one is available; NIL otherwise.")

(docfun read-delimited-list function (char &optional (stream *standard-input*) (recursive-p nil)) "
Reads objects from STREAM until the next character after an object's
representation is CHAR.  Returns all objects read, as a list.")

(docfun read-from-string function (string &optional (eof-error-p t) (eof-value nil)
              &key (start 0) (end (length string)) (preserve-whitespace nil)) "
Reads an object from STRING and returns the object.  As the second value,
returns the index to the character next to the object's representation.
PRESERVE-WHITESPACE specifies whether to leave the character next to the
object's representation.")

(docfun read-line function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads a line of characters from STREAM and returns them as a string.  The
newline character at the end of the line will be discarded.")

(docfun read-preserving-whitespace function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads an object from STREAM and returns the object.  Unlike READ, always
leaves the character next to the object's representation.")

(doctype readtable "
A readtable defines the syntax used to read objects.
Each readtable object remembers the syntactic class of each character.  The
following syntactic classes are supported.  The characters in parenthesis
below are those standard characters that belong to each syntactic class as
defined in the standard readtable.
	white-space (space and newline)
	single-escape ( \\ )
	multiple-escape ( | )
	macro-character ( \"  #  '  (  )  ,  ;  ` )
	constituent (the others)
For each macro-character, the readtable remembers the definition of the
associated read macro and the non-terminating-p flag.  In the standard
readtable, only single-quote is non-terminating.  Dispatch macro characters
are classified to macro-characters.  A readtable is notated as
	#<readtable n>
where N is actually a number that identifies the readtable.")

(docfun readtablep function (x) "
Returns T if X is a readtable object; NIL otherwise.")

(docfun realpart function (number) "
Returns the realpart of NUMBER if it is a complex.  Otherwise, returns NUMBER.")

(docfun reduce function (function sequence
       &key (from-end nil) (start 0) (end (length sequence)) initial-value) "
Combines all the elements of SEQUENCE using the binary operation FUNCTION.")

(docfun rem function (number divisor) "
Returns the second value of (TRUNCATE NUMBER DIVISOR), i.e. the value of
	(- NUMBER (* (TRUNCATE NUMBER DIVISOR) DIVISOR))")

(docfun remf macro "(remf place form)" "
Removes the property specified by FORM from the property list stored in PLACE.
Returns T if the property list had the specified property; NIL otherwise.")

(docfun remhash function (key hash-table) "
Removes the entry for KEY in HASH-TABLE.  Returns T if such an entry existed;
NIL otherwise.")

(docfun remove function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with those elements equal to ITEM (in the sense of
TEST) removed.")

(docfun remove-duplicates function (sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns a copy of SEQUENCE without duplicated elements.")

(docfun remove-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with elements satisfying TEST removed.")

(docfun remove-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with elements not satisfying TEST removed.")

(docfun remprop function (symbol indicator) "
Removes the specified property from the property list associated with SYMBOL.
Returns T if the property list had the specified property; NIL otherwise.")

(docfun rename-file function (filespec new-filespec) "
Renames the file specified by FILESPEC as specified by NEW-FILESPEC.  Returns
as three values the new pathname, the old full pathname, and the new full
pathname.  FILESPEC and NEW-FILESPEC may be a symbol, a string, a pathname, or
a file stream.")

(docfun rename-package function (package new-name &optional (new-nicknames nil)) "
Renames PACKAGE to NEW-NAME and replaces the nicknames with NEW-NICKNAMES.
See MAKE-PACKAGE.")

(docfun replace function (sequence1 sequence2
       &key (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))) "
Replaces elements of SEQUENCE1 with the corresponding elements of SEQUENCE2.
SEQUENCE1 may be destroyed and is returned.")

(docfun require function (module-name &optional pathname) "
If the specified module name is not found in *MODULES*, then loads the files
specified by PATHNAME.  Otherwise, does nothing.  MODULE-NAME may be a string
or a symbol.  If it is a symbol, the print name of the symbol is used as the
module name.  PATHNAME may be a pathname object or it may be a list of
pathname objects.  If PATHNAME is not given, then ECL tries to load the file
whose file name is MODULE-NAME and whose filetype is either .FASL, .LSP, or
none.  See PROVIDE.")

(docfun sys:reset-gc-count function () "
ECL specific.
Resets the counter of the garbage collector that records how many times the
garbage collector has been called for each implementation type.")

(docfun sys:reset-stack-limits function () "
ECL specific.
Resets the stack limits to the normal state.  When a stack has overflowed, ECL
extends the limit for the stack in order to execute the error handler.  After
processing the error, ECL resets the stack limit with this function.")

(docfun rest function (x) "
Equivalent to CDR.")

(docfun return macro "(return [result])" "
Terminates execution of the lexically surrounding NIL block and returns all
values of RESULT (which defaults to NIL) as the values of the terminated
block.")

(docfun return-from special "(return-from symbol [result])" "
Terminates execution of the lexically surrounding block named SYMBOL and
returns all values of RESULT (which defaults to NIL) as the values of the
terminated block.")

(docfun revappend function (x y) "
Equivalent to (APPEND (REVERSE X) Y)")

(docfun reverse function (sequence) "
Returns a new sequence containing the same elements as SEQUENCE but in the
reverse order.")

(docfun room function (&optional (x t)) "
Displays information about storage allocation in the following format.
	* for each type class
		* number of pages so-far allocated for the type class
		* maximum number of pages for the type class
		* percentage of used cells to cells so-far allocated
		* number of times the garbage collector has been called to
		  collect cells of the type class
		* implementation types that belongs to the type class
	* number of pages actually allocated for contiguous blocks
	* maximum number of pages for contiguous blocks
	* number of times the garbage collector has been called to collect
	  contiguous blocks
	* number of pages in the hole
	* maximum number of pages for relocatable blocks
	* number of times the garbage collector has been called to collect
	  relocatable blocks
	* total number of pages allocated for cells
	* total number of pages allocated
	* number of available pages
	* number of pages ECL can use.
The number of times the garbage collector has been called is not shown, if the
number is zero.  The optional X is simply ignored.")

(docfun rotatef macro "(rotatef {place}*)" "
Saves the values of PLACEs, and then assigns to each PLACE the saved value of
the PLACE to its right.  The rightmost PLACE gets the value of the leftmost
PLACE.  Returns NIL.")

(docfun round function (number &optional (divisor 1)) "
Returns the integer nearest to NUMBER/DIVISOR.  Returns the value of (- NUMBER
(* first-value DIVISOR)) as the second value.")

(docfun rplaca function (cons x) "
Replaces the car of CONS with X, and returns the modified CONS.")

(docfun rplacd function (cons x) "
Replaces the cdr of CONS with X, and returns the modified CONS.")

(docfun save function (filespec) "
ECL specific.
Saves the current ECL core image into a program file specified by PATHNAME.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  This
function depends on the version of ECL.  See ECL Report for details.")

(docfun sys:save-system function (pathname) "
ECL specific.
Saves the current ECL core image into the specified program file.  This
function differs from SAVE in that the contiguous and relocatable areas are
made permanent in the saved image.  Usually the standard image of ECL
interpreter/compiler is saved by this function.")

(docfun sbit function (simple-bit-array &rest subscripts) "
Returns the specified bit in SIMPLE-BIT-ARRAY.")

(docfun scale-float function (float integer) "
Returns the value of (* FLOAT (expt (float-radix FLOAT) INTEGER)).")

(docfun schar function (simple-string n) "
Returns the character object representing the N-th character in SIMPLE-STRING.
This is faster than CHAR.")

(docfun search function (sequence1 sequence2
       &key (key '#'identity) (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil)) "
Searches SEQUENCE2 for a subsequence that element-wise matches SEQUENCE1.
Returns the index to the first element of the subsequence if such a
subsequence is found.  Returns NIL otherwise.")

(docfun second function (x) "
Equivalent to CADR.")

(doctype sequence "
A sequence is either a list or a vector.")

(docfun set function (symbol object) "
Assigns OBJECT to the global variable named SYMBOL.  Returns OBJECT.")

(docfun set-char-bit function (char bit-name flag) "
Returns a character with the same code and attributes as CHAR except the
bit specified by BIT-NAME is on (if FLAG is non-NIL) or off. In ECL, the
bit-attributes handled are :control :meta :super and :hyper")


(docfun set-difference function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Returns, as a list, those elements of LIST1 that are not elements of LIST2.")

(docfun set-dispatch-macro-character function (char subchar function &optional (readtable *readtable*)) "
Replaces FUNCTION for the read macro of SUBCHAR associated with the dispatch
macro character CHAR in READTABLE.  When the ECL reader reads an object that
begins with CHAR followed by SUBCHAR, it calls FUNCTION with the input stream,
SUBCHAR, and NIL as arguments.  When the ECL reader reads an object that
begins with CHAR, followed by a decimal representation of a number N, followed
by SUB-CHAR, it calls FUNCTION with N as the third argument.  In both cases,
if FUNCTION returns a single value, then that value is returned as the value
of the reader.  If FUNCTION returns no value, then the reader tries to read an
object again.  See MAKE-DISPATCH-MACRO-CHARACTER and GET-DISPATCH-MACRO-
CHARACTER.")

(docfun set-exclusive-or function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Returns, as a list, those elements of LIST1 that are not elements of LIST2 and
those elements of LIST2 that are not elements of LIST1.")

(docfun sys:set-hole-size function (fixnum) "
ECL specific.
Sets the size of the memory hole (in pages).")

(docfun set-macro-character function (char function
       &optional (non-terminating-p nil) (readtable *readtable*)) "
Registers CHAR as a macro character in READTABLE and makes FUNCTION the read
macro associated with CHAR.  When the ECL reader reads an object that begins
with CHAR, it calls FUNCTION with the input stream and CHAR as arguments.  If
FUNCTION returns a single value, it is returned as the value of the reader.
If FUNCTION returns no value, then the reader tries to read an object again.
NON-TERMINATING-P specifies whether CHAR is non-terminating or not (see
READTABLE).
Use GET-MACRO-CHARACTER to get the read macro associated with a character.")

(docfun set-syntax-from-char function (to-char from-char
       &optional (to-readtable *readtable*) (from-readtable nil)) "
Replaces the information for TO-CHAR in TO-READTABLE with the information for
FROM-CHAR in FROM-READTABLE.  If FROM-READTABLE is NIL, then the standard
readtable is used.  TO-CHAR belongs to the same syntactic class as FROM-CHAR,
and if FROM-CHAR is a macro character, TO-CHAR inherits the read macro and
non-terminating-p flag of FROM-CHAR.  See READTABLE.")

(docfun setf macro "(setf {place form}*)" "
Evaluates each FORM and assigns the value to the corresponding PLACE in order.
Returns the value of the last FORM.
Each PLACE may be any one of the following:
  * A symbol that names a variable.
  * A function call form whose first element is the name of the following
    functions:
	nth	elt	subseq	rest	first ... tenth
	c?r	c??r	c???r	c????r
	aref	svref	char	schar	bit	sbit	fill-pointer
	get	getf	documentation	symbol-value	symbol-function
	symbol-plist	macro-function	gethash
	char-bit	ldb	mask-field
	apply	slot-value
    where '?' stands for either 'a' or 'd'.
  * A function call form whose first element is:
        1. an access function for a structure slot
        1. an accessor method for a CLOS object
  * the form (THE type place) with PLACE being a place recognized by SETF.
  * a macro call which expands to a place recognized by SETF.
  * any form for which a DEFSETF or DEFINE-SETF-METHOD declaration has been
    made.")

(docfun setq special "(setq {var form}*)" "
Evaluates each FORM and assigns the value to VAR in order.  Returns the value
of the last FORM.")

(docfun seventh function (x) "
Equivalent to (CADDR (CDDDDR X)).")

(docfun shadow function (symbol &optional (package *package*)) "
If no symbol is registered in PACKAGE with the same name as SYMBOL, then
creates an internal symbol with the same name and registers it into PACKAGE.
The created symbol shadows external symbols of the same name in those packages
that PACKAGE uses.  SYMBOL may be a list of symbols.")

(docfun shadowing-import function (symbol &optional (package *package*)) "
Registers SYMBOL as an internal symbol of PACKAGE.  Does nothing if SYMBOL is
already registered in PACKAGE.  If there exists already a symbol in PACKAGE
with the same name, then uninterns the symbol first.  SYMBOL shadows external
symbols of the same name in those packages that PACKAGE uses.  SYMBOL may be a
list of symbols.")

(docfun shiftf macro "(shiftf {place}+ form)" "
Saves the values of PLACE and FORM, and then assigns the value of each PLACE
to the PLACE on its left.  The rightmost PLACE gets the value of FORM.
Returns the original value of the leftmost PLACE.")

(doctype short-float "
A short-float is a short-precision floating point number.")

(docvar short-float-epsilon constant "
The smallest positive short-float E that satisfies
	(not (= (float 1 e) (+ (float 1 e) e))).")

(docvar short-float-negative-epsilon constant "
The smallest positive short-float E that satisfies
	(not (= (float 1 e) (- (float 1 e) e))).")

(docfun short-site-name function () "
Returns, as a string, the location of the machine on which ECL runs.")

(doctype signed-byte "
As a type specifier, (SIGNED-BYTE n) specifies those integers that can be
represented with N bits in 2's complement representation.")

(docfun signum function (number) "
Returns a number that represents the sign of NUMBER.  Returns NUMBER If it is
zero.  Otherwise, returns the value of
	(/ NUMBER (ABS NUMBER))")

(doctype simple-array "
A simple-array is an array that is not displaced to another array, has no
fill-pointer, and is not adjustable.")

(doctype simple-bit-vector "
A simple-bit-vector is a bit-vector that is not displaced to another array,
has no fill-pointer, and is not adjustable.")

(docfun simple-bit-vector-p function (x) "
Returns T if X is a simple-bit-vector; NIL otherwise.")

(doctype simple-string "
A simple-string is a string that is not displaced to another array, has no
fill-pointer, and is not adjustable.")

(docfun simple-string-p function (x) "
Returns T if X is a simple-string; NIL otherwise.")

(doctype simple-vector "
A simple-vector is a vector that is not displaced to another array, has no
fill-pointer, and is not adjustable.")

(docfun simple-vector-p function (x) "
Returns T if X is a simple-vector; NIL otherwise.")

(docfun sin function (radians) "
Returns the sine of RADIANS.")

(doctype single-float "
A single-float is a single-precision floating point number.
SINGLE-FLOAT as a type specifier is equivalent to LONG-FLOAT in ECL.")

(docvar single-float-epsilon constant "
Same as LONG-FLOAT-EPSILON.")

(docvar single-float-negative-epsilon constant "
Same as LONG-FLOAT-NEGATIVE-EPSILON.")

(docfun sinh function (number) "
Returns the hyperbolic sine of NUMBER.")

(docfun sixth function (x) "
Equivalent to (CADR (CDDDDR X)).")

(docfun sleep function (n) "
Suspends execution for N seconds.  N may be any non-negative, non-complex
number.")

#+clos
(docfun sys:sl-boundp function (object) "
ECL/CLOS specific.
Returns nil if the OBJECT is not null.")

#+clos
(docfun sys:sl-makunbound function (instance index) "
ECL/CLOS specific.
Removes the value associated with the INDEX-th slot of INSTANCE.")

(docfun software-type function () "
Returns, as a string, the type of the software under which ECL runs.")

(docfun software-version function () "
Returns, as a string, the version of the software under which ECL runs.")

(docfun some function (predicate sequence &rest more-sequences) "
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise.")

(docfun sort function (sequence test &key (key '#'identity)) "
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  The order of two
elements X and Y is arbitrary if both
	(FUNCALL TEST X Y)
	(FUNCALL TEST Y X)
evaluates to NIL.  See STABLE-SORT.")

(docfun special-form-p function (symbol) "
Returns T if SYMBOL names a special form; NIL otherwise.
The special forms defined in Common Lisp are:
	block		if			progv
	catch		labels			quote
	compiler-let	let			return-from
	declare		let*			setq
	eval-when	macrolet		tagbody
	flet		multiple-value-call	the
	function	multiple-value-prog1	throw
	go		progn			unwind-protect
In addition, ECL implements the following macros as special forms, though of
course macro-expanding functions such as MACROEXPAND work correctly for these
macros.
	and		incf			prog1
	case		locally			prog2
	cond		loop			psetq
	decf		multiple-value-bind	push
	defmacro	multiple-value-list	return
	defun		multiple-value-set	setf
	do		or			unless
	do*		pop			when
	dolist		prog
	dotimes		prog*")

(docfun sys:specialp function (symbol) "
ECL specific.
Returns T if the SYMBOL names a globally special variable; NIL otherwise.")

(docfun sqrt function (number) "
Returns the square root of the arg.")

(docfun stable-sort function (sequence test &key (key '#'identity)) "
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  For two elements
X and Y, if both
	(FUNCALL TEST X Y)
	(FUNCALL TEST Y X)
evaluates to NIL, then the order of X and Y are the same as in the original
SEQUENCE.  See SORT.")

(doctype standard-char "
A standard-char is a space character (#\\Space), a newline character
(#\\Newline,) or a character that represents one of the following letters.
	!  \"  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4
	5  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H
	I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \\
	]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p
	q  r  s  t  u  v  w  x  y  z  {  |  }  ~~")

(docfun standard-char-p function (char) "
Returns T if CHAR is a standard-char; NIL otherwise.")

(docfun step macro "(step form)" "
Evaluates FORM in the Stepper mode and returns all its values.  See ECL Report
for Stepper mode commands.")

(doctype stream "
A stream is a source of input or a destination of output.  The following kinds
of streams are supported.
	file streams
	string-input streams
	string-output streams
	two-way streams
	echo streams
	synonym streams
	concatenated streams
	broadcast streams
Basically, file streams are created by OPEN and other kinds of streams are
created by MAKE-...-STREAM.  See these functions.")

(docfun stream-element-type function (stream) "
Returns the type specifier for the io unit of STREAM.")

(docfun streamp function (x) "
Returns T if X is a stream object; NIL otherwise.")

;;; CLOS Streams ------------------------------------------------------------

#+CLOS
(docfun stream-read-char method ((obj stream-class)) "
Reads the next character object from the CLOS stream OBJ.")

#+CLOS
(docfun stream-read-line method ((obj stream-class) &rest make-array-options) "
Reads character objects from the CLOS stream OBJ, up to and including the
next newline character, and returns them as a string (without the newline).
If given, the MAKE-ARRAY-OPTIONS arguments are passed to make-array
when the returned string is created.")

#+CLOS
(docfun stream-unread-char method ((obj stream-class) character) "
Unreads the character object.
CHARACTER will be the next character read by STREAM-READ-CHAR .")

#+CLOS
(docfun stream-peek-char method ((obj stream-class) peek-type) "
Returns the character object which would be returned by STREAM-READ-CHAR
but does not remove it from the input buffer.
If PEEK-TYPE is T, stream-peek-char skips over any whitespace characters,
removing them from the input buffer, and returns the next character.")

#+CLOS
(docfun stream-listen method ((obj stream-class)) "
Returns NIL if no character is immediately available from the CLOS stream.
Otherwise, the next character is returned, as if stream-peek-char
had been called.")

#+CLOS
(docfun stream-clear-input method ((obj stream-class)) "
Clears any buffered characters received from the CLOS stream OBJ.
Returns NIL.")

#+CLOS
(docfun stream-write-char method ((obj stream-class) character) "
Outputs the CHARACTER to the CLOS stream OBJ and returns the CHARACTER.")

#+CLOS
(docfun stream-write-string method ((obj stream-class) string &optional start end) "
Outputs characters in the STRING to the CLOS stream OBJ and returns the
STRING. The START and END arguments, if given, indicate a substring that
is to be output.")

#+CLOS
(docfun stream-fresh-line method ((obj stream-class)) "
Outputs a newline to the CLOS stream if and only if the CLOS stream OBJ
is not already at the beginning of a new line. Returns non-NIL if a
newline was output and NIL otherwise.")

#+CLOS
(docfun stream-clear-output method ((obj stream-class)) "
Aborts any outstanding output operation on the CLOS stream OBJ
and returns NIL .")

#+CLOS
(docfun stream-force-output method ((obj stream-class)) "
Initiates the emptying of internal buffers on the CLOS stream OBJ
and returns NIL.")

;;; end of CLOS streams ---------------------------------------------------

(doctype string "
A string is a vector of characters.  A string is notated by surrounding the
characters with double quotes.  Some strings may be displaced to another
string, may have a fill-pointer, or may be adjustable.  Other strings are
called simple-strings.")

(docfun string function (x) "
Coerces X into a string.  If X is a string, then returns X itself.  If X is a
symbol, then returns its print name.  If X is a character, then returns a one
element string containing that character.  Signals an error if X cannot be
coerced into a string.")

(docfun string-capitalize function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with the first character of each word converted to
upper case, and remaining characters converted to lower case.  Its destructive
version is NSTRING-CAPITALIZE.")

(doctype string-char "
A string-char is a character that can be stored in strings.  In ECL, every
character is a string-character.")

(docfun string-char-p function (char) "
Returns T if CHAR is a string-char, i.e. can be stored in strings; NIL
otherwise.  In ECL, this function always returns T.")

(docfun sys:string-concatenate function (&rest strings) "
ECL specific.
Concatenates STRINGs and returns the result.")

(docfun string-downcase function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with all upper case characters converted to lower
case.  Its destructive version is NSTRING-DOWNCASE.")

(docfun string-equal function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns T if STRING1 and STRING2 are character-wise CHAR-EQUAL; NIL otherwise.")

(docfun string-greaterp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING>, but ignores cases.")

(docfun string-left-trim function (char-bag string) "
Returns a copy of STRING with the specified characters removed from the left
end.  CHAR-SPEC must be a sequence of characters.")

(docfun string-lessp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING<, but ignores cases.")

(docfun string-not-equal function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns NIL if the strings are character-wise CHAR-EQUAL.  Otherwise, returns
the number of characters in the longest common prefix of the strings.")

(docfun string-not-greaterp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING<=, but ignores cases.")

(docfun string-not-lessp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING>=, but ignores cases.")

(docfun string-right-trim function (char-bag string) "
Returns a copy of STRING with the specified characters removed from the right
end.  CHAR-SPEC must be a sequence of characters.")

(docfun sys:string-to-object function (string) "
ECL specific.
Equivalent to (READ-FROM-STRING STRING), but is much faster.")

(docfun string-trim function (char-spec string) "
Returns a copy of STRING with the specified characters removed from both ends.
CHAR-SPEC must be a sequence of characters.")

(docfun string-upcase function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with all lower case characters converted to upper
cases.  Its destructive version is NSTRING-UPCASE.")

(docfun string/= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns NIL if the strings are character-wise CHAR=.  Otherwise, returns the
number of characters in the longest common prefix of the strings.")

(docfun string< function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes before STRING2 in lexicographic order, then returns the
number of characters in the longest common prefix of the strings.  Otherwise,
returns NIL.")

(docfun string<= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes before STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun string= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns T if STRING1 and STRING2 are character-wise CHAR=; NIL otherwise.")

(docfun string> function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes after STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun string>= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes after STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun stringp function (x) "
Returns T if X is a string object; NIL otherwise.")

(docfun sys:structurep function (x) "
ECL specific.
Returns T if X is a structure object defined by DEFSTRUCT; NIL otherwise.")

(docfun sublis function (alist tree &key (key '#'identity) (test '#'eql) test-not) "
Substitutes subtrees of TREE by using ALIST and returns the result.  The
original TREE is not destroyed.")

(docfun subseq function (sequence start &optional (end (length sequence))) "
Returns a copy of the subsequence of SEQUENCE between START (inclusive) and
END (exclusive).")

(docfun subsetp function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Returns T if every element of LIST1 is also an element of LIST2.  Returns NIL
otherwise.")

(docfun subst function (new old tree &key (key '#'identity) (test '#'eql) test-not) "
Substitutes NEW for subtrees of TREE that match OLD and returns the result.
The original TREE is not destroyed.")

(docfun subst-if function (new test tree &key (key '#'identity)) "
Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.
The original TREE is not destroyed.")

(docfun subst-if-not function (new test tree &key (key '#'identity)) "
Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the
result.  The original TREE is not destroyed.")

(docfun substitute function (new old sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that match OLD replaced by NEW.
The original SEQUENCE is not destroyed.")

(docfun substitute-if function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that satisfy TEST replaced by
NEW.  The original SEQUENCE is not destroyed.")

(docfun substitute-if-not function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that do not satisfy TEST replaced
by NEW.  The original SEQUENCE is not destroyed.")

(docfun subtypep function (type1 type2) "
Returns T if TYPE1 is a subtype of TYPE2; NIL otherwise.  If this is not
determined, then returns NIL as the first and second values.  Otherwise, the
second value is T.")

(docfun svref function (simple-vector n) "
Returns the N-th element of SIMPLE-VECTOR.")

(docfun sxhash function (object) "
Returns the hash code for OBJECT as an integer.")

(doctype symbol "
Symbol objects.")

(docfun symbol-function function (symbol) "
Returns the global function definition named SYMBOL.")

(docfun symbol-name function (symbol) "
Returns the print name of SYMBOL.")

(docfun symbol-package function (symbol) "
Returns the home package of SYMBOL.  Returns NIL if SYMBOL is not interned.")

(docfun symbol-plist function (symbol) "
Returns the property list of SYMBOL.")

(docfun symbol-value function (symbol) "
Returns the value of the global variable named SYMBOL.")

(docfun symbolp function (x) "
Returns T if X is a symbol; NIL otherwise.")

#+unix
(docfun system function (string) "
ECL/UNIX specific.
Executes a Shell command as if STRING is an input to the Shell.")

(doctype t "
The type T is a supertype of every type.  Every object belongs to this type.")

(docvar t constant "
The value of T is T.")

(docfun tagbody special "(tagbody {tag | statement}*)" "
Executes STATEMENTs in order and returns NIL after the execution of the last
STATEMENT.  But, if a GO form causes a jump to one of the TAGs, then execution
continues at the point right after the TAG.  Lists are regarded as STATEMENTs
and other objects are regarded as TAGs.")

(docfun tailp function (x list) "
Returns T if X is identical to one of the conses that constitute LIST.
Returns NIL otherwise.")

(docfun tan function (radians) "
Returns the tangent of RADIANS.")

(docfun tanh function (number) "
Returns the hyperbolic tangent of NUMBER.")

(docfun tenth function (x) "
Equivalent to (CADR (CDDDDR (CDDDDR X))).")

(docfun terpri function (&optional (stream *standard-output*)) "
Outputs a newline character.")

(docfun the special "(the type form)" "
Declares that FORM evaluates to a value of TYPE.  Evaluates FORM and checks if
the value belongs to TYPE.  If it does, returns the value.  Otherwise, signals
an error.")

(docfun third function (x) "
Equivalent to CADDR.")

(docfun throw special "(throw tag form)" "
Evaluates TAG and aborts the execution of the most recent CATCH form that
establishes a catcher with the same catch tag.  Returns all values of FORM as
the values of the CATCH form.")

(docfun time macro "(time form)" "
Evaluates FORM, outputs the realtime and runtime used for the evaluation to
*TRACE-OUTPUT*, and then returns all values of FORM.")

(docfun sys:top-level function () "
ECL specific.
The top-level loop of ECL.
When ECL is invoked, it evaluates (FUNCALL 'SYS:TOP-LEVEL).  To change the top-
level of ECL, redefine SYS:TOP-LEVEL and save the core image into a program
file.  When the saved image is invoked, it will start the redefined top-level.")

(docfun trace macro
	"(trace ({function-name | ({function-name}+)} {keyword [form]\}*)" "
Begins tracing the specified functions.  With no FUNCTION-NAMEs, returns a
list of functions currently being traced. The printed information consists of
the name of function followed at entry by its arguments and on exit by its
return values.
The keywords allow to control when and how tracing is performed.
The possible keywords are:

 :COND-BEFORE	information is printed upon entry if form evaluates to non-nil
 :COND-AFTER	information is printed upon exit if form evaluates to non-nil
 :COND		specifies a single condition for both entry and exit
 :BREAK		a breakpoint is entered after printing the entry trace
		information, but before applying the traced function to its
		arguments, if form evaluates to non-nil
 :BREAK-AFTER 	like :BREAK but the breakpoint is entered after the function
		has been executed and the exit trace information has been
		printed and before control returns
 :PRINT		prints the values of the forms in the list upon entry.
		They are preceeded by a backslash (\\)
 :PRINT-AFTER	prints the values of the forms in the list upon exit from the
		function. They are preceeded by a backslash (\\)
 :STEP		turns on the stepping facility

Forms can refer to the list of arguments of the function through the variable
SYS::ARGS.")

(docfun tree-equal function (x y &key (test '#'eql) test-not) "
Returns T if X and Y have the same tree structures and corresponding leaves
are all the same in the sense of TEST.  Returns NIL otherwise.")

(docfun truename function (filespec) "
Returns the full pathname of the file specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun truncate function (number &optional (divisor 1)) "
Returns the integer obtained by truncating NUMBER/DIVISOR.  Returns the value
of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun type-of function (x) "
Returns a type specifier of the type to which X belongs.")

(docfun typecase macro "(typecase keyform {(type {form}*)}*)" "
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, simply returns NIL.  The symbols T and OTHERWISE may
be used as a TYPE to specify the default case.")

(docfun typep function (x type) "
Returns T if X belongs to TYPE; NIL otherwise.")

#+unix
(docfun sys:uncatch-bad-signals function () "
ECL/UNIX specific.
Undoes the effect of SYS:CATCH-BAD-SIGNALS.")

(docfun unexport function (symbol &optional (package *package*)) "
Undoes the registration of SYMBOL as an external symbol of PACKAGE and makes
SYMBOL internal to PACKAGE.  SYMBOL may be a list of symbols.")

(docfun unintern function (symbol &optional (package *package*)) "
Removes SYMBOL from PACKAGE.  If PACKAGE is the home package of SYMBOL, then
makes SYMBOL uninterned.  Returns T if SYMBOL is actually registered in
PACKAGE; NIL otherwise.")

(docfun union function (list1 list2 &key (key '#'identity) (test '#'eql) test-not) "
Returns, as a list, the union of elements in LIST1 and in LIST2.")

(docfun sys:universal-error-handler function (error-name continuable-p function-name
       continue-format-string error-format-string
       &rest args) "
ECL specific.
Starts the error handler of ECL.
When an error is detected, ECL calls this function with the specified
arguments.  To change the error handler of ECL, redefine this function.
ERROR-NAME is the name of the error.  CONTINUABLE-P is T for a continuable
error and NIL for a fatal error.  FUNCTION-NAME is the name of the function
that caused the error.  CONTINUE-FORMAT-STRING and ERROR-FORMAT-STRING are the
format strings of the error message.  ARGS are the arguments to the format
strings.")

(docfun unless macro "(unless test {form}*)" "
If TEST evaluates to NIL, then evaluates FORMs and returns all values of the
last FORM.  If not, simply returns NIL.")

(docfun unread-char function (char &optional (stream *standard-input*)) "
Puts CHAR back on the front of the input stream STREAM.")

(doctype unsigned-byte "
As a type specifier, (UNSIGNED-BYTE n) specifies non-negative integers that
can be represented with N bits.")

(docfun untrace macro "(untrace {function-name}*)" "
Ends tracing the specified functions.  With no FUNCTION-NAMEs, ends tracing
all functions.")

(docfun unuse-package function (package-spec &optional (package *package*)) "
Causes PACKAGE not to use packages specified by PACKAGE-SPEC.  PACKAGE-SPEC
may be a package object, a string, a symbol, or a list consisting of package
objects, strings, and, symbols.")

(docfun unwind-protect special "(unwind-protect form {cleanup-form}*)" "
Evaluates FORM and returns all its values.  Before returning, evaluates
CLEANUP-FORMs in order, whether FORM returns normally or abnormally by a non-
local exit.")

(docfun upper-case-p function (char) "
Returns T if CHAR is an upper-case character; NIL otherwise.")

(docfun use-package function (package-spec &optional (package *package*)) "
Causes PACKAGE to use packages specified by PACKAGE-SPEC, in addition to those
packages that PACKAGE already uses.  PACKAGE-SPEC may be a package object, a
string, a symbol, or a list consisting of package objects, strings, and
symbols.")

(docfun user-homedir-pathname function (&optional host) "
Returns a pathname the represents the user's home directory.  HOST is simply
ignored in ECL.")

(docfun values function (&rest args) "
Returns ARGs as multiple values, the N-th ARG being the N-th value.")

(docfun values-list function (list) "
Returns all elements of LIST as multiple values, the N-th element of LIST
being the N-th value.")

(doctype vector "
A vector is a one-dimensional array.  Strings and bit-vectors are kinds of
vectors.  Other vectors are called general vectors and are notated as
	#(elem ... elem)
Some vectors may be displaced to another array, may have a fill-pointer, or
may be adjustable.  Other vectors are called simple-vectors.")

(docfun vector function (&rest objects) "
Creates and returns a simple-vector, with the N-th OBJECT being the N-th
element.")

(docfun vector-pop function (vector) "
Decrements the fill-pointer of VECTOR by one and returns the element pointed
to by the new fill-pointer.  Signals an error if the old value of the fill-
pointer is 0 already.")

(docfun vector-push function (item vector) "
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  Returns NIL if the new
value of the fill-pointer becomes too large.  Otherwise, returns the new fill-
pointer as the value.")

(docfun vector-push-extend function (item vector &optional (n (length vector))) "
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  If the new value of
the fill-pointer becomes too large, extends VECTOR for N more elements.
Returns the new value of the fill-pointer.")

(docfun vectorp function (x) "
Returns T if X is a vector; NIL otherwise.")

(docfun warn function (format-string &rest args) "
Formats FORMAT-STRING and ARGs to *ERROR-OUTPUT* as a warning message.  Enters
a break level if the value of *BREAK-ON-WARNINGS* is non-NIL.  Otherwise,
returns with NIL.")

(docfun when macro "(when test {form}*)" "
If TEST evaluates to non-NIL, then evaluates FORMs and returns all values of
the last FORM.  If not, simply returns NIL.")

(docfun with-input-from-string macro
	"(with-input-from-string (var string-form {keyword value}*)
           {decl}* {form}*)" "
Evaluates FORMs with VAR bound to a string input stream from the string that
is the value of STRING-FORM.  The stream is automatically closed on exit.
Possible keywords are :INDEX, :START, and :END.")

(docfun with-open-file macro
	"(with-open-file (var filespec-form {options}*) {decl}* {form}*)" "
Opens the specified file using OPTIONs, and evaluates FORMs with VAR bound to
a stream to/from the file.  The file is automatically closed on exit.  See
OPEN for the options.")

(docfun with-open-stream macro
	"(with-open-stream (var stream-form) {decl}* {form}*)" "
Evaluates FORMs with VAR bound to the value of STREAM-FORM.  The stream is
automatically closed on exit.")

(docfun with-output-to-string macro
	"(with-output-to-string (var [string-form]) {decl}* {form}*)" "
Evaluates FORMs with VAR bound to a string output stream to the string that is
the value of STRING-FORM.  If STRING-FORM is not given, a new string is used.
The stream is automatically closed on exit and the string is returned.")

(docfun write function (object &key (stream *standard-output*) (escape *print-escape*)
                   (radix *print-radix*) (base *print-base*)
                   (circle *print-circle*) (pretty *print-pretty*)
                   (level *print-level*) (length *print-length*)
                   (case *print-case*) (array *print-array*)
                   (gensym *print-gensym*)) "
Prints OBJECT in the specified mode.  See the variable docs of *PRINT-...* for
the mode.")

(docfun write-byte function (integer stream) "
Outputs INTEGER to the binary stream STREAM.  Returns INTEGER.")

(docfun sys:write-bytes function (stream string start end) "
ECL specific.
Write onto STREAM a series of bytes from STRING
starting from position START up to END (exclusive).
It returns the number of bytes actually transferred, or -1 if the
operation failed.
The stream is automatically flushed.")

(docfun write-char function (char &optional (stream *standard-output*)) "
Outputs CHAR to STREAM.  Returns CHAR.")

(docfun write-line function (string &optional (stream *standard-output*)
              &key (start 0) (end (length string))) "
Outputs STRING and a newline character to STREAM.  Returns STRING.")

(docfun write-string function (string &optional (stream *standard-output*)
              &key (start 0) (end (length string))) "
Outputs STRING to STREAM.  Returns STRING.")

(docfun write-to-string function (object &key (escape *print-escape*) (radix *print-radix*)
                   (base *print-base*) (circle *print-circle*)
                   (pretty *print-pretty*) (level *print-level*)
                   (length *print-length*) (case *print-case*)
                   (array *print-array*) (gensym *print-gensym*)) "
Returns as a string the printed representation of OBJECT in the specified
mode.  See the variable docs of *PRINT-...* for the mode.")

(docfun y-or-n-p function (&optional (format-string nil) &rest args) "
Asks the user a Y-or-N question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear.")

(docfun yes-or-no-p function (&optional (format-string nil) &rest args) "
Asks the user an YES-or-NO question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear.")

(docfun zerop function (number) "
Returns T if the arg is zero; NIL otherwise.")

#|
;;; ----------------------------------------------------------------------
;;; System Builder Tools

(unless (find-package 'sbt) (make-package 'sbt))

(docfun sbt::build-system macro "(system &optional op mode)" "
It allows to perform operations on a system defined with SBT:DEFSYSTEM.
The possible operations are: :LOAD, :COMPILE and :PRINT.
For the load operation, in alternative to the default of loading all the
binaries in the appropriate order, there are two modes of operation
specifiable via the optional parameter MODE, which can be
:QUERY and :SOURCE.
The latter option will load the sources of the system, while with :QUERY
the user will be prompted on each file to be loaded.

The default mode for compilation is to compile just the files which need
to be recompiled according to their dependencies.
With the :FORCE option, all the files are recompiled, while with the
:QUERY option, the user will be prompted.

By supplying \fCT\fP for the :PRINT option, the sequence of operations
to be performed to build the system will be printed.
")

(docfun sbt::defsystem macro
	"(name &key :modules :directory :pathname-types)" "
NAME should be a symbol which will be used to refer to the system.
The value of :MODULES should be a list of module dependencies of
the form:

	(file load-deps compile-deps recompilation-deps)

where load-deps compile-deps recompilation-deps are lists of module names.
If the value specified for :directory is a cons, then the CAR is used as
the source file directory and the CDR is used as the binary file directory.
The values specified for :PATHNAME-TYPES specifies the extensions for
LISP souce files and binaries.")

;;; ----------------------------------------------------------------------
;;; THREADS

(docfun %delay function (nsec) "
Stops the thread execution for a time interval of NSEC real seconds. The
thread status is set to suspended.")

(docfun %disable-scheduler function () "
Disables the scheduler, so that the execution of the current thread will not be
interrupted by the scheduler until the %enable-scheduler function is called")

(docfun %enable-scheduler function () "
Enables the scheduler, so that it will time-slice execution among all
running threads.")

(docfun %suspend function () "
Sets the current thread status to suspended and suspends its execution.")

(docfun %thread-wait function (predicate &rest args) "
Applies the PREDICATE to the ARGS, in the environment of the calling thread.
If the result is not nil the thread will continue its execution. Otherwise the
thread is suspended and its status set to waiting. The thread will be resumed
again when the condition will become true.")

(docfun %thread-wait-with-timeout function (nsec predicate &rest args) "
Applies the PREDICATE to the ARGS, in the environment of the calling thread.
If the result is not nil the thread will continue its execution. Otherwise the
thread is suspended and its status set to waiting.  The thread will be resumed
again when either the condition will become true or the timeout of NSEC 
seconds has expired.")

(docfun current-thread function (thread) "
Returns the THREAD within which this function was called.")

(docfun deactivate function (thread) "
Stops a running THREAD, setting its status to stopped. A stopped thread can
be resumed with reactivate function.")

(docfun kill-thread function (thread) "
Stops the THREAD execution and set its status to dead.")

(docfun make-continuation function (thread &key :cont) "
Creates a unique new continuation for resuming the THREAD. :CONT is an optional
continuation to be supplied to the thread.")

(docfun make-thread function (function) "
Creates a new thread ready to execute FUNCTION. The thread is in a suspended
status and can run only making a continuation for it and issuing a resume
to such continuation with a list of arguments.")

(docfun reactivate function (thread) "
Sets the THREAD status to running.")

(docfun resume function (continuation &rest args) "
Resumes execution of CONTINUATION and passes it the ARGS.")

(docfun spawn macro (function &rest args) "
Creates a new thread where FUNCTION is applied to the ARGS. Returns immediately
the new thread without waiting for the function to return.")

(docfun thread-list function (thread) "
Returns the full list of the not DEAD threads")

(docfun thread-status function (thread) "
Returns the THREAD status (this can be: running, suspended, stopped or dead)")

(docfun without-scheduling macro "({form}*)" "
Executes the FORMs in sequence within a critical region, ensuring that the
scheduler is disabled.")

;;; ----------------------------------------------------------------------
;;; Unify instructions

(docfun sys:dereference function (locative) "
ECL specific.
Given LOCATIVE, it returns the object to which it points. If the
location is unbound, the value returned is OBJNULL.")

(docfun sys:locativep function (object) "
ECL specific.
Returns true if OBJECT is bound to a locative.")

(docfun sys:make-variable function (name) "
ECL specific. 
Creates a new logical variable with name name implemented as a cons. 
Name is used just for printing purposes.")

(docfun sys:get-constant function (constant object) "
ECL specific.
The value of OBJECT is unified with the constant CONSTANT.
Returns T if successful, otherwise NIL.")

(docfun sys:get-cons function (object) "
ECL specific.
The value of OBJECT is unified with a CONS cell.
Returns T if successful, otherwise NIL.")

(docfun sys:get-instance function (object class arity) "
ECL specific.
The value of OBJECT is unified with an instance of class CLASS
with ARITY number of slots.
Returns T if successful, otherwise NIL.")

(docfun sys:get-value function (variable object) "
ECL specific.
The value of VARIABLE and OBJECT are unified.
Returns T if successful, otherwise NIL.")

(docfun get-variable macro (variable object) "
ECL specific.
Identical to SETQ: assigns to the variable VARIABLE the value OBJECT.")

(docfun sys:get-nil function (object) "
ECL specific.
The value of OBJECT is unified with the constant NIL.
Returns T if successful, otherwise NIL.")

(docfun sys:trail-mark function () "
ECL specific.
Sets up a choice point by putting a mark on the trail stack for
backtracking.")

(docfun sys:trail-restore function () "
ECL specific.
Unwinds the trail stack up to the latest choice point.")

(docfun sys:trail-unmark function () "
ECL specific.
Does a TRAIL-RESTORE and also removes the latest choice point.")

(docfun sys:unboundp function (locative) "
ECL specific.
Returns true if LOCATIVE is bound to OBJNULL.")

(docfun sys:unify-constant function (constant) "
ECL specific.
Read mode: the next subterm is unified with the constant CONSTANT.
Write mode: the constant constant is stored as the next subterm.")

(docfun sys:unify-nil function () "
ECL specific.
Read mode: the next subterm is unified with the constant NIL.
Write mode: the constant NIL is stored as the next subterm.")

(docfun sys:unify-value function (variable) "
ECL specific.
Read mode: the value of VARIABLE is unified with the next subterm.
Write mode: the value of VARIABLE is stored as the next subterm.")

(docfun sys:unify-variable macro (variable) "
ECL specific.
Read mode: VARIABLE is assigned the next subterm.
Write mode: a new variable is stored in VARIABLE as the next subterm.")

|#
;;;----------------------------------------------------------------------

(close *stream*)
