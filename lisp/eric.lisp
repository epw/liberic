;; Copyright (C) Eric Willisson 2011
;; This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
;; for details

;; This is a library of useful functions I have developed for Common Lisp
;; programs

(eval-when (:compile-toplevel)
  (dolist (pkg '(:cl-ppcre :split-sequence :drakma :usocket))
    (handler-case (asdf:oos 'asdf:load-op pkg)
      (asdf:missing-component (c) (declare (ignore c)) nil))))

(defpackage :eric
  (:use :cl)
  (:export :include
	   :def-if-pkg
	   :not-found
	   :assign
	   :make-keyword
	   :symbol-concat
	   :get-all-symbols
	   :save-package-state
	   :fopen
	   :popen
	   :read-in-file
	   :read-from-file
	   :write-to-file
	   :overwrite-to-file
	   :var-output
	   :new-line
	   :rtod
	   :dtor
	   :int/
	   :2*
	   :2/
	   :hypot
	   :between
	   :nop
	   :convert-seq
	   :regex-match
	   :strsub
	   :strsub-list
	   :split
	   :string+
	   :add
	   :subtract
	   :multiply
	   :divide
	   :equals
	   :len
	   :range
	   :insert
	   :unsert
	   :swap
	   :[]
	   :print-hash
	   :lookup
	   :defclass-structure-form
	   :factor
	   :factor-pairs
	   :http-error
	   :get-internet-file
	   :name-of-weekday
	   :name-of-month
	   :socket-format
	   :socket-read-line
	   :socket-read-all
	   :incomplete-code-condition
	   :finish-later
	   :ncr
	   :npr
	   :fac))

(in-package :eric)

(defun not-found (pkg)
  "Signal error because package was missing."
  (error 'simple-error :format-control "Necessary package ~a not found."
	 :format-arguments (list pkg)))

(eval-when (:compile-toplevel)
  (defmacro def-if-pkg (package-designator definition)
    "Compile top-level form only if package is included."
    (if (find-package package-designator) definition
	`(,(first definition) ,(second definition) ,(third definition)
	   ,(if (stringp (fourth definition)) (fourth definition) "")
	   (not-found ,package-designator)))))

(defun include (&rest packages)
  "Convenience function for (asdf:oos 'asdf:load-op package)"
  (let ((*standard-output* (make-string-output-stream)))
    (dolist (pkg packages)
      (handler-bind ((style-warning
		      (lambda (c)
			(declare (ignore c))
			(invoke-restart 'muffle-warning))))
	(asdf:oos 'asdf:load-op pkg)))))

(defmacro assign (var form &body bdoy)
  "Syntactic sugar for let using only one variable, and returning it"
  `(let ((,var ,form))
     ,@bdoy
     ,var))

(defun make-keyword (symbol)
  "Concatenate : onto front of symbol, converting it to a keyword."
  (read-from-string (format nil ":~a" symbol)))

(defun symbol-concat (&rest symbols)
  "Concatenate symbols together into one longer symbol."
  (read-from-string (format nil "~{~(~a~)~}"
			    (mapcar #'symbol-name symbols))))

(defmacro fopen ((stream filespec &optional (mode :r) &rest options)
		 &body body)
  "This macro streamlines with-open-file. It takes all the same options,
but also accepts mode as the third argument, which can be :r, :w, :a, or :io.
These behave as the modes for libc's fopen, though with :io instead of
\"r+\"."
  (let (direction if-exists
		  (if-does-not-exist :create))
    (cond ((eq mode :r)
	   (setf direction :input)
	   (setf if-does-not-exist nil))
	  ((eq mode :w)
	   (setf direction :output)
	   (setf if-exists :supersede))
	  ((eq mode :a)
	   (setf direction :output)
	   (setf if-exists :append))
	  ((eq mode :io)
	   (setf direction :io)
	   (setf if-exists :overwrite))
	  (t (error "Unknown mode ~a" mode)))
    `(with-open-file (,stream ,filespec :direction ,direction
			      :if-exists ,if-exists
			      :if-does-not-exist ,if-does-not-exist
			      ,@options)
       ,@body)))

(defun popen (command &optional args input output-stream-p)
  "Streamlines sb-ext:run-program. Command is a string, as would be typed
into a shell, with the executable and arguments. The optional args is a list
of strings as further arguments to be appended, because command will not
understand quotes around arguments with spaces. Input is either a string
which is converted into an input-stream and fed to the executable, or is
passed directly to the :input keyword of sb-ext:run-program.
Returns a string of the process' output unless output-stream-p is t, in which
case a string-output-stream is returned."
  #+abcl (declare (ignore command args input output-stream-p))
  #-abcl (let* ((words (split command))
		(executable (first words))
		(args (append (rest words) args))
		(stdin (if (stringp input) (make-string-input-stream input) input))
		(stdout (if output-stream-p :stream
			    (make-string-output-stream))))
	   (let ((process (sb-ext:run-program executable args :input stdin
					      :output stdout :search t)))
	     (if output-stream-p (sb-ext:process-output process)
		 (get-output-stream-string stdout)))))

(defun get-all-symbols (&optional (package *package*))
  "Returns a list of two lists. First, all symbols which correspond to
variables. Second, all symbols which correspond to functions and macros."
  (let ((var-list ())
	(fun-list ())
        (package (find-package package)))
    (do-all-symbols (s)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s fun-list))
            (push s fun-list)))
      (when (boundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s var-list))
            (push s var-list))))
    (list var-list fun-list)))

(defun save-package-state (filespec &optional (package *package*))
  "Should save entire package as seen from the REPL, making it LOAD-able.
Does not yet work."
  (let* ((all-symbols (get-all-symbols package))
	 (var-symbols (first all-symbols))
	 (fun-symbols (second all-symbols))
	 (pkg-desig (make-keyword (package-name package))))
    (fopen (f filespec :w)
      (flet ((print-f (object) (print object f)))
	(print-f `(defpackage ,pkg-desig
		    (:use ,@(mapcar (lambda (p) (make-keyword (package-name
							       p)))
				    (package-use-list package)))
		    (:export ,@(assign externed ()
				       (do-external-symbols (sym package)
					 (push (make-keyword sym)
					       externed))))))
	(print-f `(in-package ,pkg-desig))
	(dolist (sym var-symbols)
	  (print-f `(defparameter ,sym ,(symbol-value sym))))
	(dolist (sym fun-symbols)
	  (print-f `(eric::defun ,sym ,(get sym 'function-lambda-list)
		      ,@(get sym 'function-lambda-expression))))
	(print-f `(provide ,pkg-desig))
	(princ #\Newline f)))))

(defun read-stream (stream)
  "Read entire stream into string as efficiently as possible."
  (let ((string (make-string (file-length stream))))
    (read-sequence string stream)
    string))

(defun read-in-file (filespec)
  "Convenience function for (fopen (f filespec) (read-stream f))"
  (fopen (file filespec :r)
    (if file
	(read-stream file)
	nil)))

(defun read-from-file (filespec)
  "Convenience function for (fopen (f filespec) (read f))"
  (fopen (file filespec :r)
    (if file
	(read file)
	nil)))

(defun read-from-file-closure (filespec)
  "Create a closure which contains a file object specified by FILESPEC, and
every time it is called it returns the next line (subject to upgrading)"
  (let ((file (open filespec :direction :input :if-does-not-exist nil)))
    (values
     (lambda () (handler-case (read-line file)
		  (end-of-file () (progn (close file)
					 'END-OF-FILE))
		  (t (e) (progn (close file)
				(error e)))))
     file)))


(defun write-to-file (filespec fmt &rest fmt-args)
  "Append string (format style) to file identified by path."
  (assign string (apply #'format (append (list nil fmt) fmt-args))
    (fopen (file filespec :a)
      (format file string))
    string))

(defun overwrite-to-file (filespec fmt &rest fmt-args)
  "Write string (format style) to file identified by path."
  (assign string (apply #'format (append (list nil fmt) fmt-args))
    (fopen (file filespec :w)
      (format file string))
    string))

(defmacro var-output (variable &optional (tabbing 0))
  "Print out a symbol and the value of the variable bound to it."
  (let ((spaces (make-sequence 'string tabbing :initial-element #\Space)))
    `(format t "~{ ~}~s: ~s~%" ,spaces ',variable ,variable)))

(defun new-line ()
  "Print a new line."
  (princ #\Newline)
  nil)

(defun rtod (radians)
  "Convert radians to degrees"
  (* radians (/ 180 pi)))

(defun dtor (degrees)
  "Convert degrees to radians"
  (* degrees (/ pi 180)))

(defun int/ (&rest numbers)
  "Integer division."
  (floor (apply #'/ numbers)))

(defun 2* (number)
  "Part of the set of functions based off of the 1+ function. Doubles a number."
  (* number 2))

(defun 2/ (number)
  "Part of the set of functions based off of the 1+ function. Halves a number."
  (/ number 2))

(defun hypot (a b &rest more)
  "Calculate the hypotenuse of two or more sides."
  (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) (cons a (cons b more))))))

(defun nop (obj)
  "Returns the argument (no operation). Good for mapping sequences."
  obj)

(defun convert-seq (type sequence)
  "Convert a sequence to given type (accepts 'str and 'array). Uses special
cases to handle many conversions to strings."
  (let ((pred #'nop))
    (cond ((eq type 'str) (setf type 'string))
	  ((eq type 'array) (setf type 'vector)))
    (if (eq type 'string)
	(setf pred #'character))
    (map type pred sequence)))

(def-if-pkg :ppcre
    (defun regex-match (regex target-string &key (start 0) end)
      "Attempt to match TARGET-STRING with REGEX, optionally using START
and END as string bounds."
      (if end
	  (ppcre:scan-to-strings regex target-string :start start :end end)
	  (ppcre:scan-to-strings regex target-string :start start))))

(def-if-pkg :ppcre
    (defun strsub (original old new)
      "CL-PPCRE wrapper for regex-replace-all. Takes original string, regular
expression, and replacement string."
      (ppcre:regex-replace-all old original new)))

(def-if-pkg :ppcre
    (defun strsub-list (original &rest pairs)
      "Executes strsub on each pair (just two arguments) of old and new strings."
      (loop for n from 0 to (2/ (1- (length pairs))) do
	   (setf original (strsub original (nth (2* n) pairs)
				  (nth (1+ (2* n)) pairs))))
      original))

(defun between (number left-bound right-bound &optional (inclusive t))
  "Check whether number is between left-bound and right-bound (optionally
declare whether test is inclusive)"
  (if (< right-bound left-bound)
      (let ((tmp right-bound))
	(setf right-bound left-bound)
	(setf left-bound tmp)))
  (if inclusive 
      (and (>= number left-bound) (<= number right-bound))
      (and (> number left-bound) (< number right-bound))))

(defun split (string &optional (deliminator " ") max)
  "Split string using substring DELIMINATOR, up to MAX times."
  (let ((pieces ())
	(pos 0))
    (if (not (stringp deliminator)) (setf deliminator
					  (format nil "~a" deliminator)))
    (loop
       (setf pos (search deliminator string))
       (if (null pos) (return))
       (if (> pos 0) (push (subseq string 0 pos) pieces))
       (setf string (subseq string (+ pos (length deliminator))))
       (when max
	 (decf max)
	 (if (= max 0) (return))))
    (if (> (length string) 0) (push string pieces))
    (reverse pieces)))

(defun string+ (&rest strings)
  "Concatenate strings together."
  (apply #'concatenate 'string strings))

(defgeneric add (x &rest args)
  (:documentation "Add x to the rest of the arguments (a method should
specialize x, and run something equivalent to (+ x (apply #'add args)"))

(defmethod add ((x (eql nil)) &rest args)
  (declare (ignore args))
  0)

(defmethod add ((x number) &rest args)
  (+ x (apply #'add (or args '(())))))


(defgeneric subtract (x &rest args)
  (:documentation "Subtracts the rest of the arguments from x(a method should
specialize x, and run something equivalent to (- x (apply #'add args)"))

(defmethod subtract ((x (eql nil)) &rest args)
  (declare (ignore args))
  0)

(defmethod subtract ((x number) &rest args)
  (- x (apply #'add (or args '(())))))


(defgeneric multiply (x &rest args)
  (:documentation "Multiply x by the rest of the arguments (a method should
specialize x, and run something equivalent to (* x (apply #'multiply args)"))

(defmethod multiply ((x (eql nil)) &rest args)
  (declare (ignore args))
  1)

(defmethod multiply ((x number) &rest args)
  (* x (apply #'multiply (or args '(())))))

(defgeneric divide (x &rest args)
  (:documentation "Divide x by the rest of the arguments (a method should
specialize x, and run something equivalent to (/ x (apply #'multiply args)"))

(defmethod divide ((x (eql nil)) &rest args)
  (declare (ignore args))
  1)

(defmethod divide ((x number) &rest args)
  (/ x (apply #'multiply (or args '(())))))

(defgeneric equals (x y)
  (:documentation "Generic form of EQUAL function"))

(defmethod equals (x y)
  (equal x y))

(defgeneric len (sequence)
  (:documentation "Generic form of LENGTH function (return an integer that
is the length of SEQUENCE)"))

(defmethod len ((sequence list))
  (length sequence))

(defmethod len ((sequence array))
  (length sequence))

(defun range (n &optional end (step 1))
  "Return list of integers. Default list is [0, N). If END is specified,
[N, END). Difference between adjacent elements of the list is STEP (can be
negative)."
  (when (null end)
    (setf end (- n step)
	  n 0))
  (let (list)
    (do ((i n (+ i step)))
	((= i (+ end step)))
      (push i list))
    (reverse list)))

(defun insert (object list place)
  "Insert OBJECT into LIST at PLACE and return the new list such that
(insert 'a '(1 2 3) 0) => (A 1 2 3)"
  (append (butlast list (- (length list) place)) (list object)
	  (subseq list place)))

(defun unsert (list place)
  "Delete element at PLACE and return the new list."
  (append (subseq list 0 place) (subseq list (1+ place))))

(defun swap (list pos1 pos2)
  "Swap elements at positions POS1 and POS2 and return the new list."
  (let ((el1 (elt list pos1))
	(el2 (elt list pos2))
	(new-list (copy-list list)))
    (setf (elt new-list pos1) el2
	  (elt new-list pos2) el1)
    new-list))

(defun class-slot-def (slot class-name)
  "Takes symbols represeting a potential slot in a class and the name of
the class and returns a list which will make a standard definition for the
slot in a defclass (with initarg and accessor)"
  (list slot :initarg (make-keyword slot)
	:accessor (symbol-concat class-name '- slot)))

(defun class-slots-def (slots class-name)
  "Takes a list of symbols representing slots and the name of the class they
belong to, and maps the slots with CLASS-SLOT-DEF"
  (mapcar (lambda (slot) (class-slot-def slot class-name)) slots))

(defmacro defclass-structure-form (name direct-superclasses
				   &rest direct-slots)
  "Defines a class, but with automatic initargs and accessors as a
structure would have."
  `(defclass ,name ,direct-superclasses
     ,(class-slots-def direct-slots name)))

(defun print-hash (hash)
  "Print out a hash table, as key/value pairs."
  (maphash (lambda (key val) (if val (print (list key val)))) hash))

(defun break-for-emacs (string &optional (length 80))
  "Break a string by inserting newlines for Emacs (defaults to 80 columns)"
  (let ((string-list (convert-seq 'list string)))
    (loop for i from length to (length string-list) by length do
	 (loop for j from i downto (- i (2/ length)) do
	      (when (char= (nth j string-list) #\Space)
		(setf (nth j string-list) #\Newline)
		(return))))
    (convert-seq 'string string-list)))

(defun [] (object &rest args)
  "General lookup function. Uses generic function to dispatch recursively based on type of each successive argument."
  (if (null (first args)) object
      (apply #'[] (lookup object (first args)) (rest args))))

(defgeneric lookup (object index)
  (:documentation "Actual generic function called by []. Can be extended to new types with methods. Specific methods are already defined for objects, structures, sequences, multidimensional arrays, alists and plists, and hashes."))

(defmethod lookup ((object standard-object) (index symbol))
  (slot-value object index))
(defmethod lookup ((object structure-object) (index symbol))
  (slot-value object index))
(defmethod lookup ((object sequence) (index integer))
  (elt object index))
(defmethod lookup ((object array) (index integer))
  (aref object index))
(defmethod lookup ((object array) (index list))
  (apply #'aref object index))
(defmethod lookup ((object list) (index symbol))
  (if (listp (first object)) (assoc index object)
      (getf object index)))
(defmethod lookup ((object hash-table) index)
  (gethash index object))

(defun factor (n)
  "Return all factors of n in a list."
  (let ((factors ()))
    (loop for i downfrom (floor (sqrt n)) to 1 do
	 (when (integerp (/ n i))
	   (push i factors)
	   (unless (= i (sqrt n))
	     (setf factors (append factors (list (/ n i)))))))
    factors))

(defun factor-pairs (n)
  "Return all factors of n, in pairs which multiply to n."
  (let ((factors ()))
    (loop for i downfrom (floor (sqrt n)) to 1 do
	 (when (integerp (/ n i))
	   (push (list i (/ n i)) factors)))
    factors))

(define-condition http-error (error)
  ((status :initarg :status :reader status)
   (reason :initarg :reason :reader reason)))

(def-if-pkg :drakma
    (defun get-internet-file (uri)
      "Return a file from the Internet, designated by uri, and cache it. If the cached file already exists, do not perform an HTTP lookup, and just return the filename of the cached version."
      (let ((filename (subseq uri (1+ (position #\/ uri :from-end t)))))
	(if (string= filename "") (setf filename "index.html"))
	(if (not (probe-file filename))
	    (multiple-value-bind (data status headers response-uri stream
				       needs-close-p reason)
		(drakma:http-request uri)
	      (declare (ignore headers response-uri stream needs-close-p))
	      (if (= status 200)
		  (eric:fopen (f filename :w
				 :element-type (typecase data
						 (string 'base-char)
						 (vector '(unsigned-byte
							   8))))
		    (write-sequence data f))
		  (error 'http-error :status status :reason reason))))
	filename)))

(defun name-of-weekday (day-of-week)
  "Convert a number from 0 to 6 into a 3 or 4 character abbreviation of a weekday."
  (let ((weekdays '("Mon" "Tues" "Wed" "Thur" "Fri" "Sat" "Sun")))
    (nth day-of-week weekdays)))

(defun name-of-month (month)
  "Convert a number from 1 to 12 into a 3 character abbreviation of a month."
  (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept"
		  "Oct" "Nov" "Dec")))
    (nth (1- month) months)))

(def-if-pkg :usocket
    (defun socket-format (socket control-string &rest format-arguments)
      "Write data to a socket, formatted with FORMAT."
      (apply #'format (cons (usocket:socket-stream socket)
			    (cons control-string format-arguments)))
      (force-output (usocket:socket-stream socket))))

(def-if-pkg :usocket
    (defun socket-read-line (socket)
      "Read one line from a socket."
      (read-line (usocket:socket-stream socket) nil)))

(def-if-pkg :usocket
    (defun socket-read-all (socket)
      "Read all available data from a socket."
      (let ((out (make-string-output-stream)))
	(do ((line
	      (socket-read-line socket) (socket-read-line socket)))
	    ((not line))
	  (format out "~A" line))
	(get-output-stream-string out))))

(define-condition incomplete-code-condition (error)
  ())

(defun finish-later (stream subchar arg)
  "Function for a ... reader macro indicating 'incomplete code'"
  (declare (ignore subchar arg))
  (if (and (char= (read-char stream) #\.)
	   (char= (read-char stream) #\.))
      `(cerror "Continue anyway" 'incomplete-code-condition)
      (error 'sb-int:simple-reader-error :stream stream
	     :format-control "not enough dots")))

(defun ncr (n k)
  "Compute n choose k"
  (cond ((= k 0) 1)
	((= n 0) 0)
	(t (+ (ncr (1- n) (1- k)) (ncr (1- n) k)))))

(defun fac (n)
  "Compute factorial of integer n."
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t (* n (fac (1- n))))))

(defun npr (n k)
  "Compute number of permutations of k elements out of n."
  (if (= n k) 1
      (* n (npr (1- n) k))))

(provide :eric)
