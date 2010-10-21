;;==============================================================
;; SUDOKU INFRASTRUCTURE CODE
;; CMSC 671 MINIPROJECT
;; PROVIDED BY PROF. MARIE DESJARDINS
;; SEPTEMBER 2010
;;

;;==============================================================
;; TEST PROBLEMS

;; A completely solved board as a reality check
(defvar *solved*)
(setf *solved* (make-array '(4 4)
			   :initial-contents
			   '((2 4 3 1) 
			     (1 3 4 2) 
			     (4 2 1 3) 
			     (3 1 2 4))))

;; A board with only one missing number
(defvar *easy-test*)
(setf *easy-test* (make-array '(4 4)
			      :initial-contents
			      '((- 4 3 1) (1 - - 2) (4 2 - 3) (3 1 2 -))))

;; A board with 7 missing numbers
(setf *test1* (make-array '(4 4)
			  :initial-contents
			  '((- 1 2 -) (3 2 1 -) (- - 3 1) (1 3 - -))))

;; A board with 10 missing numbers
(setf *test2* (make-array '(4 4)
			  :initial-contents
			  '((- 4 - -) (1 3 - -) (- - 1 3) (- - 2 -))))

;; A board with 12 missing numbers
(setf *test3* (make-array '(4 4)
			  :initial-contents
			  '((- - 1 -) (3 - - -) (- - - 1) (- 2 - -))))

;; Another board with 12 missing numbers
(setf *test4* (make-array '(4 4)
			  :initial-contents
			  '((- - 4 -) (3 - - -) (- - - 1) (- 2 - -))))


;; NOTE that for *test5* and *test6* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 2, and reload 
;; this file to recompute MAXX, MAXY, and VALUES.

;; A 6x6 board with 8 missing numbers
(setf *test5* (make-array '(6 6)
			  :initial-contents
			  '((4 - 2 1 6 5) (6 5 - 4 - -)
			    (- 1 5 6 4 3) (3 6 1 2 5 4)
			    (- 2 4 - 1 6) (1 4 6 5 3 -))))

;; A 6x6 board with 16 missing numbers
(setf *test6* (make-array '(6 6)
			  :initial-contents
			  '((1 - - - - 2) (5 - 1 2 - 4)
			    (3 2 - - 1 5) (- 5 - 1 2 6)
			    (2 - - 5 - 1) (- 1 - - 5 3))))

;; NOTE that for *test7*, *test8*, and *test9* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 3, and reload 
;; this file to recompute MAXX, MAXY, and VALUES.

;; An "easy" standard 9x9 board
(setf *test7* (make-array '(9 9)
			  :initial-contents
			  '((6 - - 7 - 3 - - 9) (2 - - - - - - - 4)
			    (- 3 - 9 - 1 - 2 -) (- 5 - 2 - 6 - 8 -)
			    (8 - - - 3 - - - 2) (- 1 - 4 - 9 - 6 -)
			    (- 2 - 5 - 4 - 7 -) (3 - - - - - - - 6)
			    (1 - - 3 - 7 - - 5))))

;; A "medium" 9x9 board
(setf *test8* (make-array '(9 9)
			  :initial-contents
			  '((1 9 - - 6 - 7 - 8) (- - - - - 7 - - 5)
			    (7 - - 2 3 - - - -) (- 1 - - - - 5 - -)
			    (3 - 6 - - - 4 - 9) (- - 9 - - - - 7 -)
			    (- - - - 1 5 - - 3) (5 - - 9 - - - - -)
			    (9 - 3 - 7 - - 5 2))))


;; A "hard" 9x9 board
(setf *test9* (make-array '(9 9)
			  :initial-contents
			  '((9 - - 4 - - 6 - -) (- - 7 - - - - - 3)
			    (- - - 1 2 - - - -) (1 2 - - 4 3 - 5 -)
			    (7 - - - - - - - 4) (- 4 - 7 6 - - 8 9)
			    (- - - - 7 1 - - -) (6 - - - - - 9 - -)
			    (- - 4 - - 8 - - 2))))


;;==============================================================
;; MACROS AND GLOBAL CONSTANTS

;; How many blocks along the x and y axes?  default is 2x2 -> 4x4 board
(defvar XBLOCKS 0
  "Number of large blocks along the x-axis")
(setf XBLOCKS 2)

(defvar YBLOCKS 0
  "Number of large blocks along the y-axis")
(setf YBLOCKS 2)

(defvar MAXX 0
  "Maximum X dimension [1-based] of the current XBLOCKS*YBLOCKS game")
(setf MAXX (* XBLOCKS YBLOCKS))
(defvar MAXY 0
  "Maximum Y dimension [1-based] of the current XBLOCKS*YBLOCKS game")
(setf MAXY (* YBLOCKS XBLOCKS))

(defvar VALUES nil
  "Possible values to fill in a cell")
(setf VALUES (loop for i from 1 to (* XBLOCKS YBLOCKS) collect i))


(defmacro EMPTY-CELL (cell)
  "Return t if a Sudoko cell is empty (i.e., contains '-)"
  `(eq ,cell '-))

(defmacro EMPTY (board x y)
  "Return T if an x,y position on a Sudoku board is an empty cell"
  `(and (not (< ,x 0)) (not (< ,y 0))
	(not (>= ,x MAXX)) (not (>= ,y MAXY))
	(empty-cell (aref ,board ,x ,y))))


;;==============================================================
;; DATA STRUCTURES

;; A game (i.e., a node i the search tree) is a board (i.e., game
;; state) plus some bookkeeping information.

(defclass game ()
  ((name :accessor game-name
         :initarg name
         :initform "")
   (board :accessor game-board
          :initarg board
          :initform (make-array
                     (list MAXX MAXY)
                     :initial-element '-
                     ))
   (parent :accessor game-parent
	   :initarg parent
	   :initform nil)
   (fofx :accessor fofx
	 :initarg fofx
	 :initform 0)
   ;; lists of lists of lists, marking the values still needed in
   ;; each row/col/box.
   (row-req :accessor game-row-req
	    :initform (loop for i from 1 to MAXY collect values))
   (col-req :accessor game-col-req
	    :initform (loop for i from 1 to MAXX collect values))
   (box-req :accessor game-box-req
	    :initform (loop for i from 1 to YBLOCKS
			    (loop for j from 1 to XBLOCKS
				  collect values)))
   ))

;; Method to enforce consistency of slots
(defmethod initialize-instance :after ((newgame game) &key)
  (let ((board (game-board newgame)))
    ;; TODO: Eliminate requirements based on board
    ))

;;==============================================================
;; USEFUL UTILITY FUNCTIONS

;; COPY-ARRAY:  Make a copy of a 2-D array that is EQUAL but
;; not EQ to the original array.  Will be used by EXPAND.
(defun COPY-ARRAY (a)
  "Copy a 2-D array"
  (let* ((dims (array-dimensions a))
	 (b (make-array dims)))
    (loop for x from 0 to (- (first dims) 1) do
	  (loop for y from 0 to (- (second dims) 1) do
		(setf (aref b x y) (aref a x y))))
    b))


;; ARRAY-EQUAL:  Return T if two 2-D arrays are EQUAL (that
;; is, if they are EQ at every array position).  Will be used
;; in CHECK-REPEATED.
(defun ARRAY-EQUAL (a1 a2)
  "Check to see whether two 2-D arrays are EQUAL,
i.e., are EQ at every array position"
  (let* ((dims1 (array-dimensions a1))
	 (dims2 (array-dimensions a2)))
    (when (equal dims1 dims2)
      (loop for x from 0 to (- (first dims1) 1) do
	    (loop for y from 0 to (- (second dims1) 1) do
		  ;; If any position isn't EQ, return nil
		  (if (not (eq (aref a1 x y) (aref a2 x y)))
		      (return-from array-equal nil))))
      ;; If we got this far, then the array is EQ everywhere
      (return-from array-equal t)))
  ;; If we got here, the dimensions didn't match
  nil)


;; Method for printing an object of class GAME to an output stream
;; (default standard output) - just uses PRINT-BOARD to print the board.
;; (This is so that when you look at a game in the interpreter window,
;; it won't print a big hairy defstruct object.)
(defmethod PRINT-OBJECT ((g game) str)
  "Print an object of class GAME to an output stream"
  (format str "Game ~s:~%" (game-name g))
  (print-board (game-board g) str))


;; Print a game board, neatly formatted with 4-space columns. 
;; See ~< ~> formatting string and behold the power of FORMAT! 
(defun PRINT-BOARD (b &optional (str t))
  "Print a formatted Sudoku game board"
  (loop for x from 0 to (- MAXX 1) do
	(loop for y from 0 to (- MAXY 1) do
	      (format str "~4<~s~>" (or (aref b x y)
					'-)))
	(format str "~%")))


;;==============================================================
;; BOARD PROCESSING FUNCTIONS

(defun BLOCK-GROUPS (board)
  "Take a board (array) and return a list of lists, one with the values
in each block of the board"
  (loop 
   for x from 0 to (- XBLOCKS 1)
   nconc (loop 
	  for y from 0 to (- YBLOCKS 1)
	  collect (loop 
		   for i from 0 to (- YBLOCKS 1)
		   nconc (loop for j from 0 to (- XBLOCKS 1)
			       ;; Each block is YBLOCKS cells wide and 
			       ;; XBLOCKS cells high.
			       ;; We're on the xth horizontal block
			       ;; and the yth vertical block, so offset
			       ;; appropriately.
			       collect (aref board 
					     (+ (* y XBLOCKS) j)
					     (+ (* x YBLOCKS) i)
					     ))))))

(defun ROW-GROUPS (board)
  "Take a board (array) and return a list of lists, one with the values
in each row of the board"
  (loop for i from 0 to (- MAXX 1)
	collect (loop for j from 0 to (- MAXY 1)
		      collect (aref board i j))))


(defun COLUMN-GROUPS (board)
  "Take a board (array) and return a list of lists, one with the values
in each column of the board"
  (loop for j from 0 to (- MAXY 1)
	collect (loop for i from 0 to (- MAXX 1)
		      collect (aref board i j))))



;;==============================================================
;; DEBUGGING AND LOGGING

(defvar *DEBUG* t
  "Output stream for printing debugging and logging messages;
default is stdout (T)")


;; Open a log file.
;; Use (format *DEBUG* FORMAT-STRING ARG ...) to record debugging
;; and logging information into the logging file.  If no log file
;; is open, these messages will be written to the terminal screen.
(defun OPEN-DEBUG (file)
  (setf *DEBUG* (open file :direction :output :if-exists :overwrite
		      :if-does-not-exist :create)))


;; Close the log file, and reset *DEBUG* to standard output (T)
(defun CLOSE-DEBUG ()
  (when (not (eq *DEBUG* t))
    (close *DEBUG*)
    (setf *DEBUG* t)))

