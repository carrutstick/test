;;=============================================================================
;; Isaac Carruthers' quick-and-dirty sudoku solver
;;=============================================================================

;; Helper functions and class definitions
(load "dummy.lisp")

;; Check to see whether every block, row, and column contains every needed
;; element
(defun solvedp (board)
  (eval
   ;; Read: (and (each-block-has-all-values) (each-row...))
   (cons 'and (append (loop for block in (block-groups board)
			    collect (subsetp values block)) 
		      (loop for row in (row-groups board)
			    collect (subsetp values row))
		      (loop for col in (column-groups board)
			    collect (subsetp values col))) )))

;; Faster method which just makes sure there are no empty elements
(defun fullp (board)
  (eval
   (cons 'and 
	 (loop for x from 0 to (- MAXX 1) 
	       collect (cons 
			'and 
			(loop for y from 0 to (- MAXY 1)
			      collect (not (empty board x y))))))))

;; Finds all non-empty elements on the specified row
(defun row-contents (board y)
  (loop for x from 0 to (- MAXX 1) 
	when (not (empty board y x)) collect (aref board y x)))

;; Finds all non-empty elements on the specified column
(defun col-contents (board x)
  (loop for y from 0 to (- MAXY 1)
	when (not (empty board y x)) collect (aref board y x)))

;; Finds all non-empty elements in the specified box
(defun box-contents (board boxx boxy)
  (apply 
   #'union
   (loop for j from (* boxy yblocks) below (* (+ boxy 1) yblocks)
	 collect
	 (loop for i from (* boxx xblocks) below (* (+ boxx 1) xblocks)
	       when (not (empty board j i)) 
	       collect (aref board j i)))))

;; Determines the number of naive options available to a space
(defun cell-options (board x y)
  (set-difference 
   values 
   (union 
    (union
     (row-contents board y)
     (col-contents board x) )
    (box-contents board (floor (/ x XBLOCKS)) (floor (/ y YBLOCKS))) )))

;; Set all fully-constrained cells to their contrained values (non-destructive)
(defun fill-board (board)
  (let (new-board (copy-array board))
    (loop for j from 0 to (- MAXY 1) do
	  (loop for i from 0 to (- MAXX 1) 
		when (and (empty new-board j i)
			  (equal (length (cell-options new-board i j)) 1))
		do (setf (aref new-board j i) 
			 (car (cell-options new-board i j)) )
		(setq i 0) (setq j 0) ))))
			  