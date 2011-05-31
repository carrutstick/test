Sudoku Solver
=============

Solves a Sudoku game using a depth-first search with some intermediate logic.

After loading the "search.lisp" file, boards should be constructed as 2D arrays
of numbers, with the symbol `-` representing an empty square.
A board may be solved by making a new `game` object and passing it to
`solve-game` as follows:
    (setq game (make-instance 'game 'board *test9*))
    (setq sol (solve-game game))

