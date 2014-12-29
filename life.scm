#!/usr/bin/guile \
-e main -s
!#
;;;
;;; TODO -b or --block_size for >1 size blocks
;;; TODO look into the getlongopts function in ice-9 module
;;; TODO color schemes for alive and dead blocks, or for whole system.
;;; TODO work on screen flicker
;;; 

(use-modules (ncurses curses)
             (srfi srfi-1)
             (ice-9 format))


;;; Game of Life functions


(define (subseq lst l h)
  "returns the subsequence of lst from l to h"
  (let ((len (length lst)))
    (cond ((or (>= l h) (>= l len)) '())
          ((< l 0) (subseq lst 0 h))
          ((> h len) (subseq lst l len))
          (else (let loop ((i 0) (lst lst) (l l))
                  (if (< i l)
                      (loop (+ i 1) (cdr lst) l)
                      (if (< l h)
                          (cons (car lst)
                                (loop (+ i 1) (cdr lst) (+ l 1)))
                          '())))))))


(define (submatrix lst lx hx ly hy)
  "returns a subsequence of subsequences or submatrix"
  (map (lambda (x)
         (subseq x lx hx))
       (subseq lst ly hy)))


(define (check-life state x y)
  "Checks the state of the square at (x, y)"
  (list-ref (list-ref state y) x))


(define (number-of-neighbors state x y)
  "Calculate the number of neighbors for a square"
  (- (fold + 0
           (map (lambda (x) (fold + 0 x))
                (submatrix state (- x 1) (+ x 2) (- y 1) (+ y 2))))
     (check-life state x y)))


(define (cell-update state x y)
  "Determines the future of a cell (lives, dies or is born)"
  (let ((alive (= (check-life state x y) 1))
        (neighbors (number-of-neighbors state x y)))
    (cond
     ((and alive (< neighbors 2)) 0)
     ((and alive (or (= neighbors 2)
                     (= neighbors 3))) 1)
     ((and alive (> neighbors 3)) 0)
     ((and (not alive) (= neighbors 3)) 1)
     (else 0))))


(define (update-board state)
  "Updates the board"
  (let ((rows (length state))
        (cols (length (car state))))
    (let y-loop ((y 0))
      (if (< y rows)
          (cons (let x-loop ((x 0))
                  (if (< x cols)
                      (cons (cell-update state x y)
                            (x-loop (+ x 1)))
                      '()))
                (y-loop (+ y 1)))
          '()))))


;;; Ncurses functions


(define* (set-state-array size #:key init-state)
  "Generates an array of zeros or sets to init-state if optional arg is provided"
  (let* ((rows (car size))
         (cols (cadr size)))
    (if init-state
        (let* ((init-rows (length init-state))
               (init-cols (length (car init-state)))
               (reduce-cols (lambda (state)
                              (map (lambda (row) (list-head row cols))
                                   state)))
               (expand-cols (lambda (state)
                              (map (lambda (row) (append row
                                                    (make-list (- cols init-cols) 0)))
                                   state)))
               (reduce-rows (lambda (state)
                              (list-head state rows)))
               (expand-rows (lambda (state)
                              (append state
                                      (make-list (- rows init-rows)
                                                 (make-list cols 0)))))
               (determine-cols (lambda (state)
                                 (cond ((> cols init-cols)
                                        (expand-cols state))
                                       ((< cols init-cols)
                                        (reduce-cols state))
                                       (else state)))))

          ;; Expand or reduce init-state to current dimensions
          (cond ((> rows init-rows)
                 (expand-rows (determine-cols init-state)))
                ((< rows init-rows)
                 (reduce-rows (determine-cols init-state)))
                (else (determine-cols init-state))))
        
        ;; return an array of zeros
        (make-list rows
                   (make-list cols 0)))))


(define (random-array size)
  "Generates a random array" 
  (let ((rows (car size))
        (cols (cadr size)))
    (let loop ((i rows) (arr '()))
      (if (> i 0)
          (loop (- i 1) (cons (map (lambda (_) (random 2))
                                   (make-list cols))
                              arr))
          arr))))


(define (draw-state screen state ch)
  "Draws the state array to the curses screen"
  (let ((rows (length state))
        (cols (length (car state))))
    (let loop ((s state) (row 0))
      (let inner-loop ((r (car s)) (col 0))
        (when (= (car r) 1)
          (addch screen ch #:y row #:x col))
        (when (not (null? (cdr r)))
          (inner-loop (cdr r) (+ col 1))))
      (if (not (null? (cdr s)))
          (loop (cdr s) (+ row 1))))))


(define (main args)
  "Main method"
  (let* ((stdscr (initscr))
         (cur-size (getmaxyx stdscr))
         (state (set-state-array cur-size #:init-state (random-array cur-size))))
    
    ;; Init ncurses methods
    (cbreak!)
    (noecho!)
    (curs-set 0)
    (nodelay! stdscr #t)
    (keypad! stdscr #t)

    ;; Init colors
    (when (has-colors?)
      (begin
        (start-color!)
        (init-pair! 0 COLOR_BLUE COLOR_BLACK)
        (init-pair! 1 COLOR_CYAN COLOR_BLACK)
        (init-pair! 2 COLOR_GREEN COLOR_BLACK)
        (init-pair! 3 COLOR_MAGENTA COLOR_BLACK)
        (init-pair! 4 COLOR_RED COLOR_BLACK)
        (init-pair! 5 COLOR_WHITE COLOR_BLACK)
        (init-pair! 6 COLOR_YELLOW COLOR_BLACK)))
    

    ;; (when (and (= (length args) 3)
    ;;            (= (length (member "-f" args)) 2))
    ;;     (let ((infile (read (open-input-file (list-ref args 2)))))
    ;;       (if (list? infile)
    ;;           (set! state (set-state-array cur-size
    ;;                                        #:init-state infile))
    ;;           (begin
    ;;             (format #t "usage: ~a -f <life-list-file>~%" (car args))
    ;;             (endwin)))))
    
    
    ;; game loop
    (let loop ((size (getmaxyx stdscr))
               (state state)
               (ch (getch stdscr)))
        
      ;; check to see if terminal has been resized
      (when (not (or (= (car size) (car cur-size))
                     (= (cadr size) (cadr cur-size))))
        (set! cur-size size)
        (set! state (set-state-array cur-size
                                     #:init-state state)))
      
      ;; clear the screen
      (clear stdscr)

      ;; draw state to screen
      (attr-on! stdscr (logior A_BOLD (color-pair 4)))
      (draw-state stdscr state (bold #\#))
      
      ;; refresh the screen
      (refresh stdscr)
      
      ;; sleep for a bit ...
      (usleep 50000)
      
      (cond ((eqv? ch #\q) (endwin))
            ((eqv? ch #\r) (loop (getmaxyx stdscr)
                                 (random-array size)
                                 (getch stdscr)))
            (else (loop (getmaxyx stdscr)
                        (update-board state)
                        (getch stdscr)))))))
