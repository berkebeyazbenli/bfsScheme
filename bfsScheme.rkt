#lang racket

;; Task 1: Defining getHeight and getWidth
(define (getHeight path)
  (length path))

(define (getWidth path)
  (length (car path)))

;; Task 2: Defining getLetter
(define (getLetter path row col)
  (list-ref (list-ref path row) col))

;; Task 3: Defining solvePath using BFS
(define (solvePath path)
  (let ((start (find-start path)))
    (if start
        (let ((result (bfs path start)))
          (if result
              (map direction-to-char result)
              '()))
        '())))

(define (direction-to-char dir)
  (case dir
    ((U) 'U)
    ((D) 'D)
    ((L) 'L)
    ((R) 'R)))

(define (find-start path)
  (let loop ((i 0) (path path))
    (if (null? path)
        #f
        (let ((row (car path)))
          (let loop-row ((j 0) (row row))
            (cond
              ((null? row) (loop (+ i 1) (cdr path)))
              ((equal? "S" (car row)) (list i j))
              (else (loop-row (+ j 1) (cdr row)))))))))

(define (bfs path start)
  (define (neighbors pos)
    (list (cons 'D (move pos 'D))
          (cons 'R (move pos 'R))
          (cons 'U (move pos 'U))
          (cons 'L (move pos 'L))))
  (define (valid-pos? pos)
    (and (not (out-of-bounds? path pos))
         (not (wall? path pos))))
  (let loop ((queue (list (list '() start))) (visited '()))
    (if (null? queue)
        #f
        (let* ((current (car queue))
               (current-path (car current))
               (current-pos (cadr current)))
          (if (finish? path current-pos)
              (reverse current-path)
              (let ((new-visited (cons current-pos visited))
                    (next-positions (filter (lambda (n) (and (valid-pos? (cdr n)) (not (member (cdr n) visited))))
                                            (neighbors current-pos))))
                (loop (append (cdr queue) (map (lambda (n) (list (cons (car n) current-path) (cdr n))) next-positions))
                      new-visited)))))))

(define (out-of-bounds? path pos)
  (let ((x (car pos)) (y (cadr pos)))
    (or (< x 0) (< y 0) (>= x (getHeight path)) (>= y (getWidth path)))))

(define (wall? path pos)
  (equal? (getLetter path (car pos) (cadr pos)) "-"))

(define (finish? path pos)
  (equal? (getLetter path (car pos) (cadr pos)) "F"))

(define (move pos direction)
  (let ((x (car pos))
        (y (cadr pos)))
    (case direction
      ((U) (list (- x 1) y))
      ((D) (list (+ x 1) y))
      ((L) (list x (- y 1)))
      ((R) (list x (+ y 1))))))

;; Function to building the path
(define (buildPath rows)
  (cond
    ((null? rows) null)
    (else (cons (car rows) (buildPath (cdr rows))))))

;; Example dynamic path
(define sample-path
 (buildPath '(("S" "-" "-" "-" "-")
               ("E" "-" "-" "-" "-")
               ("E" "E" "E" "E" "E")
               ("-" "-" "-" "-" "E")
               ("-" "-" "-" "-" "F"))))


;; Dynamic path example with different dimensions
;;(define sample-path2
  ;;(buildPath '(("S" "-" "E" "E" "E" "-")
    ;;           ("E" "-" "E" "-" "E" "-")
      ;;         ("E" "E" "E" "-" "E" "E")
        ;;       ("-" "-" "E" "-" "-" "E")
          ;;     ("-" "-" "E" "-" "-" "E")
            ;;   ("-" "-" "E" "E" "E" "F"))))




;; Testing the functions

;;(display (getHeight sample-path2)) ; should return 6
;;(newline)
;;(display (getWidth sample-path2)) ; should return 6
;;(newline)
;;(display (getLetter sample-path2 0 0)) ; should return "S"
;;(newline)
;;(display (getLetter sample-path2 1 0)) ; should return "E"
;;(newline)
;;(display (getLetter sample-path2 1 1)) ; should return "-"
;;(newline)
;;(display (getLetter sample-path2 5 5)) ; should return "F"
;;(newline)
;;(display (solvePath sample-path2)) ; should return '(D D R R D D D R R R)
;;(newline)

(display (getHeight sample-path)) ; should return 5
(newline)
(display (getWidth sample-path)) ; should return 5
(newline)
(display (getLetter sample-path 0 0)) ; should return "S"
(newline)
(display (getLetter sample-path 1 0)) ; should return "E"
(newline)
(display (getLetter sample-path 1 1)) ; should return "-"
(newline)
(display (getLetter sample-path 4 4)) ; should return "F"
(newline)
(display (solvePath sample-path)) ; should return the correct path for sample-path2
(newline)
