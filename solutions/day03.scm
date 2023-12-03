(use-modules (ice-9 regex))
(use-modules (ice-9 ports))
(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-43))

(define (slurp filename)
  (call-with-input-file filename get-string-all))

(define (first xs)
  (car xs))

(define (last xs)
  (car (reverse xs)))

(define (flat-map func lst)
  (apply append (map func lst)))

(define (sum-of-list xs)
  (fold (lambda (x sum) (+ x sum)) 0 xs))

(define example-input-part-1 "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(define (parse-rows s)
  (filter (lambda (x) (< 0 (string-length x)))
          (string-split s #\newline)))

(define (string->vector s)
  (list->vector (string->list s)))

(define (parse-grid s)
  (let ((rows (parse-rows s)))
    (list->array 1 (map string->vector rows))))

(define-record-type coord
  (make-coord x y)
  coord?
  (x coord-x)
  (y coord-y))

(define (coord-move-left coord)
  (make-coord (1- (coord-x coord))
              (coord-y coord)))

(define (coord-move-right coord)
  (make-coord (1+ (coord-x coord))
              (coord-y coord)))

(define-record-type part-number
  (make-part-number coord string-content)
  part-number?
  (coord part-number-coord)
  (string-content part-number-string-content))

(define (find-part-number-candidates grid)
  (vector-fold (lambda (y candidates row)
                (vector-fold (lambda (x candidates cell)
                               (display (list x y cell))
                               (newline)
                               (let ((part-num (part-number-at grid (make-coord x y))))
                                 (if part-num
                                     (cons part-num candidates)
                                     candidates))
                            )
                             candidates
                          row))
               '()
              grid))

(define (out-of-bounds grid coord)
  (and (< (coord-y coord) (vector-length grid))
       (or (= 0 (coord-x coord)
              (< (coord-x coord) (vector-length (vector-ref grid 0)))))))

;; return #f when out of bounds
(define (value-at grid coord)
  (if (out-of-bounds grid coord)
      #f
      (vector-ref
       (vector-ref grid (coord-y coord))
       (coord-x coord))

      )
  )

(define (char-digit? c)
  (and (char? c)
       (<= 48 (char->integer c) 57)))

(define (part-number-value grid coord)
  (let loop ((value (string (value-at grid coord)))
             (coord (coord-move-right coord)))
    (display (list coord value))
    (newline)
    (if (and (not (out-of-bounds grid coord))
             (char-digit? (value-at grid coord)))
        (loop (string-append value (string (value-at grid coord)))
              (coord-move-right coord)) value
        )))

(define (part-number-at grid coord)
  (display (value-at grid coord))
  (newline)
  (display (char-digit? (value-at grid coord)))
  (newline)
  (newline)
    ;; no number to the left
    ;; number on the current coord
    ;; look ahead to find complete length of the number
    ;; otherwise: #f
    (if (and (char-digit? (value-at grid coord))
             (not (char-digit? (value-at grid (coord-move-left coord)))))
        (make-part-number coord "123" #;(part-number-value grid coord))
        #f))

(define (part-number? grid part-num)
  ;; check around the coord
  ;; check grid bounds
  ;; maybe add caching?
  #f
  )

(define (part-1-sum-of-part-numbers s)
  (let ((grid (parse-grid s)))
    (display (reverse (find-part-number-candidates grid)))
    )
  )

(part-1-sum-of-part-numbers example-input-part-1)
(part-1-sum-of-part-numbers (slurp "../inputs/day03.txt"))
