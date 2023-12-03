(use-modules (ice-9 regex))
(use-modules (ice-9 ports))
(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))

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
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(define (parse-lines s)
  (filter (lambda (x) (< 0 (string-length x)))
          (string-split s #\newline)))

(define-record-type game
  (make-game id reveals)
  game?
  (id game-id)
  (reveals game-reveals))

(define-record-type reveal
  (make-reveal red-count green-count blue-count)
  reveal?
  (red-count reveal-red-count reveal-set-red-count!)
  (green-count reveal-green-count reveal-set-green-count!)
  (blue-count reveal-blue-count reveal-set-blue-count!))

(define (parse-reveal-for-color s color)
  ;; TODO: assert
  (let* ((regexp (string-append "([0-9]+) " color))
        (match (string-match regexp s)))
    (if match
        (string->number (match:substring match 1))
        0)))

(define (parse-reveal s)
  (let ((red-count (parse-reveal-for-color s "red"))
        (green-count (parse-reveal-for-color s "green"))
        (blue-count (parse-reveal-for-color s "blue")))
    (make-reveal red-count green-count blue-count)))

(define (parse-game s) (map parse-reveal (string-split s #\;)))
(define (parse-games lines)
  (map
   (lambda (line)
     (let* ((match (string-match "Game ([0-9]+): (.+)" line))
             (id (match:substring  match 1))
             (set-string (match:substring  match 2)))
       (make-game (string->number id) (parse-game set-string))))
   lines))

(define (game-possible? game spec)
  (every (lambda (reveal)
           (and (<= (reveal-red-count reveal) (reveal-red-count spec))
                (<= (reveal-green-count reveal) (reveal-green-count spec))
                (<= (reveal-blue-count reveal) (reveal-blue-count spec))))
          (game-reveals game)))

(define (part-1-sum-of-ids s)
  (let* ((games (parse-games (parse-lines s)))
         (game-spec (make-reveal 12 13 14))
         (possible-games (filter (lambda (game) (game-possible? game game-spec)) games)))
    (sum-of-list (map game-id possible-games))))

(part-1-sum-of-ids example-input-part-1)
(part-1-sum-of-ids (slurp "../inputs/day02.txt"))

;;;;;;;;;; PART 2
;;;;;;;;;;

(define (fewest-cubes-possible game)
  (let ((game-spec (make-reveal 0 0 0)))
    (for-each (lambda (reveal)
                (reveal-set-red-count! game-spec (max (reveal-red-count game-spec) (reveal-red-count reveal)))
                (reveal-set-green-count! game-spec (max (reveal-green-count game-spec) (reveal-green-count reveal)))
                (reveal-set-blue-count! game-spec (max (reveal-blue-count game-spec) (reveal-blue-count reveal))))
              (game-reveals game))
    game-spec))

(define (compute-power reveal)
  (* (reveal-red-count reveal)
     (reveal-green-count reveal)
     (reveal-blue-count reveal)))

(define (part-2-sum-of-powers s)
  (let* ((games (parse-games (parse-lines s))))
    (sum-of-list (map compute-power (map fewest-cubes-possible games)))))

(part-2-sum-of-powers example-input-part-1)
(part-2-sum-of-powers (slurp "../inputs/day02.txt"))
