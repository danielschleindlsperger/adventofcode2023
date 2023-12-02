(use-modules (ice-9 regex))
(use-modules (ice-9 ports))
(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))

(define (slurp filename)
  (call-with-input-file filename get-string-all))

(define (first xs)
  (car xs))

(define (last xs)
  (car (reverse xs)))

(define example-input-part-1 "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")


(define (parse-lines s)
  (filter (lambda (x) (< 0 (string-length x)))
          (string-split s #\newline)))

(define digit-rx (make-regexp "[0-9]"))
(define (find-all-numbers-basic s) (let loop ((idx 0)
                                              (matches '()))
                                     (let ((match (regexp-exec digit-rx s idx)))
                                       (if match
                                           (loop (match:end match) (cons (match:substring match) matches))
                                           (reverse matches)))))

(define (find-calibration-values xs)
  (string->number (string-append (first xs) (last xs))))

(define (sum-of-list xs)
  (fold (lambda (x sum) (+ x sum)) 0 xs))

(sum-of-list (map (lambda (x) (find-calibration-values (find-all-numbers-basic x)))
                  (parse-lines example-input-part-1)))

(define (part-1-sum-calibration-values s)
  (sum-of-list (map
                (lambda (x) (find-calibration-values (find-all-numbers-basic x)))
                (parse-lines s))))

(part-1-sum-calibration-values example-input-part-1)
(part-1-sum-calibration-values (slurp "../inputs/day01.txt"))


;;;;;;; PART 2

(define example-input-part-2 "
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(define digit-rx-advanced (make-regexp "[0-9]|one|two|three|four|five|six|seven|eight|nine"))
(define (find-all-numbers-advanced s)
  (let loop ((idx 0)
             (matches '()))
    (let ((match (regexp-exec digit-rx-advanced s idx)))
      (if match
          (loop (match:end match) (cons (match:substring match) matches))
          (reverse matches)))))

(define (map-to-digits xs)
  (map
   (lambda (x)
     (display x )
     (display (eqv? "two" x))
     (cond
      ((string=? x "one") "1")
      ((string=? x "two") "2")
      ((string=? x "three") "3")
      ((string=? x "four") "4")
      ((string=? x "five") "5")
      ((string=? x "six") "6")
      ((string=? x "seven") "7")
      ((string=? x "eight") "8")
      ((string=? x "nine") "9")
      (else x)))
   xs))


(define (part-2-sum-calibration-values-corrected s)
  (sum-of-list (map
                (lambda (x) (find-calibration-values (map-to-digits (find-all-numbers-advanced x))))
                (parse-lines s))))

(part-2-sum-calibration-values-corrected example-input-part-2)
(part-2-sum-calibration-values-corrected (slurp "../inputs/day01.txt"))
