;; utils.clar
;; Safe-math & time helpers for escrow & arbitration protocol

;; Error codes
(define-constant ERR-ARITHMETIC-OVERFLOW u5001)
(define-constant ERR-DIVISION-BY-ZERO u5002)
(define-constant ERR-INVALID-INPUT u5003)

;; Time constants (in blocks)
(define-constant BLOCKS-PER-MINUTE u0.1) ;; ~10 blocks per minute
(define-constant BLOCKS-PER-HOUR u6) ;; ~6 blocks per hour
(define-constant BLOCKS-PER-DAY u144) ;; ~144 blocks per day
(define-constant BLOCKS-PER-WEEK u1008) ;; ~1008 blocks per week
(define-constant BLOCKS-PER-MONTH u4320) ;; ~4320 blocks per month (30 days)

;; Safe math functions
(define-read-only (safe-add (a uint) (b uint))
  (let
    (
      (result (+ a b))
    )
    (if (< result a)
      (err ERR-ARITHMETIC-OVERFLOW)
      (ok result))
  )
)

(define-read-only (safe-sub (a uint) (b uint))
  (if (< a b)
    (err ERR-ARITHMETIC-OVERFLOW)
    (ok (- a b))
  )
)

(define-read-only (safe-mul (a uint) (b uint))
  (let
    (
      (result (* a b))
    )
    (if (and (> a u0) (< (/ result a) b))
      (err ERR-ARITHMETIC-OVERFLOW)
      (ok result)
    )
  )
)

(define-read-only (safe-div (a uint) (b uint))
  (if (is-eq b u0)
    (err ERR-DIVISION-BY-ZERO)
    (ok (