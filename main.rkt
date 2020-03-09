#lang racket/gui

(require pict racket/draw racket/contract)
(module+ test (require rackunit))

;; ............................... Tetris block definitions.

(define I-Block
  '(".I.."
    ".I.."
    ".I.."
    ".I.."))

(define Q-Block
  '("...."
    ".QQ."
    ".QQ."
    "...."))

(define L-Block
  '("LL.."
    ".L.."
    ".L.."
    "...."))

(define J-Block
  '(".JJ."
    ".J.."
    ".J.."
    "...."))

(define T-Block
  '(".T.."
    "TTT."
    "...."
    "...."))

(define Z-Block
  '(".Z.."
    "ZZ.."
    "Z..."
    "...."))

(define S-Block
  '("S..."
    "SS.."
    ".S.."
    "...."))

(define all-blocks (list I-Block Q-Block L-Block J-Block T-Block Z-Block S-Block))

(define (valid-block-row? row)
  (and (string? row)
       (= (string-length row) 4)
       (for/and ([item (in-string row)])
         (and (member item '(#\. #\I #\Q #\L #\J #\T #\Z #\S)) #t))))

(define (valid-block? block)
  (and (list? block)
       (= (length block) 4)
       (andmap valid-block-row? block)))

(module+ test
  (check-false (valid-block-row? 1))
  (check-false (valid-block-row? "......"))
  (check-false (valid-block-row? "X..."))

  (check-false (valid-block? "hello"))
  (check-false (valid-block? (append L-Block T-Block)))
  (check-false (valid-block? (list "...." "...." "...." 1)))
  (check-false (valid-block? (list "X..." "...." "...." "....")))

  (for ([block (in-list all-blocks)])
    (check-pred valid-block? block)))


;; ............................... Block graphics.

(define square-size 30)
(define colors
  (hash
   #\I (make-color 0 119 187)
   #\Q (make-color 51 187 238)
   #\L (make-color 0 153 136)
   #\J (make-color 238 119 51)
   #\T (make-color 204 51 17)
   #\Z (make-color 238 51 119)
   #\S (make-color 136 34 85)))

(define/contract (block->pict block)
  (-> valid-block? pict?)
  (apply vc-append (map row->squares block)))

(define/contract (row->squares row)
  (-> string? pict?)
  (define items
    (for/list ([char (in-string row)])
      (define color (hash-ref colors char #f))
      (if color
          (filled-rectangle square-size square-size #:color color)
          (rectangle square-size square-size))))
  (apply hc-append items))


;; ............................... Block rotations.

(define/contract (rotate-clockwise block)
  (-> valid-block? valid-block?)
  (for/list ([a (in-string (first block))]
             [b (in-string (second block))]
             [c (in-string (third block))]
             [d (in-string (fourth block))])
    (string d c b a)))

(define/contract (all-rotations block)
  (-> valid-block? (listof valid-block?))
  (reverse
   (for/fold ([rotations (list block)])
             ([n (in-range 3)])
     (cons (rotate-clockwise (car rotations)) rotations))))

(define/contract (rotate-clockwise* block times)
  (-> valid-block? exact-nonnegative-integer? valid-block?)
  (if (> times 0)
      (let ([rotated (rotate-clockwise block)])
        (rotate-clockwise* rotated (sub1 times)))
      block))

(define/contract (rotate-counter-clockwise block)
  (-> valid-block? valid-block?)
  (rotate-clockwise* block 3))

(module+ test
  (for ([block (in-list all-blocks)])
    (check-equal? (rotate-clockwise* block 4) block)
    (check-equal? (rotate-clockwise (rotate-counter-clockwise block)) block)))

