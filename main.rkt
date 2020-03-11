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


;; ............................... Frame, canvas & events

(define tetris-frame%
  (class frame%
    (init) (super-new)
    (define/override (on-subwindow-char receiver event)
      (define handled? (super on-subwindow-char receiver event))
      (if handled?
          #t
          (on-tetris-event event)))))

(define-values (field-width field-height) (values 12 16))
(define-values (window-width window-height)
  (values (* field-width square-size) (* field-height square-size)))

(define frame
  (new tetris-frame% [label "Tetris"] [width window-width] [height window-height]))

(define-values (current-block block-x block-y) (values L-Block 0 0))

(define (on-tetris-paint canvas dc)
  (send dc clear)
  (define x (* block-x square-size))
  (define y (* block-y square-size))
  (draw-pict (block->pict current-block) dc x y))

(define canvas (new canvas% [parent frame]
                    [min-width window-width]
                    [min-height window-height]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [paint-callback on-tetris-paint]))

(define (on-tetris-tick)
  (if (< block-y field-height)
      (set! block-y (add1 block-y))
      (spawn-new-block))
  (send canvas refresh))

(define timer (new timer% [notify-callback on-tetris-tick] [interval 500]))

(define block-count -1)
(define (spawn-new-block)
  (set! block-count (add1 block-count))
  (set! current-block (list-ref
                       all-blocks
                       (modulo block-count (length all-blocks))))
  (set! block-y 0)
  (set! block-x (exact-truncate (- (/ field-width 2) 2))))

(define (on-tetris-event event)
  (case (send event get-key-code)
    ((left) (on-left-right-move sub1))
    ((right) (on-left-right-move add1))
    ((up) (on-rotation rotate-clockwise))
    ((down) (on-rotation rotate-counter-clockwise)))
  (send canvas refresh))

(define (on-rotation rotate-function)
  (set! current-block (rotate-function current-block)))

(define (on-left-right-move direction)
  (set! block-x (direction block-x)))


(define (start-game)
  (spawn-new-block)
  (send canvas focus)
  (send frame show #t))
