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

  (unless (null? filled-lines)
    (define depth (* (- field-height (length filled-lines)) square-size))
    (draw-pict (filled-lines->pict filled-lines) dc 0 depth))

  (when current-block                   ; will be #f at the end of the game
    (define x (* block-x square-size))
    (define y (* block-y square-size))
    (draw-pict (block->pict current-block) dc x y)))

(define canvas (new canvas% [parent frame]
                    [min-width window-width]
                    [min-height window-height]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [paint-callback on-tetris-paint]))

(define (on-tetris-tick)
  (when current-block                   ; will be #f at the end of the game
    (define inside? (inside-playing-field? current-block block-x (add1 block-y)))
    (define collision? (block-collision? current-block block-x (add1 block-y) filled-lines))

    (if (and inside? (not collision?))
        (set! block-y (add1 block-y))
        (spawn-new-block))

    (send canvas refresh)))

(define timer (new timer% [notify-callback on-tetris-tick] [interval 500]))

(define block-count -1)
(define (spawn-new-block)
  (when current-block
    (set! filled-lines (merge-block current-block block-x block-y filled-lines))
    (set! filled-lines (remove-full-lines filled-lines)))
  (define candidate (random (length all-blocks)))
  (set! current-block (list-ref all-blocks candidate))
  (set! block-y 0)
  (set! block-x (exact-truncate (- (/ field-width 2) 2)))
  ;; Playing field is full.  Game Over.
  (when (block-collision? current-block block-x block-y filled-lines)
    (set! current-block #f)))

(define (on-tetris-event event)
  (when current-block                   ; will be #f at the end of the game
    (case (send event get-key-code)
      ((left) (on-left-right-move sub1))
      ((right) (on-left-right-move add1))
      ((up) (on-rotation rotate-clockwise))
      ((down) (on-rotation rotate-counter-clockwise)))
    (send canvas refresh)))

(define (on-rotation rotate-function)
  (define candidate (rotate-function current-block))
  (define-values (min-x min-y max-x max-y) (block-bounding-box candidate))
  (cond
    ; rotating the block would make it collide, don't change it
    ((block-collision? candidate block-x block-y filled-lines)
     (void))
    ;; rotating the block would make it go below the field bottom, don't
    ;; change it.
    ((>= (+ block-y max-y) field-height)
     (void))
    (#t
     (define x (adjust-x-position candidate block-x block-y))
     ;; Bringing the block inside the playing field might make it collide, so
     ;; we need to check again for collisions.
     (unless (block-collision? candidate x block-y filled-lines)
       (set! current-block candidate)
       (set! block-x x)))))

(define (on-left-right-move direction)
  (when (and (inside-playing-field? current-block (direction block-x) block-y)
             (not (block-collision? current-block (direction block-x) block-y filled-lines)))
    (set! block-x (direction block-x))))

(define filled-lines '())

(define (start-game)
  (set! filled-lines '())
  (set! current-block #f)
  (spawn-new-block)
  (send canvas focus)
  (send frame show #t))

(define (inside-playing-field? block x y)
  (-> valid-block? integer? integer? boolean?)
  (define-values (min-x min-y max-x max-y)
    (block-bounding-box block))
  (and (< (+ x max-x) field-width)
       (>= (+ x min-x) 0)
       (< (+ y max-y) field-height)))

(define/contract (block-bounding-box block)
  (-> valid-block? (values integer? integer? integer? integer?))
  (define-values (min-x max-x)
    (for/fold ([min-x 3] [max-x 0])
              ([row (in-list block)])
      (define row-min-x (for/first ([(item position) (in-indexed (in-string row))]
                                    #:unless (equal? #\. item))
                          position))
      (define row-max-x (for/last ([(item position) (in-indexed (in-string row))]
                                   #:unless (equal? #\. item))
                          position))
      (values (if row-min-x (min min-x row-min-x) min-x)
              (if row-max-x (max max-x row-max-x) max-x))))

  (define min-y
    (for/first ([(row position) (in-indexed (in-list block))]
                #:unless (equal? row "...."))
      position))
  (define max-y
    (for/last ([(row position) (in-indexed (in-list block))]
               #:unless (equal? row "...."))
      position))
  (values min-x min-y max-x max-y))

(module+ test
  (define (bb-helper block rotations)
    (call-with-values (lambda () (block-bounding-box (rotate-clockwise* block rotations))) list))

  ;; Check that bounding boxes are correctly detected for all blocks and their
  ;; rotations.  Since there are 28 possibilities (7 blocks, 4 rotations
  ;; each), the `all-blocks-and-rotations` function was used to display the
  ;; block visually and determine what the bounding boxes should be.

  (check-equal? (bb-helper I-Block 0) '(1 0 1 3))
  (check-equal? (bb-helper I-Block 1) '(0 1 3 1))
  (check-equal? (bb-helper I-Block 2) '(2 0 2 3))
  (check-equal? (bb-helper I-Block 3) '(0 2 3 2))

  (check-equal? (bb-helper Q-Block 0) '(1 1 2 2))
  (check-equal? (bb-helper Q-Block 1) '(1 1 2 2))
  (check-equal? (bb-helper Q-Block 2) '(1 1 2 2))
  (check-equal? (bb-helper Q-Block 3) '(1 1 2 2))

  (check-equal? (bb-helper L-Block 0) '(0 0 1 2))
  (check-equal? (bb-helper L-Block 1) '(1 0 3 1))
  (check-equal? (bb-helper L-Block 2) '(2 1 3 3))
  (check-equal? (bb-helper L-Block 3) '(0 2 2 3))

  (check-equal? (bb-helper J-Block 0) '(1 0 2 2))
  (check-equal? (bb-helper J-Block 1) '(1 1 3 2))
  (check-equal? (bb-helper J-Block 2) '(1 1 2 3))
  (check-equal? (bb-helper J-Block 3) '(0 1 2 2))

  (check-equal? (bb-helper T-Block 0) '(0 0 2 1))
  (check-equal? (bb-helper T-Block 1) '(2 0 3 2))
  (check-equal? (bb-helper T-Block 2) '(1 2 3 3))
  (check-equal? (bb-helper T-Block 3) '(0 1 1 3))

  (check-equal? (bb-helper Z-Block 0) '(0 0 1 2))
  (check-equal? (bb-helper Z-Block 1) '(1 0 3 1))
  (check-equal? (bb-helper Z-Block 2) '(2 1 3 3))
  (check-equal? (bb-helper Z-Block 3) '(0 2 2 3))

  (check-equal? (bb-helper S-Block 0) '(0 0 1 2))
  (check-equal? (bb-helper S-Block 1) '(1 0 3 1))
  (check-equal? (bb-helper S-Block 2) '(2 1 3 3))
  (check-equal? (bb-helper S-Block 3) '(0 2 2 3)))

(define/contract (adjust-x-position block x y)
  (-> valid-block? integer? integer? integer?)
  (define-values (min-x min-y max-x max-y)
    (block-bounding-box block))
  (if (< (+ y max-y) field-height)
      (let loop ([x x])
        (if (inside-playing-field? block x y)
            x
            (loop (if (>= x 0) (sub1 x) (add1 x)))))
      x))

(define (valid-filled-line? line)
  (and (string? line)
       (= (string-length line) field-width)
       (for/and ([item (in-string line)])
         (and (member item '(#\. #\I #\Q #\L #\J #\T #\Z #\S)) #t))))

(define/contract (filled-lines->pict lines)
  (-> (listof valid-filled-line?) pict?)
  (apply vc-append (map row->squares lines)))

(define/contract (block-row->filled-line row x-position)
  (-> valid-block-row? integer? valid-filled-line?)
  (define limit (+ x-position (string-length row)))
  (define items
    (for/list ([pos (in-range field-width)])
      (if (or (< pos x-position) (>= pos limit))
          #\.
          (string-ref row (- pos x-position)))))
  (apply string items))

(define/contract (merge-lines line1 line2)
  (-> valid-filled-line? valid-filled-line? valid-filled-line?)
  (define items
    (for/list ([a (in-string line1)]
               [b (in-string line2)])
      (cond ((equal? a #\.) b)
            ((equal? b #\.) a)
            (#t (error (format "Line collision: ~a vs ~a" line1 line2))))))
  (apply string items))

(define/contract (block-row-with-line-collision? block-row x line)
  (-> valid-block-row? integer? valid-filled-line? boolean?)
  (define bline (block-row->filled-line block-row x))
  (with-handlers
    ((exn:fail? (lambda (e) #t)))
    (merge-lines bline line)
    #f))

(define/contract (block-collision? block x y filled-lines)
  (-> valid-block? integer? integer? (listof valid-filled-line?) boolean?)
  (let loop ([bdepth y]
             [block block]
             [fdepth (- field-height (length filled-lines))]
             [filled filled-lines])
    (cond ((or (null? block) (null? filled))
           #f)
          ((< bdepth fdepth)
           (loop (add1 bdepth) (cdr block) fdepth filled))
          ((> bdepth fdepth)
           (loop bdepth block (add1 fdepth) (cdr filled)))
          (#t
           (if (block-row-with-line-collision? (car block) x (car filled))
               #t
               (loop (add1 bdepth) (cdr block) (add1 fdepth) (cdr filled)))))))

(define empty-line (make-string field-width #\.))

(define (maybe-add line result)
  (if (and (equal? line empty-line) (null? result))
      result
      (cons line result)))

(define (merge-block block x y filled-lines)
  (let loop ([bdepth y]
             [block block]
             [fdepth (- field-height (length filled-lines))]
             [filled filled-lines]
             [result '()])
    (cond ((< bdepth fdepth)
           ;; Block row is above filled lines, create new filled lines at the top.
           (let ([line (block-row->filled-line (car block) x)])
             (loop (add1 bdepth) (cdr block)
                   fdepth filled
                   (maybe-add line result))))
          ((> bdepth fdepth)
           ;; Filled lines are above the block row, just add them to the
           ;; result, no merging is needed
           (loop y block
                 (add1 fdepth) (cdr filled)
                 (cons (car filled) result)))
          ((>= fdepth field-height)
           ;; Filled lines depth is now greater than the field depth -- we're done.
           (reverse result))
          ((null? block)
           ;; We're done with the block rows, just add the remaining filled lines.
           (loop (add1 bdepth) block
                 (add1 fdepth) (cdr filled)
                 (cons (car filled) result)))
          (#t
           ;; The block row is at the same level as a filled line.  Merge
           ;; them, to create a new line
           (let* ([bline (block-row->filled-line (car block) x)]
                  [line (merge-lines (car filled) bline)])
             (loop (add1 bdepth) (cdr block) (add1 fdepth) (cdr filled)
                   (maybe-add line result)))))))

(define/contract (full-line? line)
  (-> valid-filled-line? boolean?)
  (for/and ([char (in-string line)])
    (not (equal? #\. char))))

(define (remove-full-lines filled-lines)
  (-> (listof valid-filled-line?) (listof valid-filled-line?))
  (for/list ([line (in-list filled-lines)] #:unless (full-line? line))
    line))
