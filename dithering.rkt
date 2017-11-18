#lang racket

(require racket/draw)

; Fred Martin, fred_martin@uml.edu
; Nov 18 2017

; exported functions are:
; bitmap->floyd-steinberg
; bitmap->burkes
;
; each accepts an alpha-red-green-blue "argb" bitmap
; and returns a dithered bitmap also in argb format,
; but with each pixel either pure white or pure black.

; these exprs are demo/test code (e.g. type them into REPL):
;
; (require net/url)
; (define flowers
;  (read-bitmap
;   (get-pure-port
;    (string->url "https://www.almanac.com/sites/default/files/birth_month_flowers-primary-1920x1280px_pixabay.jpg"))
;    #:backing-scale 3.))
; flowers
; (bitmap->floyd-steinberg flowers)
; (bitmap->burkes flowers)

; accepts a bitmap, outputs vector of grayscale values in range 0. to 1.
; adds an extra row + 1 so the floyd-steinberg can complete.
; adds another one for burkes
(define (bitmap->grayscale-vector bm)
  (define (gray pixels pos)
    (/ (+ (* 0.2126 (bytes-ref pixels (+ pos 1)))
          (* 0.7152 (bytes-ref pixels (+ pos 2)))
          (* 0.0722 (bytes-ref pixels (+ pos 3))))
       255.))
  (let* ((width (send bm get-width))
         (height (send bm get-height))
         (pixels (make-bytes (* 4 width height)))
         (grays (make-vector (+ (* width (add1 height)) 2) 0.)))
    (begin
      (send bm get-argb-pixels 0 0 (sub1 width) (sub1 height) pixels)
      (for ((y (in-range 0 height)))
        (for ((x (in-range 0 width)))
          (vector-set! grays (+ x (* y width))
                       (gray pixels (+ (* x 4) (* y width 4))))))
      grays)))

; this version outputs nums from 0 to 255 instead of floats.
(define (bitmap->grayscale-vector-int bm)
  (define (gray pixels pos)
    (inexact->exact
     (round (+ (* 0.2126 (bytes-ref pixels (+ pos 1)))
               (* 0.7152 (bytes-ref pixels (+ pos 2)))
               (* 0.0722 (bytes-ref pixels (+ pos 3)))))))
  (let* ((width (send bm get-width))
         (height (send bm get-height))
         (pixels (make-bytes (* 4 width height)))
         (grays (make-vector (+ (* width (add1 height)) 2) 0)))
    (begin
      (send bm get-argb-pixels 0 0 (sub1 width) (sub1 height) pixels)
      (for ((y (in-range 0 height)))
        (for ((x (in-range 0 width)))
          (vector-set! grays (+ x (* y width))
                       (gray pixels (+ (* x 4) (* y width 4))))))
      grays)))

; Floyd-Steinberg pseudocode from Wikipedia
; https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
; 
; for each y from top to bottom
;    for each x from left to right
;       oldpixel  := pixel[x][y]
;       newpixel  := find_closest_palette_color(oldpixel)
;       pixel[x][y]  := newpixel
;       quant_error  := oldpixel - newpixel
;       pixel[x + 1][y    ] := pixel[x + 1][y    ] + quant_error * 7 / 16
;       pixel[x - 1][y + 1] := pixel[x - 1][y + 1] + quant_error * 3 / 16
;       pixel[x    ][y + 1] := pixel[x    ][y + 1] + quant_error * 5 / 16
;       pixel[x + 1][y + 1] := pixel[x + 1][y + 1] + quant_error * 1 / 16

; mutates vector v based on floyd-steinberg algorithm
; assumes v has values from 0. to 1.
(define (floyd-steinberg v width height)
  (for ((y (in-range 0 height)))
    (for ((x (in-range 0 width)))
      (let* ((oldpx (vector-ref v (+ (* y width) x)))
             (newpx (if (> oldpx 0.5) 1. 0.))
             (error (- oldpx newpx)))
        (begin
          (vector-set! v (+ (* y width) x) newpx)
          (vector-set! v (+ (add1 x) (* y width))
                       (+ (/ (* 7 error) 16) (vector-ref v (+ (add1 x) (* y width)))))
          (vector-set! v (+ (sub1 x) (* (add1 y) width))
                       (+ (/ (* 3 error) 16) (vector-ref v (+ (sub1 x) (* (add1 y) width)))))
          (vector-set! v (+ x (* (add1 y) width))
                       (+ (/ (* 5 error) 16) (vector-ref v (+ x (* (add1 y) width)))))
          (vector-set! v (+ (add1 x) (* (add1 y) width))
                       (+ (/      error  16) (vector-ref v (+ (add1 x) (* (add1 y) width))))))))))            

; Burkes dithering, as described at
; http://www.tannerhelland.com/4660/dithering-eleven-algorithms-source-code/
;
;             X   8   4 
;     2   4   8   4   2
;
;           (1/32)

; this version operates on the floating point [0., 1.] grayscale vector
(define (burkes v width height)
  (for ((y (in-range 0 height)))
    (for ((x (in-range 0 width)))
      (let* ((oldpx (vector-ref v (+ (* y width) x)))
             (newpx (if (> oldpx 0.5) 1. 0.))
             (error (- oldpx newpx)))
        (begin
          (vector-set! v (+ (* y width) x) newpx)
          (vector-set! v (+ (+ x 1) (* y width))
                       (+ (/ (* 8 error) 32) (vector-ref v (+ (+ x 1) (* y width)))))
          (vector-set! v (+ (+ x 2) (* y width))
                       (+ (/ (* 4 error) 32) (vector-ref v (+ (+ x 2) (* y width)))))
          (vector-set! v (+ (- x 2) (* (add1 y) width))
                       (+ (/ (* 2 error) 32) (vector-ref v (+ (- x 2) (* (add1 y) width)))))
          (vector-set! v (+ (- x 1) (* (add1 y) width))
                       (+ (/ (* 4 error) 32) (vector-ref v (+ (- x 1) (* (add1 y) width)))))
          (vector-set! v (+ x (* (add1 y) width))
                       (+ (/ (* 8 error) 32) (vector-ref v (+ x (* (add1 y) width)))))
          (vector-set! v (+ (+ x 1) (* (add1 y) width))
                       (+ (/ (* 4 error) 32) (vector-ref v (+ (+ x 1) (* (add1 y) width)))))
          (vector-set! v (+ (+ x 2) (* (add1 y) width))
                       (+ (/ (* 2 error) 32) (vector-ref v (+ (+ x 2) (* (add1 y) width))))))))))

; this version operates on the integer bitmap vector [0, 255]
(define (burkes-int v width height)
  (for ((y (in-range 0 height)))
    (for ((x (in-range 0 width)))
      (let* ((oldpx (vector-ref v (+ (* y width) x)))
             (newpx (if (> oldpx 127) 255 0))
             (error (- oldpx newpx)))
        (begin
          (vector-set! v (+ (* y width) x) newpx)
          (vector-set! v (+ (+ x 1) (* y width))
                       (+ (arithmetic-shift (arithmetic-shift error 3) -5)
                          (vector-ref v (+ (+ x 1) (* y width)))))
          (vector-set! v (+ (+ x 2) (* y width))
                       (+ (arithmetic-shift (arithmetic-shift error 2) -5)
                          (vector-ref v (+ (+ x 2) (* y width)))))
          (vector-set! v (+ (- x 2) (* (add1 y) width))
                       (+ (arithmetic-shift (arithmetic-shift error 1) -5)
                          (vector-ref v (+ (- x 2) (* (add1 y) width)))))
          (vector-set! v (+ (- x 1) (* (add1 y) width))
                       (+ (arithmetic-shift (arithmetic-shift error 2) -5)
                          (vector-ref v (+ (- x 1) (* (add1 y) width)))))
          (vector-set! v (+ x (* (add1 y) width))
                       (+ (arithmetic-shift (arithmetic-shift error 3) -5)
                          (vector-ref v (+ x (* (add1 y) width)))))
          (vector-set! v (+ (+ x 1) (* (add1 y) width))
                       (+ (arithmetic-shift (arithmetic-shift error 2) -5)
                          (vector-ref v (+ (+ x 1) (* (add1 y) width)))))
          (vector-set! v (+ (+ x 2) (* (add1 y) width))
                       (+ (arithmetic-shift (arithmetic-shift error 1) -5)
                          (vector-ref v (+ (+ x 2) (* (add1 y) width))))))))))

; convert the grayscale vector back to a bitmap
; works for either float or integer representation of vector
; because floating point zero = integer zero:
;                  (rgb (if (= 0 (vector-ref v vix)) 0 255))
(define (grayscale-vector->bitmap v width height)
  (let ((pixels (make-bytes (* 4 width height)))
        (bm (make-bitmap width height)))
    (begin
      (for ((y (in-range 0 height)))
        (for ((x (in-range 0 width)))
          (let* ((vix (+ (* y width) x))
                 (rgb (if (= 0 (vector-ref v vix)) 0 255))
                 (pix (* 4 vix)))
            (begin
              (bytes-set! pixels pix 255) ; alpha
              (bytes-set! pixels (add1 pix) rgb) ; r
              (bytes-set! pixels (+ 2 pix) rgb) ; g
              (bytes-set! pixels (+ 3 pix) rgb))))) ;b
      (send bm set-argb-pixels 0 0 (sub1 width) (sub1 height) pixels)
      bm)))

; use this function
(define (bitmap->floyd-steinberg bm)
  (let ((width (send bm get-width))
        (height (send bm get-height))
        (v (bitmap->grayscale-vector bm)))
    (begin
      (floyd-steinberg v width height)
      (grayscale-vector->bitmap v width height))))

; use this function
(define (bitmap->burkes bm)
  (let ((width (send bm get-width))
        (height (send bm get-height))
        (v (bitmap->grayscale-vector-int bm)))
    (begin
      (burkes-int v width height)
      (grayscale-vector->bitmap v width height))))
