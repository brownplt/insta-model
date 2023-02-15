#lang at-exp slideshow

;; ?? min talk, ?? min qa
;; what time of day?
;; [ ] start with PLDI intro
;;     4 camps
;;     research focus: 1-2, industry focus: 3
;;     today focus = 4
;;     a few papers ... compelling result
;;     and now a stellar implementation
;; [ ] context (what the heck are these N worlds):
;;      TR, soundnesss, death traps
;;      Retic, weak soundness, still death traps!
;;      ... gotta explain the strategies a bit
;; [ ] SP checks
;;  - constant size
;;  - 
;; 
;; [ ] how they did it?
;;     - python slow! lots of room with numbers simple ops (cpython)
;;     - ??? something else
;;     - concrete data structures
;; [ ] hilites
;;   - non-GG
;;     - fig 2a: override type with Dyn in typed class
;;       coarse granularity
;;     - fig 2b: chkdict, caller must have type
;;     - opt-in is the key: make the most! light at end!
;;       instead of all this theory work about fine-grained, focus on semantics and performance
;;      ** payoff in vtable lookup
;;   - primitive ~= dynamic
;;     - ok for module boundaries, not for typed code
;;     - again, gg violation, again ok because opt-in
;;   - gradual class hierarchy
;;     - rare in RW: Thorn, SafeTS don't allow it
;;     - Nom allows plenty, makes dispatch tricky, optimistic / pess.
;;     - no m overloading (python)
;;     - single inheritance
;;     - typed code, type-based override
;;     - no @property for typed fields
;;     - attrs set by __init__ (all PEP checkers want this)
;;   - first order functions, classes, objects; fall back to python for the rest
;;   - table for all generic/concrete instantiations
;;   - CPU efficiency
;;     - how to interpret
;;     - 
;;   - how to edit code for migration, what edits happened: for types, for perf.
;; [ ] formalization
;;   - SP test -> redex test -> check!
;;     265 total, 52 tuned, rest auto. Omit 537 tests.
;;   - 5 soundness bugs, fixed!
;;   - 16 correctness issues, example in paper sec 4
;;   - model: S types and T types 
;;   - typing rules, D vs CD? maybe show earlier ... maybe refer to earlier down here
;;   - cast insertion example; SP approximation; ... open world python
;; [ ] more small hilites
;;   - pep 484 compatible, but a subset, no plans to cover all
;;     can always run Pyre!
;;   - limited concretes at the moment
;;   - no unions at runtime, only optional
;;   - no recursive types anywhere (Dyn for that, as default like for Set[T]?)
;;   - no **kwargs, coming soon
;;   - per-method JIT
;; [ ] summary
;;   - no GG, no eval, no mult.inhr., no first-class class, no overload
;;   - 

;; [ ] TODO ask SP for current events! calls for help!


(require
  images/icons/misc
  images/icons/symbol
  images/icons/style
  images/icons/file
  images/icons/control
  (only-in pict/face face)
  (only-in math/statistics mean)
  (only-in racket/random random-sample)
  file/glob
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  racket/runtime-path
  gtp-pict
  pict
  ppict/2
  pict-abbrevs
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  (only-in slideshow para bt)
  gtp-plot/configuration-info gtp-plot/plot gtp-plot/typed-racket-info gtp-plot/reticulated-info gtp-plot/performance-info gtp-plot/sample-info
  plot/no-gui (except-in plot/utils min* max*))

[*OVERHEAD-DECORATION?* #f]

(define slide-top 4/100)
(define slide-left 2/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 82/100)
(define slide-text-left (* 3 slide-left)) ;; 3/2 SD 4:3
(define head-left 20/100) ;; slide-left SD 4:3
(define head-right (- 1 head-left)) ;; slide-right SD 4:3
(define text-left slide-text-left)
(define slide-text-right (- 1 slide-text-left))
(define text-right slide-text-right)
(define slide-heading-top (* 1.4 slide-top))
(define slide-text-top (* 4 slide-top))
(define hi-text (* 6 slide-top))
(define lo-text (* 2.5 hi-text))
(define slide-text-bottom slide-bottom)

(define slide-text-coord (coord slide-text-left slide-text-top 'lt))
(define slide-text-coord-left slide-text-coord)
(define slide-text-coord-mid (coord 1/2 slide-text-top 'ct))
(define slide-text-coord-right (coord slide-text-right slide-text-top 'rt))
(define slide-text-coord-l  slide-text-coord-left)
(define slide-text-coord-m   slide-text-coord-mid)
(define slide-text-coord-r slide-text-coord-right)
(define heading-text-coord (coord head-left slide-heading-top 'lt))
(define heading-text-coord-left heading-text-coord)
(define heading-text-coord-mid (coord 1/2 slide-heading-top 'ct))
(define heading-text-coord-right (coord head-right slide-heading-top 'rt))
(define heading-coord heading-text-coord)
(define heading-coord-left heading-text-coord-left)
(define heading-coord-mid heading-text-coord-mid)
(define heading-coord-right heading-text-coord-right)
(define heading-coord-l  heading-coord-left)
(define heading-coord-m  heading-coord-mid)
(define heading-coord-r  heading-coord-right)
(define bottom-coord-left (coord slide-left slide-text-bottom 'lb))
(define bottom-coord-mid (coord 1/2 slide-text-bottom 'cb))
(define bottom-coord-right (coord slide-right slide-text-bottom 'rb))
(define bottom-coord-l bottom-coord-left)
(define bottom-coord-m bottom-coord-mid)
(define bottom-coord-r bottom-coord-right)
(define center-coord (coord 1/2 1/2 'cc))
(define title-coord (coord 1/2 26/100 'ct))
(define hi-text-coord-left (coord slide-text-left hi-text 'lt))
(define hi-text-coord-mid (coord 1/2 hi-text 'ct))
(define hi-text-coord-right (coord slide-text-right hi-text 'rt))
(define hi-text-coord-l  hi-text-coord-left)
(define hi-text-coord-m   hi-text-coord-mid)
(define hi-text-coord-r hi-text-coord-right)
(define lo-text-coord-left (coord slide-text-left lo-text 'lt))
(define lo-text-coord-mid (coord 1/2 lo-text 'ct))
(define lo-text-coord-right (coord slide-text-right lo-text 'rt))
(define title-coord-m (coord 1/2 26/100 'ct))
(define all-lang-coord (coord 99/100 1/2 'rc))

(define img "img")
(define src img)

(define default-line-width 4)
(define default-arrow-size 14)
(define large-arrow-size 18)

(define turn revolution)

(define x%->pixels w%->pixels)
(define y%->pixels h%->pixels)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define border-x-sep (w%->pixels 4/100))
(define small-x-sep (w%->pixels 5/100))
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))

(define code-brush-alpha 0.6)

(define (color%++ c n)
  (make-object color%
               (byte-round (+ (send c red) n))
               (byte-round (+ (send c green) n))
               (byte-round (+ (send c blue) n))
               (send c alpha)))

(define (byte-round n)
  (if (< n 0)
    0
    (if (< 255 n)
      255 n)))


(define black (hex-triplet->color% #x222222))
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define lite-grey (hex-triplet->color% #xeeeeee)) ; "gainsboro"
(define transparent (color%-update-alpha white 0))
(define dark-orange (hex-triplet->color% #xE05626))
(define lite-orange (hex-triplet->color% #xF89C3F))
(define dark-blue (hex-triplet->color% #x002E6D))
(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))
(define lite-blue (hex-triplet->color% #xC0EFFF))
(define lite-green (hex-triplet->color% #x00b18f))

(define utah-red (hex-triplet->color% #xCC0000))
(define utah-black (hex-triplet->color% #x000000))
(define utah-white (hex-triplet->color% #xFFFFFF))
(define utah-sunrise (hex-triplet->color% #xFFB81D))
(define utah-lake (hex-triplet->color% #x3ABFC0))
(define utah-crimson (hex-triplet->color% #x890000))
(define utah-granite (hex-triplet->color% #x708E99))
(define utah-darkgrey (hex-triplet->color% #xE2E6E6))
(define utah-litegrey (hex-triplet->color% #xF7F9FB))

(define typed-color utah-sunrise)
(define untyped-color utah-granite)
(define shallow-color utah-lake)
(define deep-color typed-color)
(define typed-brush-color (color%++ typed-color 20))
(define shallow-pen-color shallow-color #;(hex-triplet->color% #xffc20a) )
(define deep-pen-color deep-color #;(hex-triplet->color% #x0c7bdc))
(define untyped-pen-color untyped-color)
(define shallow-brush-color (color%-update-alpha shallow-pen-color 0.4) #;lite-orange #;(hex-triplet->color% #xfdc008))
(define deep-brush-color (color%-update-alpha deep-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define untyped-brush-color (color%-update-alpha untyped-pen-color 0.4) #;(color%++ untyped-color 20))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define neutral-brush-color fog-3k1)
(define green0-3k1 (hex-triplet->color% #x71BE8D))
(define green1-3k1 (hex-triplet->color% #x598F61))
(define green2-3k1 (hex-triplet->color% #x4F7459))
(define red0-3k1 (hex-triplet->color% #xF0749C))
(define red1-3k1 (hex-triplet->color% #xC3476F))
(define apple-green lite-green)
(define apple-red red1-3k1)
(define typed-pen-color #f)
(define validate-pen-color red1-3k1)
(define validate-brush-color (color%-update-alpha validate-pen-color code-brush-alpha))
(define happy-cloud-color lite-blue)
(define sad-cloud-color dark-blue)
(define default-line-color dark-blue)
(define browncs-frame-color dark-blue)
(define hilite-frame-color dark-orange)
(define blame-color typed-color)
(define shallow-bg-color (color%-update-alpha shallow-pen-color 0.2))
(define deep-bg-color  (color%-update-alpha deep-pen-color 0.2))
(define typed-bg-color deep-bg-color)
(define untyped-bg-color (color%-update-alpha untyped-pen-color 0.2))

(define (color-off c)
  (color%-update-alpha c 0.2))

(define title-font "Montserrat" #;"Bree Serif")
(define body-font "Source Sans Pro" #;"Open Sans")
(define code-font "Inconsolata")

(define title-size 52)
(define subtitle-size 34)
(define head-size 38)
(define body-size 30)
(define code-size 28)
(define tcode-size (- code-size 4))

(define ((make-string->text #:font font #:size size #:color color) . str*)
  (colorize (text (apply string-append str*) font size) color))

(define (bold-style font)
  (cons 'bold font))

(define (italic-style font)
  (cons 'italic font))

(define body-font-lo (make-object font% body-size body-font 'default 'normal 'light))
(define body-font-it (make-object font% body-size body-font 'default 'italic 'light))
(define body-font-itbf (make-object font% body-size body-font 'default 'italic 'semibold))
(define body-font-md (make-object font% body-size body-font 'default 'normal 'medium))
(define body-font-hi (make-object font% body-size body-font 'default 'normal 'semibold))
(define utah-web-headline-font (make-object font% title-size title-font 'default 'normal 'semibold))
(define page-font (make-font #:face code-font #:size tcode-size))

(define titlerm (make-string->text #:font utah-web-headline-font #:size title-size #:color black))
(define titlerm2 (make-string->text #:font utah-web-headline-font #:size (- title-size 8) #:color black))
(define subtitlerm (make-string->text #:font title-font #;body-font-md #:size subtitle-size #:color black))
(define subtitlermlo
  (let ((ff (make-string->text #:font title-font #:size subtitle-size #:color black)))
    (lambda str*
      (cellophane (apply ff str*) 0.7))))
(define headrm (make-string->text #:font title-font #:size head-size #:color dark-blue))
(define coderm (make-string->text #:font code-font #:size code-size #:color black))
(define codebf (make-string->text #:font (bold-style code-font) #:size code-size #:color black))
(define codeemrm (make-string->text #:font (bold-style code-font) #:size code-size #:color green2-3k1))
(define codeemrm2 (make-string->text #:font (bold-style code-font) #:size code-size #:color dark-orange))
(define codeembf (make-string->text #:font (bold-style code-font) #:size code-size #:color apple-red))
(define tcoderm (make-string->text #:font code-font #:size tcode-size #:color black))
(define tcodebf (make-string->text #:font (bold-style code-font) #:size tcode-size #:color black))
(define tt coderm)

(define bodyrm (make-string->text #:font body-font-md #:size body-size #:color black))
(define bodyrmlo (make-string->text #:font body-font-lo #:size body-size #:color black))
(define bodyrmlobb (make-string->text #:font body-font-lo #:size body-size #:color deep-pen-color))
(define bodyrmloyy (make-string->text #:font body-font-lo #:size body-size #:color shallow-pen-color))
(define bodyrmhi (make-string->text #:font body-font-hi #:size body-size #:color black))
(define bodyrmhibb (make-string->text #:font body-font-hi #:size body-size #:color deep-pen-color))
(define bodyrmhiyy (make-string->text #:font body-font-hi #:size body-size #:color shallow-pen-color))
(define bodyit (make-string->text #:font body-font-it #:size body-size #:color black))
(define bodyitbf (make-string->text #:font body-font-itbf #:size body-size #:color black))
(define bodybf (make-string->text #:font (bold-style body-font) #:size body-size #:color black))
(define bodyemit (make-string->text #:font body-font-it #:size body-size #:color dark-orange))
(define bodyemrm (make-string->text #:font body-font-md #:size body-size #:color dark-orange))
(define bodyrmem bodyemrm)
(define bodyembf (make-string->text #:font (bold-style body-font) #:size body-size #:color dark-orange))
(define bodyemrm2 (make-string->text #:font body-font-md #:size body-size #:color green2-3k1))
(define bodyembf2 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color green2-3k1))
(define bodyembf3 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color apple-red))
(define bodyemty (make-string->text #:font body-font-md #:size body-size #:color deep-pen-color))
(define bodyemun (make-string->text #:font body-font-md #:size body-size #:color untyped-color))
(define bodyembl (make-string->text #:font body-font-md #:size body-size #:color blame-color))

(define bname bodyembf)

(define stransient "transient")
(define MAX-OVERHEAD 20)
(define gtp-version "6.0")
(define olde-rkt-version "6.2")
(define transient-rkt-version "7.8.0.5")
(define SAMPLE-RATE 10)
(define NUM-SAMPLE-TRIALS 10)

(define-runtime-path here ".")
(define data-dir (build-path here ".." "data"))

(define (benchmark-name->data-file bm-name version)
  (define pp
    (let* ((name bm-name)
           (patt (format "~a-*rktd" name)))
      (glob-first (build-path data-dir version patt))))
  (if (file-exists? pp)
      pp
      (raise-argument-error 'benchmark-name->data-file "directory-exists?" pp)))


(define (benchmark-name->performance-info bm-name version #:full-name? [full-name? #f])
  (when (and (eq? bm-name 'zordoz)
             (string=? version "6.4"))
    (error 'die))
  (define data-dir (benchmark-name->data-file bm-name version))
  (define extra-name (and full-name? (string->symbol (format "~a-~a" bm-name version))))
  (make-typed-racket-info data-dir #:name extra-name))

(define (glob-first str)
  (match (glob str)
   [(cons r '())
    r]
   ['()
    (raise-user-error 'glob-first "No results for glob '~a'" str)]
   [r*
    (printf "WARNING: ambiguous results for glob '~a'. Returning the first.~n" str)
    (car r*)]))

(define make-converter
  (let ((magic-n (*OVERHEAD-LINE-COLOR*)))
    (lambda (a b)
      (lambda (i)
        (if (= i magic-n) a b)))))

(define pen-color-converter (make-converter deep-pen-color shallow-pen-color))
(define brush-color-converter (make-converter deep-brush-color shallow-brush-color))

(define (arrowhead-pict rad #:color [color black] #:size [size 20])
  (colorize
    (arrowhead 20 rad)
    color))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn) #:color black))

(define right-arrow-pict
  (arrowhead-pict (* 0 turn) #:color black))

(define left-arrow-pict
  (arrowhead-pict (* 1/2 turn) #:color black))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn) #:color black))

(define (bg-img path)
  (rotate (bitmap path) (* 3/4 turn)))

(define (make-titlebg w h)
  (let* ((bg (bg-img "img/browncs-lite.jpeg"))
         (bg (clip-to (scale-to-fit bg w h #:mode 'distort) w h))
         #;(fg (filled-rectangle w (* 9/10 h) #:color white #:draw-border? #f)))
    bg))

(define (make-waters w h)
  (let* ((bg (bitmap "img/waters.jpg"))
         (bg (clip-to (scale-to-fit bg w (* h 2) #:mode 'preserve) w h))
         #;(fg (filled-rectangle w (* 9/10 h) #:color white #:draw-border? #f)))
    bg))

(define (make-solid-bg w h color)
  (let* ((bg (filled-rectangle w h #:color white #:draw-border? #f))
         (fg (filled-rectangle w h #:color color #:draw-border? #f)))
    (cc-superimpose bg fg)))

(define (make-bg w h) (make-solid-bg w h utah-litegrey))

#;(define make-bg
  (let ((*cache (box #f)))
    (lambda (w h)
      (filled-rectangle w h #:color utah-litegrey #:draw-border? #f)
      #;(or (unbox *cache)
          (let* ((bg (bg-img "img/browncs-lite.jpeg"))
                 (bg (clip-to (scale-to-fit bg w h #:mode 'distort) w h))
                 (fg (filled-rectangle w (* 9/10 h) #:color white #:draw-border? #f))
                 (pp (freeze (cc-superimpose bg fg))))
            (set-box! *cache pp)
            pp)))))

(define (make-deepbg w h)
  (make-solid-bg w h deep-bg-color))

(define (make-shallowbg w h)
  (make-solid-bg w h shallow-bg-color))

(define browncs-x-margin (make-parameter small-x-sep))
(define browncs-y-margin (make-parameter tiny-y-sep))

(define bbox-frame-width 2)
(define bbox-frame-color browncs-frame-color)

(define (bbox pp #:color [color white] #:x-margin [x-margin #f] #:y-margin [y-margin #f] #:frame-color [frame-color #f] #:backup? [backup? #f])
  (define xm (or x-margin (browncs-x-margin)))
  (define ym (or y-margin (browncs-y-margin)))
  (define rr 1)
  (add-rounded-border
    (if backup?
      (add-rounded-border
        pp
        #:x-margin xm #:y-margin ym #:radius rr
        #:background-color color #:frame-width 0)
      pp)
    #:x-margin (if backup? 0 xm)
    #:y-margin (if backup? 0 ym)
    #:radius rr
    #:background-color (if backup? white color)
    #:frame-width bbox-frame-width
    #:frame-color (or frame-color bbox-frame-color)))

(define (browncs-frame pp)
  (bbox pp #:x-margin 0 #:y-margin 0))

(struct code-arrow (src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style) #:transparent)

(define (add-code-arrow pp arrow
                        #:both [both-arrow #f]
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color #f]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width default-line-width))
  (define arrow-size (or pre-arrow-size default-arrow-size))
  ((if both-arrow pin-arrows-line pin-arrow-line)
    arrow-size pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color (or color default-line-color)))

(define (add-code-line pp arrow
                       #:line-width [pre-line-width #f]
                       #:color [color default-line-color]
                       #:label [label (blank)]
                       #:x-adjust-label [x-label 0]
                       #:y-adjust-label [y-label 0]
                       #:hide? [hide? #false])
  (add-code-arrow pp arrow #:arrow-size 0
                  #:line-width pre-line-width #:color color #:label label
                  #:x-adjust-label x-label #:y-adjust-label y-label #:hide? hide?))

(define (add-code-arrows pp #:arrow-size [arrow-size #f] #:color [color #f] . arrow*)
  (add-code-arrows* pp arrow* #:arrow-size arrow-size #:color color))

(define (add-code-arrows* pp* arrow* #:color [color #f] #:arrow-size [arrow-size #f])
  (for/fold ((pp pp*))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow #:color color #:arrow-size arrow-size)))

(define add-code-arrow* add-code-arrows*)

(define (add-code-lines pp #:color [color #f] . arrow*)
  (add-code-line* pp arrow* #:color color))

(define (add-code-line* pp arrow* #:color [color #f])
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-line pp arrow #:color color)))

(define (ben-rule w h #:color [color #f])
  (filled-rectangle w h #:color (or color browncs-frame-color) #:draw-border? #f))

(define (vrule h #:thickness [thickness #f] #:color [color #f])
  (ben-rule (or thickness 1) h #:color color))

(define (hrule w #:thickness [thickness #f] #:color [color #f])
  (ben-rule w (or thickness 1) #:color color))

(define (scale-to-pict pp bg)
  (scale-to-fit pp (pict-width bg) (pict-height bg)))

(define (scale-to-huge pp)
  (scale-to-fit pp
    (w%->pixels 9/10)
    (h%->pixels 85/100)))

(define (scale-to-superscript pp)
  (scale-to-fit pp
    44
    44))

(define (scale-to-tiny pp)
  (scale-to-fit pp 50 50))

(define (scale-to-width pp w)
  (scale-to-fit pp w (pict-height pp)))

(define (scale-to-width% pp w%)
  (scale-to-width pp (w%->pixels w%)))

(define (scale-to-height% pp h%)
  (scale-to-height pp (h%->pixels h%)))

(define (scale-to-height pp h)
  (scale-to-fit pp (pict-width pp) h))

(define (scale-to-square pp dim)
  (scale-to-fit pp dim dim))

(define (person-scale pp [w% #f])
  (scale-to-width% pp (or w% 15/100)))

(define (person-frame pp)
  (add-rounded-border
    pp
    #:radius 2
    #:frame-width 1
    #:frame-color black))

(define (typed-person-frame pp)
  (X-person-frame pp typed-color))

(define (untyped-person-frame pp)
  (X-person-frame pp untyped-color))

(define (X-person-frame pp cc)
    (add-rounded-border
      (person-frame pp)
      #:x-margin tiny-y-sep
      #:y-margin tiny-y-sep
      #:radius 2
      #:frame-width 0
      #:background-color cc))

(define (add-lang str)
  (string-append "lang/" str))

(define (scale-small-face pp)
  (scale-to-square pp 80))

(define (add-face str)
  (string-append "face/" str))

(define (add-src str)
  (string-append "img/" str))

(define add-img add-src)

(define (frame-person _f str w)
  (person-frame
    (person-scale (bitmap (add-src (add-face str))) w)))

(define (person-pict str)
  (person-frame (person-scale (bitmap str))))

(define (typed-person-pict str)
  (typed-person-frame (person-scale (bitmap str) 10/100)))

(define (untyped-person-pict str)
  (untyped-person-frame (person-scale (bitmap str) 10/100)))

(define word-sep 0)

(define (word-append . pp*)
  (apply hb-append word-sep pp*))

(define line-sep2 (+ 2))

(define (left-line-append2 . pp*)
  (left-line-append2* pp*))

(define (left-line-append2* pp*)
  (apply vl-append line-sep2 pp*))

(define (mid-line-append2 . pp*)
  (mid-line-append2* pp*))

(define (mid-line-append2* pp*)
  (apply vc-append line-sep2 pp*))

(define (right-line-append2 . pp*)
  (right-line-append2* pp*))

(define (right-line-append2* pp*)
  (apply vr-append line-sep2 pp*))

(define ll-append left-line-append2)
(define lc-append mid-line-append2)
(define lr-append right-line-append2)

(define line-sep tiny-y-sep)

(define (left-line-append #:sep [sep #f] . pp*)
  (left-line-append* #:sep sep pp*))

(define l-line-append left-line-append)

(define (left-line-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep line-sep) pp*))

(define (mid-line-append #:sep [sep #f] . pp*)
  (apply vc-append (or sep line-sep) pp*))

(define m-line-append mid-line-append)

(define (right-line-append . pp*)
  (apply vr-append line-sep pp*))

(define r-line-append right-line-append)

(define code-line-sep (h%->pixels 12/1000))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (codeblock-append #:sep [sep #f] . pp*)
  (codeblock-append* pp*))

(define (codeblock-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep tiny-y-sep) pp*))

(define (hcodeblock-append #:sep [sep #f] . pp*)
  (hcodeblock-append* #:sep sep pp*))

(define (hcodeblock-append* #:sep [sep #f] pp*)
  (apply ht-append (or sep tiny-x-sep) pp*))

(define boundary-append* hcodeblock-append*)

(define (scale-lang-lo pp)
  (scale-to-fit pp 120 80))

(define (scale-lang-hi pp)
  (scale-to-fit pp 120 120))

(define (lang-lo str)
  (scale-lang-lo (bitmap str)))

(define (symbol->lang-pict sym #:ext [ext #f])
  (lang-lo (add-img (add-lang (format "~a.~a" sym (or ext 'png))))))

(define (lang-hi str)
  (scale-lang-hi (bitmap str)))

(define (split/2 lang-img*)
  (split-at lang-img* (quotient (length lang-img*) 2)))

(define (split/n lang-img* n)
  (let loop ((pp* lang-img*))
    (if (< (length pp*) n)
      (list pp*)
      (let-values (((a b) (split-at pp* n)))
        (cons a (loop b))))))

(define (X-codeblock pp* #:dark? [dark? #f] #:title [title #f] #:label [label #f] #:frame-color [frame-color #f] #:background-color [background-color #f])
  (define title-pict (if (pict? title) title (if (string? title) (bodyrmlo title) #f)))
  (define label-margin (if title-pict (* 10/100 (pict-height title-pict)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (define radius 1)
  (define fw 5)
  (let* ((block-pict
           (bbox
             (code-line-append* pp*)
             #:backup? #t
             #:frame-color #f #;(if dark? #f background-color)
             #:color (if dark?
                       background-color
                       (color%-update-alpha background-color 0.4))
                       )))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vc-append 0 (ht-append 4 (blank) title-pict) (add-label-margin block-pict)) block-pict))))

(define (conslang x y)
  (if x (list* (tt x) (blank) y) y))

(define (ds-codeblock* x*)
  (LR-stack (deep-codeblock* x*) (shallow-codeblock* x*)))

(define (LR-stack lhs rhs)
  (define offset (* 1.4 pico-y-sep))
  (ppict-do
    rhs
    #:go (coord 1/2 1/2 #:abs-x (- offset) #:abs-y offset)
    lhs))

(define (untyped-code str)
  (untyped-codeblock #:title #f #:lang #f str))

(define (untyped-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang untyped"] . str*)
  (untyped-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (untyped-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (shallow-code str)
  (shallow-codeblock #:title #f #:lang #f str))

(define (shallow-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang shallow"] . str*)
  (shallow-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (shallow-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-code str)
  (deep-codeblock #:title #f #:lang #f str))

(define (deep-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang deep"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (deep-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define typed-codeblock* deep-codeblock*)

(define (untyped-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color untyped-brush-color pp))

(define (typed-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color deep-brush-color pp))

(define (typed-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang typed"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (dyn-codeblock sym)
  (case sym
    ((U untyped) untyped-codeblock)
    ((D deep) deep-codeblock)
    ((S shallow) shallow-codeblock)
    ((T typed) typed-codeblock)
    ((#f) (lambda arg* (blank)))
    (else (raise-argument-error 'dyn-codeblock "(or/c D S U)" sym))))

(define (deep-name)
  (parameterize ((browncs-x-margin pico-x-sep))
    (deep-codeblock "Deep")))

(define (shallow-name)
  (parameterize ((browncs-x-margin pico-x-sep))
    (shallow-codeblock "Shallow")))

(define (untyped-name)
  (parameterize ((browncs-x-margin pico-x-sep))
    (untyped-codeblock "Untyped")))

(define (xblank n)
  (blank n 0))

(define (yblank n)
  (blank 0 n))

(define xsep xblank)
(define ysep yblank)

(define natural-str "Guarded")
(define conatural-str "Co-Guarded")
(define forgetful-str "Forgetful")
(define transient-str "Transient")
(define amnesic-str "Amnesic")
(define erasure-str "Optional")

(define (check-pict h)
  (bitmap (check-icon #:color apple-green #:height h #:material rubber-icon-material)))

(define (caution-pict h)
  (bitmap (close-icon #:color utah-sunrise #:height h #:material plastic-icon-material)))

(define (stop-pict h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material plastic-icon-material)))

(define (record-pict h)
  (bitmap (record-icon #:color green1-3k1 #:height h #:material plastic-icon-material)))

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

(define (add-neutral-background pp)
  (add-rectangle-background
    #:x-margin 0 #:y-margin 0
    #:color neutral-brush-color
    #:radius 1
    (add-rectangle-background
      #:x-margin small-x-sep #:y-margin tiny-y-sep
      #:color white
      pp)))

(define (table2 #:col-sep [pre-col-sep #f]
                #:row-sep [pre-row-sep #f]
                #:col-align [col-align lc-superimpose]
                #:row-align [row-align cc-superimpose]
                . kv*)
  (table2* kv*
                     #:col-sep pre-col-sep
                     #:row-sep pre-row-sep
                     #:col-align col-align
                     #:row-align row-align))

(define (table2* kv**
                     #:col-sep [pre-col-sep #f]
                     #:row-sep [pre-row-sep #f]
                     #:col-align [col-align lc-superimpose]
                     #:row-align [row-align cc-superimpose])
  (define col-sep (or pre-col-sep 328/5))
  (define row-sep (or pre-row-sep 364/5))
  (table 2 (flatten kv**) col-align row-align col-sep row-sep))

(define (xindent pp #:sep [x #f]) (ht-append (xblank (or x small-x-sep)) pp))

(define swatch-blank (blank pico-x-sep pico-y-sep))

(define red-swatch
  (X-codeblock #:background-color apple-red #:dark? #t #:title #f (list swatch-blank)))

(define untyped-swatch
  (untyped-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define shallow-swatch
  (shallow-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define deep-swatch
  (deep-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define big-swatch-blank (blank (w%->pixels 6/100) small-y-sep))

(define (untyped-icon #:lbl [lbl "U"])
  (center-label
    (untyped-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (typed-icon #:lbl [lbl "T"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (deep-icon #:lbl [lbl "D"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (shallow-icon #:lbl [lbl "S"])
  (center-label
    (shallow-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (untyped-icon2 pp)
  (untyped-codeblock* #:title #f (list (cc-superimpose big-swatch-blank pp))))

(define (typed-icon2 pp)
  (typed-codeblock* #:title #f (list (cc-superimpose big-swatch-blank pp))))

(define (center-label pp lbl)
  (ppict-do
    pp
    #:go (coord 1/2 46/100 'cc)
    (if lbl (scale (headrm lbl) 0.9) (blank))))

(define (dyn-swatch sym)
  (case sym
    ((D T) deep-swatch)
    ((U) untyped-swatch)
    ((S) shallow-swatch)
    ((B) (bghost deep-swatch))
    ((K) red-swatch)
    (else (raise-argument-error 'dyn-swatch "(or/c 'D 'S 'U)" sym))))

(define (boundary-node sym* #:arrow [arrow #f])
  (let* ((pp*
          (for/list ((sym (in-list sym*))
                     (i (in-naturals)))
            (add-hubs
              (dyn-swatch sym)
              (string->symbol (format "N~a" i)))))
         (pp (hcodeblock-append* #:sep (if arrow tiny-x-sep 4) pp*))
         (gg (bghost pp)))
    (if arrow
      (add-code-arrows
        ;; TODO options? gotta standardize everywhere
        (vc-append gg pp gg)
        (code-arrow 'N0-E rc-find 'N1-W lc-find 0 0 0 0 'solid)
        (code-arrow 'N2-W lc-find 'N1-E rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
        (code-arrow 'N3-E rc-find 'N4-W lc-find 0 0 0 0 'solid)
        (code-arrow 'N0-N rt-find 'N3-N lt-find (* 08/100 turn) (* 92/100 turn) 1/4 1/4 'solid)
        (code-arrow 'N2-S rb-find 'N4-S lb-find (* 90/100 turn) (* 10/100 turn) 1/4 1/4 'solid))
      pp)))

(define the-boundary-pict
  (boundary-node '(U D U D D) #:arrow #t))

(define the-mixed-pict
  (boundary-node '(U D U D D)))

(define the-typed-pict
  (boundary-node '(D D D D D)))

(define the-untyped-pict
  (boundary-node '(U U U U U)))

(define lattice-x-sep small-x-sep)
(define lattice-y-sep tiny-y-sep)

(define (path-node sym*)
  (apply hc-append pico-x-sep (map dyn-swatch sym*)))

(define (eq?* x*)
  (let loop ((xx x*))
    (if (or (null? xx) (null? (cdr xx)))
      #t
      (and (eq? (car xx) (cadr xx))
           (loop (cdr xx))))))

(define (bits->path-node b*)
  (path-node
    (for/list ((b (in-list b*)))
      (if b 'T 'U))))

(define (bits->ds b*)
  (path-node
    (for/list ((b (in-list b*)))
      (if b 'S 'D))))

(define (what-to-measure-lattice n #:x [x #f])
  (define x-sep
    (if (< n 5) lattice-x-sep pico-x-sep))
  (define lattice-pict
    (make-lattice n bits->path-node #:x-margin x-sep #:y-margin lattice-y-sep))
  lattice-pict)

(define (ds-lattice n)
  (define x-sep
    (if (< n 5) lattice-x-sep pico-x-sep))
  (define lattice-pict
    (make-lattice n bits->ds #:x-margin x-sep #:y-margin lattice-y-sep))
  lattice-pict)

(define (bits->dspath b*)
  (define num-untyped (for/sum ((b (in-list b*))) (if b 0 1)))
  (define t-sym (if (< num-untyped 2) 'D 'S))
  (path-node
    (for/list ((b (in-list b*)))
      (if b t-sym 'U))))

(define (pathology-lattice n #:x [x #f])
  (define x-sep lattice-x-sep)
  (make-lattice n bits->dspath #:x-margin x-sep #:y-margin lattice-y-sep))

(define (scale-small-lattice pp)
  (scale-to-fit pp (w%->pixels 55/100) (h%->pixels 45/100)))

(define (migration-append #:arr [arr #f] . pp*)
  (migration-append* #:arr arr pp*))

(define (migration-append* #:arr [arr #f] pp*)
  (apply vc-append lattice-y-sep (add-between pp* (or arr up-arrow-pict))))

(define (dmigration-append #:arr [arr #f] . pp*)
  (dmigration-append* #:arr arr pp*))

(define (dmigration-append* #:arr [arr #f] pp*)
  (apply vc-append lattice-y-sep (add-between pp* (or arr down-arrow-pict))))

(define (email-panels pth*)
  (define w 280)
  (define h 240)
  (for/fold ((acc (blank)))
            ((pth (in-list pth*))
             (up? (in-cycle (in-list '(#t #f)))))
    (define img
      (if (pict? pth)
        (ppict-do (blank w h) #:go (coord 1/2 0/100 'ct) pth)
        (scale-to-fit (bitmap pth) w h)))
    (define yshim (yblank (* 1/2 (pict-height img))))
    (hc-append
      (- med-x-sep)
      (if up? (vc-append img yshim) (vc-append yshim img))
      acc)))

(define (tu-icon)
  (hc-append tiny-x-sep (typed-icon #:lbl #f) (untyped-icon #:lbl #f)))

(define (du-icon)
  (hc-append tiny-x-sep (deep-icon #:lbl #f) (untyped-icon #:lbl #f)))

(define (su-icon)
  (hc-append tiny-x-sep (shallow-icon #:lbl #f) (untyped-icon #:lbl #f)))

(define (dsu-icon)
  (hc-append
    tiny-x-sep
    (vc-append
      tiny-y-sep
      (deep-icon)
      (shallow-icon))
    (untyped-icon)))

(define (on-stick pp)
  (ppict-do
    pp
    #:go (coord 1/2 93/100 'ct)
    (vrule (h%->pixels 2/100) #:thickness 3)))

(define (dsu-icon2)
  (hc-append
    (w%->pixels 5/100)
    (vr-append
      tiny-y-sep
      (deep-codeblock* (list @coderm{Deep}))
      (shallow-codeblock* (list @coderm{Shallow})))
    (untyped-codeblock* (list @coderm{Untyped}))))

(define (mixed-best-pict n)
    (hc-append
      small-x-sep
      ((if (< n 1) bghost values) (mixed-best-table 2))
      (scale (example-lattice-3way #:top #t) 75/100)))

(define (rq-text pp)
  (word-append
    @bodyrmhi{RQ.  } pp))

(define (question-box pp)
  (bbox #:x-margin med-x-sep #:y-margin small-y-sep pp))

(define answer-box bbox)

(define (scale-lang-sidebar pp)
  (scale pp 80/100))

(define (scale-lang-sidebar2 pp)
  (scale pp 60/100))

(define (maybe-lbl do-lbl? pp name)
  (if do-lbl?
    (cc-superimpose pp (bbox (bodyembf name)))
    pp))

(define (interleave x* y*)
  (cond
    [(null? x*) y*]
    [(null? y*) x*]
    [else (list* (car x*) (car y*) (interleave (cdr x*) (cdr y*)))]))

(define (tr-pict)
  (racket-pict))

(define (racket-pict)
  (symbol->lang-pict 'racket))

(define (ts-pict)
  (symbol->lang-pict 'typescript))

(define (flow-pict)
  (symbol->lang-pict 'flow))

(define (mypy-pict)
  (ppict-do
    (symbol->lang-pict 'python)
    #:go (coord 1/2 1 #:abs-y (- 4))
    @coderm{mypy}))

(define (typed-clojure-pict)
  (ppict-do
    (symbol->lang-pict 'typed-clojure)
    #:go (coord 65/100 1/2 'cc)
    (symbol->lang-pict 'clojure)))

(define (pyre-pict)
  (symbol->lang-pict 'pyre))

(define (text->lang-pict str)
  (scale (headrm str) 80/100))

(define (retic-pict)
  (ppict-do
    (python-pict)
    #:go (coord 1 1 'rb #:abs-x (- 4) #:abs-y (- 4))
    (scale (bbox (indiana-pict) #:x-margin 4 #:y-margin 4) 45/100)))

(define (actionscript-pict)
  (txt-lang "AS"))

(define (cl-pict)
  (symbol->lang-pict 'cl))

(define (hack-pict)
  (symbol->lang-pict 'hack))

(define (pytype-pict)
  (ppict-do
    (symbol->lang-pict 'python)
    #:go (coord 1/2 1 #:abs-y (- 4))
    @coderm{PyType}))

(define (pyright-pict)
  (freeze (symbol->lang-pict 'Pyright)))

(define (rdl-pict)
  (symbol->lang-pict 'ruby)
  #;(ppict-do
    (symbol->lang-pict 'ruby)
    #:go center-coord
    @coderm{RDL}))

(define (strongtalk-pict)
  (symbol->lang-pict 'strongtalk))

(define (typescript-pict)
  (symbol->lang-pict 'typescript))

(define (typed-lua-pict)
  (symbol->lang-pict 'lua)
  #;(ppict-do
    (symbol->lang-pict 'lua)
    #:go center-coord
    @coderm{T. Lua}))

(define (gradualtalk-pict)
  (symbol->lang-pict 'smalltalk)
  #;(ppict-do
    (symbol->lang-pict 'smalltalk)
    #:go center-coord
    @coderm{Gradualtalk}))

(define (grift-pict)
  (txt-lang "Grift"))

(define (tpd-pict)
  (txt-lang "TPD"))

(define (pyret-pict)
  (symbol->lang-pict 'pyret))

(define (grace-pict)
  (symbol->lang-pict 'grace))

(define (pallene-pict)
  (vc-append
    4
    @coderm{Pallene}
    (scale (symbol->lang-pict 'lua) 9/10)))

(define (sp-pict)
  (ppict-do
    (symbol->lang-pict 'python)
    #:go (coord 1/2 1 #:abs-y (- 4))
    @coderm{StaticP}))

(define (csharp-pict)
  (txt-lang "C#"))

(define (dart2-pict)
  (symbol->lang-pict 'dart))

(define (nom-pict)
  (txt-lang "Nom"))

(define (js-pict)
  (symbol->lang-pict 'javascript))

(define (safets-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{SafeTS}))

(define (tsstar-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{TS*}))

(define (sorbet-pict)
  (symbol->lang-pict 'sorbet))

(define (strongscript-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{StrS.}))

(define (thorn-pict)
  (symbol->lang-pict 'thorn))

(define (tmp-lang)
  (scale-lang-lo (filled-rectangle 200 200 #:color gray #:draw-border? #f)))

(define (txt-lang str)
  (ppict-do
    (filled-rectangle 80 80 #:color gray #:draw-border? #f)
    #:go center-coord
    (coderm str)))

(define (indiana-pict)
  (symbol->lang-pict 'indiana))

(define (all-lang-pict*)
             (list (actionscript-pict) (cl-pict) (mypy-pict)
                   (flow-pict) (hack-pict) (pyre-pict)
                   (pytype-pict) (pyright-pict) (rdl-pict)
                   (strongtalk-pict) (typescript-pict) (typed-clojure-pict)
                   (typed-lua-pict) (gradualtalk-pict) (grift-pict)
                   (tpd-pict) (tr-pict) (pyret-pict) (grace-pict)
                   (pallene-pict) (retic-pict) (sp-pict) (csharp-pict)
                   (dart2-pict) (nom-pict) (safets-pict) (tsstar-pict)
                   (sorbet-pict) (strongscript-pict) (thorn-pict)))

(define (lang-grid lang* #:num [num #f])
      (let* (
             (row-width (or num 8))
             (lang** (split/n lang* row-width))
             (pp* (map (lambda (l*) (apply hc-append tiny-x-sep l*)) lang**))
             (pp (apply vc-append small-y-sep pp*)))
        pp))

(define (four-camps-pict north-tag east-tag south-tag west-tag)
  (define c*
    (list (sp-pict) (csharp-pict) (dart2-pict) (nom-pict) (safets-pict)
          (tsstar-pict) (sorbet-pict) (strongscript-pict) (thorn-pict)))
  (define e*
    (list (actionscript-pict) (cl-pict) (mypy-pict) (pyre-pict) (flow-pict)
          (hack-pict) (pytype-pict) (pyright-pict) (rdl-pict) (strongtalk-pict)
          (typescript-pict) (typed-clojure-pict) (typed-lua-pict)))
  (define n*
    (list (gradualtalk-pict) (grift-pict) (tpd-pict) (tr-pict)))
  (define t*
    (list (pyret-pict) (grace-pict) (pallene-pict) (retic-pict)))
  (define-values [t+ n+]
    (apply values (pict-bbox-sup (lang-grid t* #:num 4) (lang-grid n* #:num 4))))
  (ppict-do
    (bghost (lang-grid (all-lang-pict*)))
    #:go (coord 1/2 0 'cc)
    (tag-pict (values (lang-grid c* #:num 5)) north-tag)
    #:go (coord 1 1/2 'cc)
    (tag-pict (values t+) east-tag)
    #:go (coord 1/2 1 'cc)
    (tag-pict (values (lang-grid e*)) south-tag)
    #:go (coord 0 1/2 'cc)
    (tag-pict (values n+) west-tag)
    ))

(define (not-today why-str)
  (define no-pict (stop-pict 35))
  (bbox
    (ht-append tiny-x-sep no-pict (bodyrmlo why-str) (bghost no-pict))))

(define (yes-today why-str lbl-pict)
  (define y-pict (record-pict 38))
  (bbox
    (ht-append
      tiny-x-sep
      y-pict
      (lc-append
        (bodyrmlo why-str)
        (yblank 0)
        lbl-pict)
      (bghost y-pict))))

(define (shallow-today-pict) (yes-today "fast, wrong types" (shallow-name)))
(define (deep-today-pict) (yes-today "strong, slow types" (deep-name)))

(define (shallow-today-pict2)
  (vc-append
    pico-y-sep
    (shallow-today-pict)
    (su-icon)))

(define (deep-today-pict2)
  (vc-append
    pico-y-sep
    (deep-today-pict)
    (du-icon)))

(define (camp-lbl str)
  (bbox (bodyrmhi str)))

(define (more-lang-pict)
  (cc-superimpose
    (scale-lang-lo (filled-rectangle 200 200 #:color white #:draw-border? #f))
    @headrm{...}))

(define (vss-pict)
  ;; TODO show Vitousek and coauthors??
  (blank))

(define (python-pict)
  (symbol->lang-pict 'python))

(define (example-lattice-n n)
  (what-to-measure-lattice n))

(define (example-lattice-3way #:top [top #f])
  (define n 3)
  (define x-sep
    (if (< n 5) lattice-x-sep pico-x-sep))
  (define (atop lo hi)
    (ppict-do lo #:go (coord 1/2 1/2 'cb) (bbox hi #:backup? #t #:color shallow-brush-color)))
  (define (b->p/3 b*)
    (define base (bits->path-node b*))
    (cond
      [(equal? b* '(#t #f #f))
       (atop
         base
         (scale (path-node '(S U U)) 1/2))]
      [(equal? b* '(#f #t #t))
       (atop
         base
          (scale
           (vc-append
             lattice-y-sep
             (path-node '(U S S))
             (hc-append x-sep (path-node '(U D S)) (path-node '(U S D)))
             (path-node '(U D D)))
           40/100))]
      [(and top (equal? b* '(#t #t #t)))
       (atop
         base
         (scale
           (ds-lattice 3)
           30/100))]
      [else
        base]))
  (make-lattice n b->p/3 #:x-margin x-sep #:y-margin lattice-y-sep))

(define (whence-lattice n)
    (let* ((ss (lambda (pp) (scale pp 76/100)))
           (txt (lambda (pp str) (vc-append tiny-y-sep (bbox (bodyrmlo str)) pp))))
      (vc-append
        small-y-sep
        (txt (ss (path-node (make-list 3 'T))) "Ex: One program with 3 components")
        down-arrow-pict
        ((if (< n 1) bghost values)
         (txt (ss (example-lattice-n 3)) "8 Typed / Untyped points (2^N)"))
        ((if (< n 1) bghost values)
          down-arrow-pict)
        ((if (< n 2) bghost values)
          (txt (ss (example-lattice-3way)) "27 Deep / Shallow / Untyped points (3^N)"))
        )))

(define (pict-w-sup . pp*)
  (pict-w-sup* pp*))

(define (pict-w-sup* pp*)
  (define w (apply max (map pict-width pp*)))
  (for/list ((pp (in-list pp*)))
    (ct-superimpose (xblank w) pp)))

(define (arrow-bullet pp)
  (hc-append
    tiny-x-sep
    right-arrow-pict pp))

(define (plus-bullet pp)
  (word-append @bodyrmhi{+ } pp))

(define (minus-bullet pp)
  (word-append @bodyrmhi{- } pp))

(define (venue-pict title where #:box? [box? #t])
  (lc-append
    (bodyrmhi title)
    (vv-pict where #:box? box?)))

(define (vv-pict where #:box? [box? #t])
    ((if box? bbox values) (bodyrmlo where)))

(define (benchmark-pict img #:w% [w% #f] #:url [url #f])
  (add-lite-bg
    #:x tiny-y-sep
    #:y tiny-y-sep
    (label-below
      (bbox
        #:x-margin pico-y-sep #:y-margin pico-y-sep
        (scale-to-fit (bitmap img)
                      (w%->pixels (or w% 3/10))
                      600))
      (yblank pico-y-sep)
      (coderm url))))

(define (label-below base . pp*)
  (vc-append 0 base (apply vc-append 2 pp*)))

(define (add-lite-bg pp #:x [x #f] #:y [y #f])
  (bbox pp #:color lite-grey #:x-margin x #:y-margin y))

(define (pplay #:steps [N (pplay-steps)]
               #:delay [secs 0.05]
               #:skip-first? [skip-first? #f]
               mid)
  (unless skip-first?
    (pslide #:set (mid ppict-do-state 0)))
  (if condense?
      (skip-slides N)
      (for ([n (in-list
                (let ([cnt N])
                  (let loop ([n cnt])
                    (if (zero? n)
                        null
                        (cons (/ (- cnt -1 n) 1.0 cnt)
                              (loop (sub1 n)))))))])
        (pslide #:timeout secs
                #:set (mid ppict-do-state n)))))

(define ds-benchmark* '(synth take5 quadU jpeg suffixtree dungeon #;fsmoo))

(define (arrow-bullet* . pp*)
  (apply vl-append 2 (map arrow-bullet pp*)))

(define (bad-news pp)
  (bbox
    #:x-margin pico-y-sep #:y-margin pico-y-sep
    (question-box pp)))

(define (bbbox pp)
  (bbox
    #:x-margin pico-y-sep #:y-margin pico-y-sep
    (bbox pp)))

(define pplay-steps (make-parameter 10))

(define ((slide-assembler/background base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define mixed-best-data*
  (list
    (list "forth" "12%")
    (list "fsm" "38%")
    (list "fsmoo" "31%")
    (list "mbta" "19%")
    (list "morsecode" "25%")
    (list "zombie" "6%")
    (list "dungeon" "31%")
    (list "jpeg" "38%")
    (list "zordoz" "47%")
    (list "lnm" "66%")
    (list "suffixtree" "48%")
    (list "kcfa" "55%")
    (list "snake" "46%")
    (list "take5" "36%")
    (list "acquire" "64%")
    (list "tetris" "62%")
    ))

(define (mixed-best-table n)
  (define title* (list "Benchmark" @bodyembf{Best with D+S}))
  (define (num->pict str)
    (define n (string->number
                (let ((ss (substring str 0 (sub1 (string-length str)))))
                  (if (eq? #\> (string-ref ss 0))
                    (substring ss 1)
                    ss))))
    ((if (< n 40) coderm codeemrm) str))
  (define (?bodyrmlo x)
    (if (pict? x) x (bodyrmlo x)))
  (define (title->pre-pict* rr)
    (list (bghost (bodyrmlo (first rr))) (?bodyrmlo (second rr))))
  (define (row->pre-pict* rr)
    (list (coderm (first rr)) (num->pict (second rr))))
  (define mask*
    (case n
      ((0 1)
       (lambda (pp*)
         (define-values [ll rr] (split-at pp* (- (length pp*) (- 2 n))))
         (append ll (map bghost rr))))
      (else
       (lambda (pp*)
         pp*))))
  (define ff (compose1 mask* row->pre-pict*))
  (apply
    ht-append
    (w%->pixels 7/100)
    (for/list ((row (in-list (split/n mixed-best-data* 8)))
               #:when (not (null? row)))
      (table
        2
        (flatten (map ff row))
        (cons lc-superimpose rc-superimpose)
        cc-superimpose
        tiny-x-sep
        tiny-y-sep))))

(define overhead-data*
  (list
    (list "sieve" "16x" "4.36x" "2.97x")
    (list "forth" "5800x" "5.51x" "5.43x")
    (list "fsm" "2.24x" "2.38x" "1.91x")
    (list "fsmoo" "420x" "4.28x" "4.25x")
    (list "mbta" "1.91x" "1.74x" "1.71x")
    (list "morsecode" "1.57x" "2.77x" "1.3x")
    (list "zombie" "46x" "31x" "31x")
    (list "dungeon" "15000x" "4.97x" "3.16x")
    (list "jpeg" "23x" "1.66x" "1.56x")
    (list "zordoz" "2.63x" "2.75x" "2.58x")
    (list "lnm" "1.23x" "1.21x" "1.17x")
    (list "suffixtree" "31x" "5.8x" "5.8x")
    (list "kcfa" "4.33x" "1.24x" "1.24x")
    (list "snake" "12x" "7.67x" "7.61x")
    (list "take5" "44x" "2.99x" "2.97x")
    (list "acquire" "4.22x" "1.42x" "1.42x")
    (list "tetris" "13x" "9.93x" "5.44x")
    (list "synth" "47x" "4.2x" "4.2x")
    (list "gregor" "1.72x" "1.59x" "1.51x")
    (list "quadT" "26x" "7.39x" "7.23x")
    (list "quadU" "55x" "7.57x" "7.45x")
    ))

(define (overhead-table n)
  (define title* (list "Benchmark"
                       "Deep" "Shallow" "D|S"
                       ;;@bodyembf{Deep} @bodyembf{Shallow} @bodyembf{D||S}
                       ))
  (define (s->n str)
    (string->number
              (let ((ss (substring str 0 (sub1 (string-length str)))))
                ss)))
  (define (num->pict str #:vs [vs* #f])
    (define nnn (s->n str))
    (unless (real? nnn)
      (printf "DEAD ~a~n" str))
    (cond
      [(>= nnn 30)
       (codeembf str)]
      [(and vs* (< 1 n) (andmap (lambda (ss) (< nnn (s->n ss))) vs*))
       (codeemrm str)]
      [else
        (coderm str)]))
  (define (?bodyrmlo x)
    (if (pict? x) x (bodyrmlo x)))
  (define (title->pre-pict* rr)
    (list (bghost (bodyrmlo (first rr)))
          (?bodyrmlo (fourth rr))
          (?bodyrmlo (second rr))
          (?bodyrmlo (third rr))
          ))
  (define (row->pre-pict* rr)
    (define the-name (first rr))
    (list (tag-pict (coderm the-name) (string->symbol the-name))
          (num->pict (fourth rr) #:vs (list (second rr) (third rr)))
          (num->pict (second rr))
          (num->pict (third rr))
          ))
  (define mask*
    (case n
      ((0 1)
       (lambda (pp*)
         (define-values [ll rr] (split-at pp* (- (length pp*) (- 2 n))))
         (append ll (map bghost rr))))
      (else
       (lambda (pp*)
         pp*))))
  (define ff (compose1 mask* row->pre-pict*))
  (apply
    ht-append
    (w%->pixels 7/100)
    (for/list ((row (in-list (split/n overhead-data* 11))))
      (define pp** (map ff row)
          #;(cons
            (mask* (title->pre-pict* title*))
            (map ff row)))
      (bg-stripes
        (reverse (for/list ((bg-color (in-list (if (= n 2)
                                        (list shallow-brush-color deep-brush-color)
                                        '())))
                   (i (in-naturals 1)))
          (define (ref x*)
            (list-ref x* (- (length x*) i)))
          (cons bg-color (apply max (map (compose1 pict-width ref) pp**)))))
        (table
          4
          (flatten pp**)
          (cons lc-superimpose rc-superimpose)
          cc-superimpose
          tiny-x-sep
          tiny-y-sep)))))

(define (bg-stripes c+w* pp)
  (cond
    [(null? c+w*)
     pp]
    [else
      (define h (pict-height pp))
      (rt-superimpose
        (apply
          ht-append
          tiny-x-sep
          (for/list ((c+w (in-list c+w*)))
            (filled-rounded-rectangle
              (cdr c+w) h 1/2
              #:color (car c+w)
              #:draw-border? #f)))
        pp)]))

(define path-data*
  (list
    (list "sieve" "0%" "0%" "100%")
    (list "forth" "0%" "0%" "50%")
    (list "fsm" "100%" "100%" "100%")
    (list "fsmoo" "0%" "0%" "50%")
    (list "mbta" "100%" "100%" "100%")
    (list "morsecode" "100%" "100%" "100%")
    (list "zombie" "0%" "0%" "50%")
    (list "dungeon" "0%" "0%" "67%")
    (list "jpeg" "0%" "100%" "100%")
    (list "zordoz" "100%" "100%" "100%")
    (list "lnm" "100%" "100%" "100%")
    (list "suffixtree" "0%" "0%" "12%")
    (list "kcfa" "33%" "100%" "100%")
    (list "snake" "0%" "0%" "0%")
    (list "take5" "0%" "100%" "100%")
    ))

(define region-w (w%->pixels 26/100))
(define region-h (h%->pixels 2/10))

(define (region-pict name #:color color)
  (let* ((bg (filled-rounded-rectangle
               region-w region-h
               1
               #:color color
               #:border-color black
               #:border-width 1))
         (fg (coderm name))
         (pp (cc-superimpose bg fg))
         (sym (string->symbol name)))
    (add-hubs pp sym)))

(define interaction-y-sep big-y-sep)
(define interaction-x-sep (w%->pixels 24/100))

(define (du-interaction n)
  (let* ((t-pict (region-pict "Deep Typed" #:color deep-brush-color))
         (u-pict (region-pict "Untyped" #:color untyped-brush-color))
         (pp (hc-append interaction-x-sep t-pict u-pict))
         (arr (code-arrow '|Deep Typed-E| rc-find 'Untyped-W lc-find 0 0 0 0 'solid))
         (pp (if (< n 1)
               pp
               (add-code-arrow pp arr #:arrow-size large-arrow-size #:both #true))))
    pp))

(define (type-boundary nn dir type before after)
  (let* ((left? (eq? dir 'L))
         (pp (xblank interaction-x-sep))
         (pp
           (hc-append
             (add-hubs ((if (< nn (if left? 2 1)) bghost values) (if left? after before)) 'L)
             pp
             (add-hubs ((if (< nn (if left? 1 2)) bghost values) (if left? before after)) 'R)))
         (pp
           (add-code-arrow
             pp
             (if left?
               (code-arrow '|R-W| lc-find '|L-E| rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
               (code-arrow '|L-E| rc-find '|R-W| lc-find (* 0 turn) (* 0 turn) 0 0 'solid))
             #:arrow-size large-arrow-size))
         (pp
           (vc-append (- pico-y-sep) type pp))
         )
    pp))

(define (su-interaction n)
  (let* ((t-pict
           (let* ((pp (region-pict "Shallow Typed" #:color shallow-brush-color))
                  (pp (if (< n 2) pp (add-spots pp untyped-brush-color))))
             (if (< n 4)
               pp
               (ppict-do
                 pp
                 #:go (coord 9/10 9/10 'cc)
                 (tiny-mag)
                 #:go (coord 1/10 1/10 'cc)
                 (tiny-mag)))))
         (u-pict
           (let ((pp (region-pict "Untyped" #:color untyped-brush-color)))
             (if (< n 3)
               pp
               (add-spots pp shallow-brush-color))))
         (pp (hc-append interaction-x-sep t-pict u-pict))
         (arr (code-arrow '|Shallow Typed-E| rc-find 'Untyped-W lc-find 0 0 0 0 'solid))
         (pp (if (< n 1)
               pp
               (add-code-arrow pp arr #:arrow-size large-arrow-size #:both #true)))
         )
    pp))

(define (tiny-mag)
  (bitmap (magnifying-glass-icon #:height 60)))

(define (add-spots pp color)
  (define y-spot (tiny-spot color))
  (define n-spot (bghost y-spot))
  (define num-check 15)
  (ppict-do
    pp
    #:go (coord 1/2 10/100 'ct)
    (checkerboard 2 num-check n-spot y-spot)
    #:go (coord 1/2 90/100 'cb)
    (checkerboard 2 num-check y-spot n-spot)))

(define (checkerboard num-rows num-cols black-pict red-pict)
  (define (make-row n . pp*)
    (apply
      ht-append
      4
      (for/list ((_i (in-range n))
                 (pp (in-cycle (in-list pp*))))
        pp)))
  (define y-row (make-row num-cols black-pict red-pict))
  (define n-row (make-row num-cols red-pict black-pict))
  (apply
    vl-append
    (for/list ((_i (in-range num-rows))
               (pp (in-cycle (in-list (list y-row n-row)))))
      pp)))

(define (tiny-spot color)
  (define ww 18)
  (define hh ww)
  (cc-superimpose
    (tiny-rect ww hh white)
    (tiny-rect ww hh color)))

(define (tiny-rect ww hh color)
  (filled-rounded-rectangle
    ww hh 2
    #:color color
    ;;#:draw-border? #f
    #:border-color browncs-frame-color
    #:border-width 0.2
    ))

(define (dsu-interaction n #:su-blur [su-blur #f])
  (let* ((d-pict (region-pict "Deep Typed" #:color deep-brush-color))
         (s-pict
           (let ((pp (region-pict "Shallow Typed" #:color shallow-brush-color)))
             (if su-blur (add-spots pp untyped-brush-color) pp)))
         (ds-pict (vc-append interaction-y-sep d-pict s-pict))
         (u-pict
           (let ((pp (region-pict "Untyped" #:color untyped-brush-color)))
             (if su-blur (add-spots pp shallow-brush-color) pp)))
         (u+-pict (cc-superimpose (yblank (- (pict-height ds-pict) (pict-height d-pict))) u-pict))
         (pp (hc-append interaction-x-sep ds-pict u+-pict))
         (pp (if (< n 1)
               pp
               (add-code-arrow*
                 (add-code-arrow
                   pp
                   (code-arrow '|Deep Typed-S| cb-find '|Shallow Typed-N| ct-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid)
                   #:arrow-size large-arrow-size #:both #true)
                 (list
                   (code-arrow u+-pict ct-find '|Deep Typed-E| rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
                   (code-arrow u+-pict cb-find '|Shallow Typed-E| rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
                   (code-arrow u+-pict ct-find '|Untyped-N| ct-find (* 1/2 turn) (* 3/4 turn) 0 0 'solid)
                   (code-arrow u+-pict cb-find '|Untyped-S| cb-find (* 1/2 turn) (* 1/4 turn) 0 0 'solid))
                 #:arrow-size large-arrow-size))))
    pp))

(define (tu-bubbles)
  (define-values [tt uu]
    (apply values (pict-bbox-sup @bodyrmlo{Typed} @bodyrmlo{Untyped})))
  (values (typed-bubble tt)
          (untyped-bubble uu)))

(define (typed-bubble pp)
  (typed-codeblock* (list pp)) #;
  (bubblebox pp #:color deep-brush-color))

(define (untyped-bubble pp)
  (untyped-codeblock* (list pp)) #;
  (bubblebox pp #:color untyped-brush-color))

(define (bubblebox pp #:color [color white])
  (add-rounded-border
    pp
    #:radius 10
    #:background-color color
    #:frame-width bbox-frame-width
    #:frame-color bbox-frame-color
    #:x-margin (browncs-x-margin)
    #:y-margin (browncs-y-margin)))

(define (snoc x* x)
  (append x* (list x)))

(define (how-natural-pict)
        (word-append
          @bodyrmlo{ Q. How does  } @bodyrmhi{Natural} @bodyrmlo{  enforce }
          (deep-name) @bodyrmlo{ types?}))

(define (how-transient-pict)
        (word-append
          @bodyrmlo{ Q. How does  } @bodyrmhi{Transient} @bodyrmlo{  enforce }
          (shallow-name) @bodyrmlo{ types?}))

(define (how-transient-a)
  (word-append @bodyrmlo{A. With } @bodyrmhi{no wrappers} @bodyrmlo{ but many tiny } @bodyrmem{shape checks}))

(define (deep-prop-pict)
    (deep-codeblock* (list (lc-append
            @coderm{Type Soundness}
            @coderm{Complete Monitoring}))))

(define (at-du)
  (at-find-pict '|Deep Typed| rc-find 'cc #:abs-x (* 3/4 interaction-x-sep)))

(define (at-ds)
  (at-find-pict '|Deep Typed| cb-find 'ct #:abs-y (* 35/100 interaction-y-sep)))

(define (at-su)
  (at-find-pict '|Shallow Typed| rc-find 'cc #:abs-x (* 3/4 interaction-x-sep)))

(define (conclusion-pict nn)
    (table2
      #:col-sep small-x-sep
      #:row-sep (* 2 tiny-y-sep)
      #:col-align lt-superimpose
      #:row-align lt-superimpose
      (take/hide (* (+ 1 nn) 2)
      (list
        @bodyrmhi{Context:} (ll-append
                              @bodyrmlo{Different GT strategies exist}
                              @bodyrmlo{ (for good reason!)})
        @bodyrmhi{Inquiry:} (ll-append
                               @bodyrmlo{Can two extreme strategies interoperate?}
                               (yblank pico-y-sep)
                               (word-append (deep-name) @bodyrmlo{ types via } @bodyrmhi{Natural} @bodyrmlo{ (wrappers)})
                               (yblank 0)
                               (word-append (shallow-name) @bodyrmlo{ types via } @bodyrmhi{Transient} @bodyrmlo{ (no wrappers)}))
        @bodyrmhi{Contribution:} (ll-append
                             @bodyrmlo{Yes! In a way that:}
                             (word-append @bodyrmlo{ - preserves their formal } @bodyrmem{guarantees})
                             (word-append @bodyrmlo{ - leads to better overall } @bodyrmem{performance})
                             (word-append @bodyrmlo{ - lets TR } @bodyrmem{express} @bodyrmlo{ additional programs}))
        )
      )
      ))

(define (coming-soon-pict)
    (hc-append
      small-x-sep
      (bbox
      (hb-append
        pico-x-sep
        (make-simple-flag
          (blank 40 10)
          #:flag-border-color deep-pen-color
          #:flag-background-color shallow-pen-color)
        (lc-append
          @bodyrmlo{Opens a new dimension in}
          @bodyrmlo{gradual language design})))
      (bbbox
        (hc-append
          tiny-x-sep
          @bodyrmlo{Coming soon to Racket}
          (symbol->lang-pict 'racket)))))

(define (take/hide nn x*)
  (for/list ((x (in-list x*))
             (i (in-naturals)))
    (if (< i nn)
      x
      (bghost x))))

(define (types-and-checks nn)
    (ppict-do
      (shallow-codeblock*
        #:title "and back, with a new type."
        (list
      @coderm{f2 : Str -> Str}
      (word-append @codebf{def } @coderm{ f2 = f1})))
      #:go (coord 0 55/100 'rc #:abs-x (+ 0))
      ((if (< nn 1) bghost values)
      (hc-append
        tiny-x-sep
        (bbox
          (ll-append
            (word-append @bodyemrm{Types say  } @coderm{f2 : Str -> Str})
            (word-append @bodyemrm{Checks say  } @coderm{f2} @bodyrmlo{ is a function})))
        right-arrow-pict))))


;; -----------------------------------------------------------------------------

(define the-title-str "Deep and Shallow Types for Gradual Languages")

(define (sec:title)
  (pslide
    ;;#:next
    #:alt (
    #:go title-coord-m
    (let* ([title-pict
             (bbox
               #:y-margin small-y-sep
               (let* ((str* (string-split the-title-str " for ")))
                 (vc-append
                   (titlerm (car str*))
                   (titlerm2 (string-append "for " (cadr str*))))))]
           [tu-pict
             (vc-append
               pico-y-sep
               (boundary-node '(U D U D D))
               (boundary-node '(U U D D))
               #;(boundary-node '(D D U U D))
               )]
           [ben-pict (vr-append
                       -4
                       @subtitlerm{Ben Greenman}
                       (yblank 12)
                       @subtitlerm{2022-06-16})]
           [brown-pict
             #;(scale-to-width% (bitmap "img/browncs-logo.png") 14/100)
             (vl-append
               tiny-y-sep
               @coderm{Northeastern}
               @coderm{-> Brown*}
               @coderm{-> Utah})]
           [author-pict (bbox (hc-append small-x-sep ben-pict brown-pict))])
      (vc-append
        tiny-y-sep
          (vc-append tiny-y-sep title-pict (bghost tu-pict))
        author-pict))
    )
  )
  (void))

(define (sec:intro)
  (pslide
    ;; waters => TU dilemma
    #:go title-coord-m
    (let ()
      (define-values [tt uu] (tu-bubbles))
      (let* ((fg
                (hc-append med-x-sep
                           tt
                           (let ((rr (vrule (h%->pixels 5/100) #:thickness 4)))
                             (hc-append 4 rr rr))
                           uu))
             (hh (* 2 (pict-height fg)))
             (ww (* 3/4 client-w))
             (bg (cc-superimpose
                   (filled-rectangle (* 2 ww) hh #:color white #:draw-border? #f)
                   (hc-append
                     (filled-rectangle ww hh #:color typed-bg-color #:draw-border? #f)
                     (filled-rectangle ww hh #:color untyped-bg-color #:draw-border? #f)))))
        (cc-superimpose bg fg)))
    (yblank small-y-sep)
    @bodyrmlo{Q. Should your PL be typed or untyped?}
    #:next
    (yblank med-y-sep)
    (word-append @bodyrmhi{Gradual typing} @bodyrmlo{ says } @bodyrmhi{yes to both})
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{"} @bodyemrm{best} @bodyrmlo{"} @bodyrmem{ of two worlds})
  )
  (pslide
    ;; idea really took off, past 15 years
    ;; ... great ... inspiring ... terrific ... 
    #:go heading-coord-m
    @headrm{Great Idea!}
    @bodyrmlo{Inspired MANY Languages Over 16+ Years}
    (yblank med-y-sep)
    #:next
    (lang-grid (all-lang-pict*))
    #:next
    #:go center-coord
    (question-box
      (word-append
        ;; caveat:
        @bodyrmlo{No agreement on the } @bodyrmhi{semantics} @bodyrmlo{ of gradual types}))
  )
  (pplay
    #:skip-first? #t
    (let ((pict-a (lang-grid (all-lang-pict*)))
          (pict-b (four-camps-pict 'C 'T 'E 'N)))
    (lambda (pp step-n)
      (ppict-do
        pp
    #:go heading-coord-m
    (bghost @headrm{Great Idea!})
    (bghost @bodyrmlo{Inspired MANY Languages Over 16+ Years})
    (yblank med-y-sep)
    (cellophane pict-a (max 0 (- 1 (* 2 step-n))))
    #:go heading-coord-m
    (bghost @headrm{Great Idea!})
    (bghost @bodyrmlo{Inspired MANY Languages Over 16+ Years})
    (cellophane pict-b (min 1 (max 0 (- (* 2 step-n) 1/2))))
  ))))
  (pslide
    #:go heading-coord-m
    (bghost @headrm{Great Idea!})
    (bghost @bodyrmlo{Inspired MANY Languages Over 16+ Years})
    #:alt (
    (four-camps-pict 'C 'T 'E 'N)
    #:go (at-find-pict 'C ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Concrete")
    #:go (at-find-pict 'T ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Transient")
    #:go (at-find-pict 'E ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Erasure")
    #:go (at-find-pict 'N ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Natural")
    )
    (cellophane (four-camps-pict 'C 'T 'E 'N) 1/2)
    #:go (at-find-pict 'C ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Concrete")
    #:go (at-find-pict 'T ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Transient")
    #:go (at-find-pict 'E ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Erasure")
    #:go (at-find-pict 'N ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Natural")
    #:go (coord 1/2 58/100 'cb)
    #:alt (
      (yblank tiny-y-sep)
    ;; ... if you want to use GT to reason about behavior, gotta pay cost
    ;; ... costs can be prohibitive = high and unpredictable
      (bbox
        (lc-append
          (word-append @bodyrmhi{4} @bodyrmlo{ leading})
          @bodyrmlo{semantics because of a tradeoff:}
          (blank)
          (word-append @bodyrmlo{type } @bodyemrm{guarantees} @bodyrmlo{ vs. } @bodyemrm{performance} @bodyrmlo{ costs})
          (word-append @bodyrmlo{vs. } @bodyrmem{expressiveness})))
    )
    #:next
    #:go (at-find-pict 'C cc-find 'ct #:abs-y (- small-y-sep)) (not-today "limited interop w/ untyped")
    #:next
    #:go (at-find-pict 'E cc-find 'ct #:abs-y (- small-y-sep)) (not-today "unsound interop")
    #:next
    #:go (at-find-pict 'T cc-find 'ct #:abs-y (* 0 pico-y-sep)) (shallow-today-pict)
    #:go (at-find-pict 'N cc-find 'ct #:abs-y (* 0 pico-y-sep)) (deep-today-pict)
  )
  (pslide
    ;; without further ado, this is the main RQ
    #:go heading-coord-m
    (bghost @headrm{Great Idea!})
    (bghost @bodyrmlo{Inspired MANY Languages Over 16+ Years})
    (cellophane (four-camps-pict 'C 'T 'E 'N) 0)
    #:go (at-find-pict 'T ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Transient")
    #:go (at-find-pict 'N ct-find 'ct #:abs-y (- tiny-y-sep)) (camp-lbl "Natural")
    #:go (at-find-pict 'T cc-find 'ct #:abs-y (* 0 pico-y-sep)) (shallow-today-pict2)
    #:go (at-find-pict 'N cc-find 'ct #:abs-y (* 0 pico-y-sep)) (deep-today-pict2)
    #:go heading-coord-m
    @headrm{Starting Point}
    #:next
    #:go hi-text-coord-m
    (rq-text
      @bodyrmlo{Can Natural and Transient interoperate?})
    (yblank med-y-sep)
    (hc-append tiny-x-sep right-arrow-pict (dsu-icon) left-arrow-pict)
    #:next
    (yblank med-y-sep)
    (bbox
    (ll-append
      (word-append @bodyrmhi{Motivations} @bodyrmlo{:})
      (word-append @bodyrmlo{  - ease the } @bodyrmem{guarantees} @bodyrmlo{ vs. } @bodyrmem{performance} @bodyrmlo{ tradeoff})
      (word-append @bodyrmlo{  - no loss of } @bodyrmem{expressiveness} @bodyrmlo{; same static types})))
    #:next
    (yblank tiny-y-sep)
    (hc-append
      tiny-x-sep
      @bodyrmlo{Orthogonal to basic improvements:} 
      (blank)
      (venue-pict "Pycket" "OOPSLA'17")
      #;(venue-pict "Collapsible" "OOPSLA'18")
      #;(venue-pict "Set-Based Analysis" "DLS'18")
      (venue-pict "Corpse Reviver" "POPL'21"))
  )
  (pslide
    ;; now come to **the** technical question
    ;; what to do at the boundaries?
    ;; have 2 strategies
    ;; can they interop?!
    ;; - shallow prop = TS (spell it out, no surprises in theorem though it is weak)
    ;; - deep prop = TS + CM
    #:go heading-coord-m
    @headrm{Key Technical Question:}
    @bodyrmlo{How to Enforce Types at Boundaries?}
    ;; want to preserve properties (what are they?)
    ;; care about base values, data structures, higher-order values
    #:go center-coord
    #:alt ( (dsu-interaction 0) )
    (dsu-interaction 1)
    #:go (at-du) (bbox @coderm{?})
    #:go (at-ds) (bbox @coderm{?})
    #:go (at-su) (bbox @coderm{?})
    #:go (coord 1/2 slide-text-bottom 'ct #:abs-y tiny-y-sep)
    (vc-append
      tiny-y-sep
      @bodyrmlo{While preserving their formal properties}
      (ht-append
        tiny-x-sep
        (vv-pict "OOPSLA'19")
        (vv-pict "ICFP'18")))
    #:next
    #:go (at-find-pict '|Deep Typed| ct-find 'cb #:abs-y (* 35/100 region-h))
    (deep-prop-pict)
    #:next
    #:go (at-find-pict '|Shallow Typed| ct-find 'cb #:abs-y (* 35/100 region-h))
    (shallow-codeblock* (list @coderm{Type Soundness}))
    #:next
    #:go (at-find-pict '|Untyped| ct-find 'cb #:abs-y (* 35/100 region-h))
    (untyped-codeblock* (list @coderm{Dyn. Soundness}))
  )
  (void))

(define (sec:2way)
  (pslide
    #:go heading-coord-m
    @headrm{Key Technical Question:}
    @bodyrmlo{How to Enforce Types at Boundaries?}
    #:next
    #:go center-coord
    (question-box
      (ll-append
        @bodyrmlo{First of all:}
        (how-natural-pict)
        (yblank 0)
        (how-transient-pict)
        ))
  )
  (parameterize ((current-slide-assembler deep-bg))
  (pslide
    #:go heading-coord-m (how-natural-pict)
    #:go hi-text-coord-m (du-interaction 1)
    #:next
    #:go slide-text-coord-m
    (word-append @bodyrmlo{A. Use } @bodyrmem{wrappers} @bodyrmlo{ to guard boundaries})
    #:next
    #:go (coord 50/100 1/2 'ct #:sep tiny-y-sep)
    #:alt ( (type-boundary 1 'L (deep-code "Int -> Int") (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))) (deep-codeblock* (list @codebf{[wrap]}))) )
    (type-boundary 2 'L (deep-code "Int -> Int") (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))) (deep-codeblock* (list @codebf{[wrap]})))
    #:next
    #:alt ( (type-boundary 1 'R (deep-code "Vectorof Int") (deep-codeblock* (list (word-append @codebf{vec} @coderm{ 1 2 3}))) (untyped-codeblock* (list @codebf{[wrap]}))) )
    (type-boundary 2 'R (deep-code "Vectorof Int") (deep-codeblock* (list (word-append @codebf{vec} @coderm{ 1 2 3}))) (untyped-codeblock* (list @codebf{[wrap]})))
    ;;#:next
    ;;#:alt ( (type-boundary 1 'L (deep-code "Int") (untyped-codeblock* (list @coderm{42})) (deep-codeblock* (list @codebf{42}))) )
    ;;(type-boundary 2 'L (deep-code "Int") (untyped-codeblock* (list @coderm{42})) (deep-codeblock* (list @codebf{42})))
  )
    (void))
  (parameterize ((current-slide-assembler shallow-bg))
  (pslide
    ;; forget about barrier, too costly: allocation indirection checking
    #:go heading-coord-m (how-transient-pict)
    #:go hi-text-coord-m (su-interaction 1)
    #:next
    #:go slide-text-coord-m
    (how-transient-a)
    #:next
    #:go (coord 50/100 1/2 'ct #:sep tiny-y-sep)
    #:alt ( (type-boundary 1 'L (shallow-code "Int -> Int") (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))) (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e})))) )
    (type-boundary 2 'L (shallow-code "Int -> Int") (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))) (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))))
    #:next
    #:alt ( (type-boundary 1 'L (shallow-code "Vectorof Int") (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C}))) (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C})))) )
    (type-boundary 2 'L (shallow-code "Vectorof Int") (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C}))) (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C}))))
    #:next
    #:alt ( (type-boundary 1 'R (shallow-code "Int -> Int") (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'}))) (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'})))) )
    (type-boundary 2 'R (shallow-code "Int -> Int") (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'}))) (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'}))))
  )
  (pslide
    ;; forget about barrier, too costly: allocation indirection checking
    #:go heading-coord-m (how-transient-pict)
    #:go slide-text-coord-m
    (how-transient-a)
    #:go (coord 50/100 1/2 'ct #:sep tiny-y-sep)
    (type-boundary 2 'L (shallow-code "Int -> Int") (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))) (untyped-codeblock* (list (word-append @codebf{fun} @coderm{ x . e}))))
    (type-boundary 2 'L (shallow-code "Vectorof Int") (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C}))) (untyped-codeblock* (list (word-append @codebf{vec} @coderm{ A B C}))))
    (type-boundary 2 'R (shallow-code "Int -> Int") (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'}))) (shallow-codeblock* (list (word-append @codebf{fun} @coderm{ x . e'}))))
    #:go hi-text-coord-m
    #:alt ( (su-interaction 3) )
    (su-interaction 4)
    #:go (at-find-pict '|Shallow Typed| lb-find 'ct #:abs-y pico-y-sep)
    (bbox
      (lc-append
        @bodyrmlo{Check function calls,}
        @bodyrmlo{vector refs, etc.}))
  )
    (void))

  ;; - #checks way up, but cost of each one is way lower
  ;; great tradeoff when T U mixed, not great when T dominates
  (void))

(define (sec:3way)
  (pslide
    #:alt (
      #:go center-coord (dsu-interaction 1)
      #:alt (
      #:go (at-du) (bbox @coderm{?})
      #:go (at-ds) (bbox @coderm{?})
      #:go (at-su) (bbox @coderm{?})
      )
      #:go (at-du) (bbox (word-append @coderm{1. } @codebf{wrap}))
      #:go (at-ds) (bbox @coderm{?})
      #:alt (
        #:go (at-su) (bbox @coderm{?})
      )
      #:go (at-su) (bbox (word-append @coderm{2. } @codebf{shape} @coderm{ } @codebf{check}))
    )
    #:go center-coord (dsu-interaction 1 #:su-blur #t)
    #:go (at-du) (bbox (word-append @coderm{1. } @codebf{wrap}))
    #:go (at-su) (bbox (word-append @coderm{2. } @codebf{shape} @coderm{ } @codebf{check}))
    #:go (at-ds) (tag-pict (bbox @coderm{?}) 'bb)
    #:next
    #:go (at-find-pict 'bb rc-find 'lc #:abs-x pico-x-sep)
    (bbox
      (lc-append
        @bodyrmlo{Typed to Typed = no check?}
        @bodyrmlo{No!}))
  )
  (pslide
    #:go heading-coord-m
    @headrm{What If: No Checks Between Deep and Shallow}
    #:go hi-text-coord-l
    (ll-append
      @bodyrmlo{Example 1:}
      @bodyrmlo{  Deep code cannot trust Shallow types because}
      @bodyrmlo{  they are lazily enforced})
    #:go (coord 55/100 14/100 'lt)
    (shallow-codeblock*
      #:title "Shallow makes a function,"
      (list
    (word-append @codebf{def } @coderm{ f0(n : Int):})
    @coderm{  n + 2}))
    (yblank small-y-sep)
    #:next
    (untyped-codeblock*
      #:title "sends it to untyped code ..."
      (list
        (xblank (pict-width @coderm{f2 : Str -> Str}))
        (word-append @codebf{def } @coderm{ f1 = f0})))
    (yblank small-y-sep)
    #:next
    #:alt ( (types-and-checks 0) )
    (types-and-checks 1)
    ;;@bodyrmlo{u shim => no static error}
    (yblank small-y-sep)
    #:next
    (deep-codeblock*
      #:title "Deep gets a 'bad' function"
      (list
    @coderm{f3 : Str -> Str}
    (word-append @codebf{def } @coderm{ f3 = f2})))
  )
  (pslide
    #:go heading-coord-m
    @headrm{What If: No Checks Between Deep and Shallow}
    #:go hi-text-coord-l
    (bghost (ll-append
      @bodyrmlo{Example 1:}
      @bodyrmlo{  Deep code cannot trust Shallow types because}
      @bodyrmlo{  they are lazily enforced}))
    (ll-append
      @bodyrmlo{Example 2:}
      @bodyrmlo{  Shallow can send a Deep value to}
      @bodyrmlo{  Untyped code})
    #:go (coord 55/100 1/2 'lc)
    (deep-codeblock*
      #:title "Deep makes a function,"
      (list
    (word-append @codebf{def } @coderm{ g0(h : Int -> Int):})
    @coderm{  h(3)}))
    (yblank small-y-sep)
    #:next
    (shallow-codeblock*
      #:title "sends it to Shallow,"
      (list
        @coderm{g1 : (Int -> Int) -> Int}
        (word-append @codebf{def } @coderm{ g1 = g0})))
    (yblank small-y-sep)
    #:next
    (untyped-codeblock*
      #:title "which sends it to untyped"
      (list
        (xblank (pict-width @coderm{g2 : Str -> Str}))
        (word-append @codebf{def } @coderm{ g2 = g1})
        @coderm{g2("not a function")}))
  )
  (pslide
    #:go center-coord (dsu-interaction 1 #:su-blur #t)
    #:go (at-du) (bbox (word-append @coderm{1. } @codebf{wrap}))
    #:go (at-su) (bbox (word-append @coderm{2. } @codebf{shape} @coderm{ } @codebf{check}))
    #:alt (
      #:go (at-ds) (tag-pict (bbox @coderm{?}) 'bb)
      #:go (at-find-pict 'bb rc-find 'lc #:abs-x pico-x-sep)
      (bbox
        (lc-append
          @bodyrmlo{Typed to Typed = no check?}
          @bodyrmlo{No!}))
    )
    #:go (at-ds) (bbox (word-append @coderm{3. } @codebf{wrap}))
    #:next
    #:go heading-coord-m
    (bbox
      (hc-append
        tiny-x-sep
        (check-pict 60)
        @bodyrmlo{In paper: model, type soundness, complete monitoring}))
  )
  (void))

(define (sec:impl)
  ;; TODO
  ;; 1. built in TR
  ;; 2. subject for another talk, Transient TR at <P>
  ;; 3. tying together brought own challenges
  ;; 4. *** mention the N^2 problem ***
  ;; 5. ideally, langs independent, so if we add a 4th, the others don't need to change
  ;; 6. that's NOT what we have, every export gotta prepare for all 3 just in case
  ;; 7. alt = force the programmer ... => not terrible to remind, but redundant code & checks
  (pslide
    #:go heading-coord-m @headrm{Implementation}
    (tag-pict
    (let ((pp (racket-pict)))
      (hc-append tiny-x-sep (bghost pp) @bodyrmlo{Typed Racket} pp))
    'impl)
    #:go center-coord (dsu-interaction 1 #:su-blur #f)
    #:go center-coord 
    #:next
    #:alt (
      #:go (at-find-pict '|Shallow Typed| cb-find 'ct #:abs-y (- small-y-sep))
      (bbox
        (venue-pict #:box? #f
        "A Transient Semantics for Typed Racket"
        "Programming'22"))
    )
    #:next
    #:go (at-find-pict 'impl cc-find 'cc)
    (bbox (word-append @bodyrmlo{In paper: general lessons   (} @bodyrmhi{no macros} @bodyrmlo{)}))
  )
  (pslide
    #:go title-coord-m
    @headrm{Evaluation}
    (yblank small-y-sep)
    (word-append
      @bodyrmem{Guarantees}
      @bodyrmlo{ vs. }
      (tag-pict @bodyrmem{Performance} 'perf)
      @bodyrmlo{ vs. }
      @bodyrmem{Expressiveness})
    #:next
    #:go (at-find-pict 'perf cc-find 'cc)
    (bbox @bodyrmem{Performance} #:x-margin 6)
    #:go (at-find-pict 'perf cb-find 'ct #:abs-y tiny-y-sep)
    up-arrow-pict
  )
  (void))

(define (sec:perf)
  (pslide
    ;; TODO show library nodes
    #:go (coord slide-left slide-text-top 'lt)
    (hc-append small-x-sep (racket-pict)
               (ppict-do
                 @bodyrmlo{GTP Benchmarks}
                 #:go (coord 0 1 'lt #:abs-x pico-x-sep #:abs-y -4)
                 @bodyrmlo{21 programs}))
    (yblank small-y-sep)
    (tag-pict
      (benchmark-pict
      "img/gtp-bench.png"
      #:w% 41/100
      #:url "docs.racket-lang.org/gtp-benchmarks")
      'gtp-perf)
    #:next
    #:go (coord 47/100 8/100 'lt)
    #:alt ( (whence-lattice 0) )
    #:alt ( (whence-lattice 1) )
    (whence-lattice 2)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Better Performance}
    (yblank tiny-y-sep)
    (question-box
      @bodyrmlo{Q. How many points run fastest with a Deep + Shallow mix?})
    #:next
    (yblank med-y-sep)
    #:alt ( (mixed-best-pict 0) )
    (mixed-best-pict 1)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Better Performance}
    (yblank tiny-y-sep)
    (vc-append
      pico-y-sep
      (ppict-do
        (question-box
          @bodyrmlo{Q. What is the worst-case overhead?})
        #:go (coord 1 1/2 'lc #:abs-x tiny-x-sep)
          (hc-append
            (deep-codeblock "Deep")
            @bodyrmlo{ or }
            (shallow-codeblock "Shallow")))
      #;(hc-append small-x-sep
                 (path-node '(U D D))
                 (let ((vv (vrule (pict-height (path-node '(U))))))
                   (hc-append 2 vv vv))
                 (path-node '(U S S))))
    #:next
    (yblank pico-y-sep)
    ;; TODO colors, legend
    ;; TODO highlight zombie, gotta explain
    #:alt ( (overhead-table 0) )
    (overhead-table 2)
    #:next
    #:go (at-find-pict 'zombie lc-find 'rc #:abs-x (- pico-x-sep))
    (hc-append
      pico-x-sep
      (scale
        (bbox
          #:x-margin 8
          (lc-append
            @bodyrm{H.O. values and}
            @bodyrm{many elim. forms}))
        6/10)
      right-arrow-pict)
  )
  (pslide
    ;; - general trend: D and S have pitfalls
    ;;   - D fine untyped, excellent typed, dead in middle
    ;;   - S gets incrementally worse (not quite dead, but bad)
    #:go heading-coord-m
    @headrm{Better Performance}
    #:next
    (yblank small-y-sep)
    @bodyrmlo{Overall: switching between Deep and Shallow}
    @bodyrmlo{ can avoid perf. bottlenecks}
    (yblank small-y-sep)
    (ht-append
      med-x-sep
      (bbox
        (lc-append
          (word-append
            (deep-name)
            @bodyrmlo{ near the top,})
          @bodyrmlo{to maximize the benefits of types}))
      (bbox
        (lc-append
          (word-append
            (shallow-name)
            @bodyrmlo{ in the middle,})
          @bodyrmlo{to minimize the cost of boundaries})))
    (yblank small-y-sep)
    (scale (pathology-lattice 5) 5/10)
    (yblank med-y-sep)
    ;;#:next
    ;;(bbox
    ;;  @bodyrmlo{Future Work: fine-grained recommendations})
  )

  #;(pslide
    ;; optional: paths
    ;; - things are so much better that paths work out
    ;;   ... gotta explain what a path is
    ;;   ... then again, "gradual conversion" is a good bottom line to emphasize
  )

  (void))

(define (sec:expr)
  ;; TODO venn diagram deep within shallow
  ;; - other big payoff, can express more gradual combinations
  ;;   because of TRANSIENT (gotta say transient)
  ;; - unclear from benchmarks, b/c started off with code that works fully typed
  ;; - but, often get "stuck" in the middle (you know if you've programmed in this world)
  ;; - example 1:  ... mcar?
  ;; - example 2: any wrap
  ;; - example 3: index-of
  (pslide
    #:go heading-coord-m
    @headrm{Expressiveness}
    (yblank small-y-sep)
    @bodyrmlo{Deep types may reject good programs}
    #:next
    (yblank med-y-sep)
    (email-panels (snoc (glob "img/email/*png") (caution-pict 100)))
  )
  #;(pslide
  ;; TODO enough time for this?!
    #:go heading-coord-m
    @headrm{Expressiveness}
    (yblank small-y-sep)
    @bodyrmlo{Deep wrappers are hard to implement,}
    @bodyrmlo{and may not exist for certain types}
    #:next
    (yblank small-y-sep)
    (ht-append
      small-x-sep
      (ds-codeblock* (list
@tcoderm{(: stx-unbox (-> (Syntaxof (Boxof Int))}
@tcoderm{                 Int))}
@tcoderm{(define (stx-unbox s)}
@tcoderm{  (unbox (syntax-e s)))}
))
      (untyped-codeblock* (list
@tcoderm{(stx-unbox (syntax (box 42)))}
)))
    (yblank small-y-sep)
    (table2
      #:col-sep small-x-sep
      #:row-sep tiny-y-sep
      (list
        (deep-name) @coderm{Error: no contract for type}
        (shallow-name) @coderm{OK}))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Expressiveness}
    (yblank small-y-sep)
    @bodyrmlo{Deep must enforce the top type (Any) against higher-order clients}
    ;; cite Findler + Blume?
    #:next
    (yblank small-y-sep)
    (ht-append
      med-x-sep
      (ds-codeblock* (list
@tcoderm{(define b : (Boxof Integer)}
@tcoderm{  (box 0))}
@tcoderm{}
@tcoderm{(define any : Any b)}))
      (untyped-codeblock* (list
@tcoderm{(set-box! any 1)}
)))
    (yblank small-y-sep)
    (table2
      #:col-sep small-x-sep
      #:row-sep tiny-y-sep
      (list
        (deep-name) @coderm{Error: cannot mutate an Any-wrapped value}
        (shallow-name) @coderm{OK}))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Expressiveness}
    (yblank small-y-sep)
    @bodyrmlo{Deep wrappers may change behaviors}
    #:next
    (yblank small-y-sep)
    (ht-append
      small-x-sep
      (ds-codeblock* (list
@tcoderm{(require/typed racket/list}
@tcoderm{  [index-of}
@tcoderm{    (All (T)}
@tcoderm{      (-> (Listof T) T}
@tcoderm{          (U Natural}
@tcoderm{             #false)))])}
@tcoderm{}
@tcoderm{(index-of '(a b) 'a)}
))
      (untyped-codeblock* (list
@tcoderm{(index-of '(a b) 'a)}
)))
    (yblank small-y-sep)
    (table
      3
      (list
        (deep-name) @coderm{#false} @coderm{ = not found}
        (shallow-name) @coderm{0} @coderm{ = found at position 0})
      lc-superimpose cc-superimpose
      (cons small-x-sep 0) tiny-y-sep)
  )
  (pslide
    ;; fundamental? perhaps not (2 3 probably are), but definitely a practical issue
    ;;  same practical issue that motivates optional types in the first place
    ;; ... or, just cut to the chase ... important practical motivation
    #:go heading-coord-m
    @headrm{Expressiveness}
    #:next
    (yblank small-y-sep)
    (lc-append
      @bodyrmlo{Overall: migrating from Untyped to Shallow is more}
      @bodyrmlo{likely to work as intended})
    (yblank med-y-sep)
    (email-panels (snoc (glob "img/email/*png") (check-pict 120)))
  )
  (void))

(define (sec:end)
  (pslide
    #:go title-coord-m
    @headrm{Conclusion}
  )
  (pslide
    #:go (coord 96/100 10/100 'rt)
    (dsu-icon)
    #:go (coord (* 5/2 slide-text-left) slide-text-top 'lt)
    #:alt ( (conclusion-pict 0) )
    #:alt ( (conclusion-pict 1) )
    (conclusion-pict 2)
    ;; #:next
    ;; #:go (coord 1/2 95/100 'cb)
    ;; (coming-soon-pict)
  )
  (pslide
    #:go heading-coord-m
    @headrm{A New Dimension for Gradual Typing}
    #:go hi-text-coord-m
    (yblank small-y-sep)
    (let* ((pp
            (hc-append
              big-x-sep
              (add-hubs
                (deep-codeblock*
                  (list (hc-append @coderm{Deep} (xblank small-x-sep))))
                'deep)
              (add-hubs
                (shallow-codeblock*
                  (list (hc-append (xblank med-x-sep) @coderm{Shallow} (xblank med-x-sep))))
                'shallow)
              (add-hubs (untyped-name) 'untyped)))
           (pp
             (add-code-line*
               pp
               (list
                 (code-arrow 'deep-E rc-find 'shallow-W lc-find 0 0 0 0 'solid)
                 (code-arrow 'shallow-E rc-find 'untyped-W lc-find 0 0 0 0 'solid))))
           )
      pp)
    #:next
    #:go (at-find-pict 'deep ct-find 'cb #:abs-y (- tiny-y-sep) #:abs-x tiny-x-sep)
    (on-stick (values #;bbox @bodyrmhi{Natural}))
    #:go (at-find-pict 'shallow ct-find 'cb #:abs-y (- tiny-y-sep) #:abs-x (+ small-x-sep))
    (on-stick (values #;bbox @bodyrmhi{Transient}))
    #:next
    #:go (coord 1/2 1/2 'ct #:sep small-y-sep)
    (ht-append (bbox @bodyrmlo{Q. More regions along the spectrum?}) (xblank med-x-sep))
    #:next
    (ht-append (xblank big-x-sep) (bbox @bodyrmlo{Q. Better cooperation b/w Deep and Shallow?}))
    #:next
    (ht-append (bbox @bodyrmlo{Q. Solve the N^2 interop problem?}) (xblank (- med-x-sep tiny-x-sep)))
  )
  (pslide
    #:go heading-coord-m
    @headrm{The End}
    #:go center-coord
    (ppict-do
      (dsu-icon2)
      #:go center-coord
      (racket-pict))
    (yblank small-y-sep)
    @bodyrmlo{Coming soon to Racket}
    (yblank pico-y-sep)
    @coderm{https://racket-lang.org}
    (yblank big-y-sep)
    (scale
      (table2
        #:row-sep small-y-sep
        (list
        @bodyrmlo{Pull Request}
        @coderm{https://github.com/racket/typed-racket/pull/948})
        @bodyrmlo{Research Repo}
        @coderm{https://github.com/bennn/g-pldi-2022})
      9/10)
  )

  (void))

(define (sec:qa)
  (void))

(define (sec:unused)
  ;(pslide
  ;  #:go center-coord
  ;  @bodyrmlo{DSU alt 1}
  ;  ;; escape analysis (tough!)
  ;  ;; DS free to share unless value escapes or originally from untyped
  ;  ;; D can make all the wrappers
  ;)
  ;(pslide
  ;  #:go center-coord
  ;  @bodyrmlo{DSU alt 2}
  ;  ;; escape analysis again
  ;  ;; DS noop
  ;  ;; S conditionally wraps
  ;)
  ;; ---
  (pslide
    #:go heading-coord-m
    @headrm{Optimizations}
    #:go hi-text-coord-m
    (ht-append
      med-x-sep
    (table2
      #:col-sep small-x-sep
      #:row-sep pico-y-sep
      (list
        ;; TODO shallow or transient?
        @bodyrmlo{Topic} @bodyrmlo{Ok for Shallow TR?}
        @coderm{apply} @codeemrm{y}
        @coderm{box} @codeemrm{y}
        @coderm{dead-code} @codeembf{N}
        @coderm{extflonum} @codeemrm{y}
        @coderm{fixnum} @codeemrm{y}
        @coderm{float-complex} @codeemrm{y}
        @coderm{float} @codeemrm{y}
        ))
    (table2
      #:col-sep small-x-sep
      #:row-sep pico-y-sep
      (list
        @bodyrmlo{Topic} @bodyrmlo{Ok?}
        @coderm{list} @codeemrm{y}
        @coderm{number} @codeemrm{y}
        @coderm{pair} @codeembf{N}
        @coderm{sequence} @codeemrm{y}
        @coderm{string} @codeemrm{y}
        @coderm{struct} @codeemrm{y}
        @coderm{vector} @codeemrm{y})))
    (yblank tiny-y-sep)
    (hrule (w%->pixels 5/10))
    (yblank tiny-y-sep)
    @tcoderm{https://prl.ccs.neu.edu/blog/2020/01/15/the-typed-racket-optimizer-vs-transient}
  )
  #;(pslide
    #:go heading-coord-m
    @headrm{Example: Retic. and Dyn}
    #:go slide-text-coord-m
    @bodyrmlo{Local vars often get the Dynamic type and skip blame-map updates}
    (yblank tiny-y-sep)
    (typed-codeblock* (list
@tcoderm{def permutations(iterable:List(int))->List(List(int)):}
@tcoderm{    pool = tuple(iterable)}
@tcoderm{    n = len(pool)}
@tcoderm{    r = n}
@tcoderm{    indices = list(range(n))}
@tcoderm{    cycles = list(range(n-r+1, n+1))[::-1]}
@tcoderm{    result = [ [pool[i] for i in indices[:r]] ]}
@tcoderm{    while n:}
@tcoderm{        for i in reversed(range(r)):}
@tcoderm{            cycles[i] -= 1}
@tcoderm{            if cycles[i] == 0:}
@tcoderm{                indices[i:] = indices[i+1:] + indices[i:i+1]}
@tcoderm{                cycles[i] = n - i}
@tcoderm{            else:}
@tcoderm{                ....}
;; @tcoderm{                j = cycles[i]}
;; @tcoderm{                indices[i], indices[-j] = indices[-j], indices[i]}
;; @tcoderm{                result.append([pool[i] for i in indices[:r]])}
;; @tcoderm{                break}
;; @tcoderm{        else:}
;; @tcoderm{            return result}
;; @tcoderm{    return result}
))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Limitation}
    #:go slide-text-coord-m
    (word-append
      @bodyrmlo{Neither }
      @bodyemrm{Deep}
      @bodyrmlo{ nor }
      @bodyemrm{Shallow}
      @bodyrmlo{ TR allows occurrence types at a boundary})
    (yblank tiny-y-sep)
    (typed-codeblock* (list
@tcoderm{(require/typed racket/function}
@tcoderm{  (identity (-> Any Boolean : String)))}
@tcoderm{;; ^ Not permitted!}
@tcoderm{}
@tcoderm{(define x : Any 0)}
@tcoderm{}
@tcoderm{(define fake-str : String}
@tcoderm{  (if (identity x)}
@tcoderm{      (ann x String)}
@tcoderm{      (error 'unreachable)))}
@tcoderm{ }
@tcoderm{(string-length fake-str)}
))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Acknowledgments}
    #:next
    (yblank small-y-sep)
    @bodyrmlo{Current    &    Past}
    (yblank small-y-sep)
    (let* ((s1 (lambda (pp) (scale-to-fit pp (w%->pixels 20/100) (h%->pixels 16/100))))
           (s2 (lambda (pp) (scale-to-fit pp (w%->pixels 24/100) (h%->pixels 16/100)))))
      (table2
        #:col-sep (w%->pixels 10/100)
        #:row-sep (h%->pixels 5/100)
        #:col-align cc-superimpose
        (list
          (s1 (bitmap "img/browncs-logo.png"))
          (bbox (s2 (bitmap "img/neu-logo.png")))
          (bbox (s2 (bitmap "img/cra-logo.png")) #:x-margin 2 #:y-margin 2)
          (s1 (bitmap "img/nsf-logo.png")))))
  )
  (pslide
    ;; recruiting
    ;; Q. replace transient for better T to T interaction
    ;; Q. D+S how to program in 3 world
    ;; Q. address N^2 problem
    ;; q. optimizing transient
    ;; q. optimizing natural
    #:go center-coord
    (tag-pict
    (add-rounded-border
      (scale-to-fit (bitmap "img/flux-bg.jpeg") (* 95/100 client-w) (* 98/100 client-h))
      #:x-margin 1
      #:y-margin 1
      #:radius 0.1
      #:frame-width 1
      #:frame-color black) 'uu)
    #:go (at-find-pict 'uu ct-find 'cc)
    (add-rounded-border
      (bitmap "img/the-u.png")
      #:x-margin small-x-sep
      #:y-margin tiny-y-sep
      #:radius 0.1
      #:frame-width 1
      #:frame-color black)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Artifact}
    #:go (coord 98/100 2/100 'rt)
    (bbox (bitmap "img/aec.png") #:x-margin 0 #:y-margin 0)
    #:go center-coord
    @bodyrmhi{Software Heritage}
    (scale-to-fit
      @coderm{http://archive.softwareheritage.org/swh:1:dir:2f1f76cafb72491d8526d18ae556499065ac6853}
      (* 3/4 client-w) 200)
    (yblank small-y-sep)
    @bodyrmhi{Zenodo}
    @coderm{https://doi.org/10.5281/zenodo.6498925}
    (yblank small-y-sep)
    @bodyrmhi{GitHub}
    @coderm{https://github.com/bennn/g-pldi-2022}
  )
  (pslide
    #:go (coord 96/100 10/100 'rt)
    (dsu-icon)
    #:go (coord (* 5/2 slide-text-left) slide-text-top 'lt)
    (conclusion-pict 2)
    #:go (coord 1/2 95/100 'cb)
    (coming-soon-pict)
  )

  (void))

;; --- 

(define bg-orig (current-slide-assembler))
(define bg-cs.brown.edu (slide-assembler/background bg-orig make-bg))
(define title-cs.brown.edu (slide-assembler/background bg-orig make-titlebg))
(define waters-bg (slide-assembler/background bg-orig make-waters))
(define deep-bg (slide-assembler/background bg-orig make-deepbg))
(define shallow-bg (slide-assembler/background bg-orig make-shallowbg))

(define (do-show)
  ;; (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; [current-page-number-font page-font]
  ;; [current-page-number-color white]
  ;; --
  (parameterize ((current-slide-assembler waters-bg))
    (sec:title)
    (void))
  (parameterize ((current-slide-assembler bg-cs.brown.edu)
                 (pplay-steps 30))
    (sec:intro)
    (sec:2way)
    (sec:3way)
    (sec:impl)
    (sec:perf)
    (sec:end)

    (pslide)
    ;(sec:expr)
    ;(sec:qa)
    ;(pslide)

    (void))
  (void))

;; -----------------------------------------------------------------------------

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         ;;(define client-w 984) (define client-h 728) ;; 4:3
         (define client-w 1320) (define client-h 726) ;; 16:9 sort of, too thin
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)
    #;(make-titlebg client-w client-h)



  )))
