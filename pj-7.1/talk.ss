#lang at-exp slideshow

;; 30 min slot
;; 20:00 March 15
;; https://docs.google.com/presentation/d/1zbi3st3HDC29_o79D9vgjwEFo4l5VDKnAN92TXBlLcE/edit#slide=id.g2128e74c6b5_0_10

;; Inspiration:
;; - g-pldi-2022/talk/talk.ss
;; - ~/code/uu/cs3020/talk.ss

;;   - gradual class hierarchy
;;     - rare in RW: Thorn, SafeTS don't allow it
;;     - Nom allows plenty, makes dispatch tricky, optimistic / pess.
;;     - no m overloading (python)
;;     - single inheritance
;;     - typed code, type-based override
;;     - no @property for typed fields
;;     - attrs set by __init__ (all PEP checkers want this)
;;   - table for all generic/concrete instantiations
;;   - no recursive types anywhere (Dyn for that, as default like for Set[T]?)
;;   - no **kwargs, coming soon

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

(define turn revolution)

(define x%->pixels w%->pixels)
(define y%->pixels h%->pixels)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define border-x-sep (w%->pixels 4/100))
(define small-x-sep (w%->pixels 5/100))
(define smol-x-sep small-x-sep)
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define smol-y-sep small-y-sep)
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))

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
(define lesson-x 18/100)

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
(define hi-text-coord-l hi-text-coord-left)
(define hi-text-coord-m   hi-text-coord-mid)
(define hi-text-coord-r hi-text-coord-right)
(define hi-text-coord-ll  (coord 48/100 hi-text 'rt))
(define hi-text-coord-rr (coord 52/100 hi-text 'lt))
(define lo-text-coord-left (coord slide-text-left lo-text 'lt))
(define lo-text-coord-mid (coord 1/2 lo-text 'ct))
(define lo-text-coord-right (coord slide-text-right lo-text 'rt))
(define title-coord-m (coord 1/2 26/100 'ct))
(define all-lang-coord (coord 99/100 1/2 'rc))
(define lesson-coord-h (coord lesson-x hi-text  'lt))
(define lesson-coord-m (coord lesson-x (+ 15/100 hi-text) 'lt))
(define lesson-coord-l (coord lesson-x (+ 30/100 hi-text) 'lt))

(define img "img")
(define src img)

(define default-line-width 4)
(define default-arrow-size 14)
(define large-arrow-size 18)

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

;; TODO
(define python-blue (hex-triplet->color% #x357C9F))

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
(define shallow-color utah-sunrise)
(define concrete-color utah-crimson)
(define primitive-color utah-lake)
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
(define bg-color utah-darkgrey)

(define (color-off c)
  (color%-update-alpha c 0.2))

(define title-font "Montserrat" #;"Bree Serif")
(define body-font "Source Sans Pro" #;"Open Sans")
(define code-font "Inconsolata")

(define title-size 42)
(define subtitle-size 32)
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
(define subtitlermem (make-string->text #:font (bold-style title-font) #:size subtitle-size #:color dark-orange))
(define subtitlermemlo (make-string->text #:font title-font #:size subtitle-size #:color dark-orange))
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
(define rm bodyrmlo)
(define rmem (make-string->text #:font body-font-lo #:size body-size #:color dark-orange))
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

(define (at-find-right tag)
  (at-find-pict tag rc-find 'lc #:abs-x pico-x-sep))

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

(define (author-append . pp*)
  (apply hc-append (pict-width @rm{xxx}) pp*))

(define (affiliation-pict)
 (bbox
  (ht-append smol-x-sep (brown-logo) (meta-logo))))
; (((     (vc-append
;        tiny-y-sep
;        @subtitlermemlo{Brown University}
;        (brown-logo))
;      (vc-append
;        tiny-y-sep
;        @subtitlermlo{Meta}
;        (meta-logo)))))

(define main-logo-w 200)
(define main-logo-h 100)

(define (brown-logo)
  (main-logo "img/browncs-logo.png"))

(define (meta-logo)
  (main-logo "img/meta-logo.png"))

(define (main-logo str [ww main-logo-w] [hh main-logo-h])
  (freeze (scale-to-fit (bitmap str) ww ww)))

(define ((slide-assembler/background2 base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define (make-solid-bg w h color)
  (let* ((bg (filled-rectangle w h #:color white #:draw-border? #f))
         (fg (filled-rectangle w h #:color color #:draw-border? #f)))
    (cc-superimpose bg fg)))

(define (make-bg w h) (make-solid-bg w h bg-color))

(define bg-orig (current-slide-assembler))
(define bg-bg (slide-assembler/background2 bg-orig make-bg))

(define (make-deepbg w h)
  (make-solid-bg w h deep-bg-color))

(define (make-shallowbg w h)
  (make-solid-bg w h shallow-bg-color))

(define browncs-x-margin (make-parameter small-x-sep))
(define browncs-y-margin (make-parameter tiny-y-sep))

(define bbox-radius (make-parameter 1))
(define bbox-x-margin (make-parameter small-x-sep))
(define bbox-y-margin (make-parameter tiny-y-sep))
(define bbox-frame-width (make-parameter 2))
(define bbox-frame-color (make-parameter utah-granite))

(define (bbox pp
              #:color [color white]
              #:x-margin [x-margin #f]
              #:y-margin [y-margin #f]
              #:frame-color [frame-color #f]
              #:backup? [backup? #f])
  (define xm (or x-margin (bbox-x-margin)))
  (define ym (or y-margin (bbox-y-margin)))
  (define rr (bbox-radius))
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
    #:frame-width (bbox-frame-width)
    #:frame-color (or frame-color (bbox-frame-color))))

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

(define (bvrule h #:thickness [thickness #f] #:color [color #f])
  (ben-rule (or thickness 1) h #:color color))

(define (bhrule w #:thickness [thickness #f] #:color [color #f])
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
                       (color%-update-alpha background-color 0.4)))))
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

(define (ucode str)
  (untyped-codeblock* (list (coderm str))))

(define (tcode str)
  (typed-codeblock* (list (coderm str))))

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

(define (pblank pp)
  (blank (pict-width pp) (pict-height pp)))

(define (bblur pp #:alpha [alpha #f] #:bg [bg? #f])
  (define fg (cellophane pp (or alpha 4/10)))
  (if bg?
    (cc-superimpose (bgrect fg) fg)
    fg))

(define (bgrect pp)
  (brect pp bg-color))

(define (brect pp cc)
  (filled-rectangle (pict-width pp) (pict-height pp) #:draw-border? #f #:color cc))

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
                #:row-sep [pre-row-sep 4]
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

    ;; NOTE room for research / improvement
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
  (ruby-pict))

(define (ruby-pict)
  (symbol->lang-pict 'ruby))

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

(define sp-pict
  (let ((pp (box #f)))
    (lambda ()
      (or (unbox pp)
          (let ((vv (freeze (scale-lang-lo (static-python-logo #:title? #f)))))
            (set-box! pp vv)
            vv)))))

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

(define (label-above base . pp*)
  (vc-append 0 (apply vc-append 2 pp*) base))

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

(define (title-pict)
  (let* ([title-pict
           (bbox
             #:y-margin small-y-sep
             (let* ((str* (string-split the-title-str " Lessons")))
               (vc-append
                 (titlerm (car str*))
                 (titlerm2 (string-append "Lessons" (cadr str*))))))]
         [pypy
           (freeze (bitmap "img/python-large.png"))]
         [ben-pict
           (vc-append
             smol-y-sep
             (vc-append 8
               (author-append
                          @subtitlermemlo{Kuang-Chen Lu}
                          @subtitlermem{Ben Greenman}
                          @subtitlermlo{Carl Meyer}
                          @subtitlermlo{Dino Viehland})
               (author-append
                          @subtitlermlo{Aniket Panse}
                          @subtitlermemlo{Shriram Krishnamurthi}))
             )]
         [author-pict
           (vc-append
             tiny-y-sep
             (bbox ben-pict)
             (hc-append
               med-x-sep
               (affiliation-pict)
               (bbox @subtitlerm{‹Programming› 2023})))]
         [fg (vc-append smol-y-sep title-pict author-pict)])
    (ppict-do
      (pblank fg)
      #:go (coord 80/100 82/100 'cc) (cellophane pypy 0.5)
      #:go center-coord fg)))

(define (static-python-logo #:title? [title? #t])
  (define mm 2)
  (define oo 12)
  (define py (big-python-pict))
  (define body
    (add-rounded-border
      #:x-margin mm
      #:y-margin mm
      (ppict-do
        (blank (+ 40 (pict-width py)) (- (pict-height py) 0))
        #:go (coord 1/2 8/100 'ct)
        (ppict-do py
          #:go (coord 1 0 'rt #:abs-x oo #:abs-y (- oo))
          (insta-pict)))))
  (if title?
    (vc-append tiny-y-sep @titlerm2{Static Python} body)
    body))

(define (big-python-pict)
  (define ww 300)
  (main-logo "img/python-large.png" ww ww))

(define (insta-pict)
  (define ww 90)
  (main-logo "img/instagram.jpeg" ww ww))

(define (py-migration n)
  ;; TODO check insta syntax for functions, return types
  ;; https://pandas.pydata.org/pandas-docs/stable/reference/
  (define arr right-arrow-pict)
  (define ucode* (list
@coderm|{# Python code}|
@coderm|{}|
@coderm|{def join(d0,d1,sort,how):}|
@coderm|{  ....}|
@coderm|{}|
@coderm|{}|
))
  (define tcode* (list
@coderm|{def join(d0:DataFrame,}|
@coderm|{         d1:DataFrame,}|
@coderm|{         sort:Bool,}|
@coderm|{         how:Left|Right)}|
@coderm|{    -> DataFrame:}|
@coderm|{  ....}|
))
  (define uu (untyped-codeblock* ucode*))
  (define tt
    (vc-append
      tiny-y-sep
      (typed-codeblock* (list @coderm{DataFrame}))
      (typed-codeblock* (list @coderm{Bool}))
      (typed-codeblock* (list @coderm{Left|Right}))))
  (define gg
    (let ((maxw (+ 4 (apply max (map pict-width tcode*)))))
      (cc-superimpose
        (untyped-codeblock*
          (list*
            @coderm{# Python + Types}
            @coderm{}
            @coderm{}
            (map (lambda (pp) (blank maxw (pict-height pp))) ucode*)))
        (typed-codeblock* tcode*))))
  (if (< n 3)
    (hc-append
      tiny-x-sep
      (tag-pict uu 'ucode)
      arr
      ((if (< n 1) bghost values) tt)
      ((if (< n 1) bghost values) arr)
      (tag-pict ((if (< n 2) bghost values) gg) 'gcode))
    gg))

(define (types-traditional-benefits)
    (bbox
      (vc-append
        tiny-y-sep
        (table2
          ;; TODO icons
          #:col-sep tiny-x-sep
          @rm{Types for:} @rm{static checks}
          (blank) @rm{run-time guarantees}
          (blank) @rm{optimizations})
        @rm{vs. cost of untyped interop.})))

(define (happy-face)
  (tiny-face 'sortof-happy))

(define (confused-face)
  (tiny-face 'unhappy))

(define (tiny-face sym)
  (scale (face sym) 24/100))

;; -----------------------------------------------------------------------------

(define the-title-str "Gradual Soundness: Lessons from Static Python")

(define (sec:title)
  (pslide
    #:next
    #:go title-coord-m
    (title-pict))
  (void))

(define (sec:what)
  (pslide
    #:next
    #:go hi-text-coord-ll
    (static-python-logo)
    #:next
    #:go hi-text-coord-rr
    (yblank med-y-sep)
    @rm{Enhanced Python, by Instagram}
    (yblank smol-y-sep)
    (word-append
      @rm{ +2 years running  } @bodyrm{in production})
    #:next
    (yblank smol-y-sep)
    (ll-append
      @rm{Gradually typed}
      (word-append
        @rm{ ... for some value of  }
        @rmem{gradual}))
    )
  (pslide
    #:go heading-coord-r
    @titlerm2{What is Gradual Typing?}
    ;; one main point of talk = subtle question!
    #:next
    #:go hi-text-coord-m
    (bbox @rm{Idea: combine the best parts of typed and untyped code})
    ;; untyped as before, flexible ... idiomatic, handy, lovable, concise
    ;; typed, use statics soundness performance
    #:next
    (yblank smol-y-sep)
    #:alt ((py-migration 0)
           #:go (at-find-pict 'ucode ct-find 'lc #:abs-x smol-x-sep)
           (bbox
             (hc-append
               (confused-face) @rm{  so many params!})))
    #:alt ((py-migration 1))
    (py-migration 2)
           #:go (at-find-pict 'gcode rt-find 'rc #:abs-x (- pico-x-sep))
           (bbox (happy-face))
    #:next
    #:go (coord 1/2 40/100 'ct)
    (bbox
      (lc-append
        @rm{Great!}
        @rm{}
        @rm{But, what happens when}
        (hc-append
          (typed-codeblock* (list @rm{typed code}))
          @rm{  and  }
          (untyped-codeblock* (list @rm{untyped code})))
        @rm{interact?}
        @rm{}
        @rm{Are types sound?}))
  )
  (pslide
    #:go heading-coord-r
    @titlerm2{What is Gradual Typing?}
    #:next
    #:go hi-text-coord-l
    (three-answers 0)
    #:next
    #:go (at-find-right 'a1)
    #:alt ((a1-optional 0))
    #:alt ((a1-optional 1))
    (a1-optional 2)
    #:next
    #:go (at-find-right 'a2)
    #:alt ((a2-deep 0))
    #:alt ((a2-deep 1))
    (a2-deep 2)
    #:next
    ;; TODO perf interlude?? (highlight word in RED then use RED background or something for the detour)
    ;; why = 1: lattice, 2: build suspense for the SP perf improvement
    ;; 2014: "in the context of current impl tech sound gradual typing is dead"
    #:go (at-find-right 'a3)
    (a3-concrete 2)
    #:go (at-find-pict 'a3 cb-find 'ct #:abs-y pico-y-sep)
    (parameterize ((bbox-x-margin pico-x-sep))
      (bbox @rm{Today!}))
  )
  (pslide


    ;; TODO
    #:go heading-coord-l
    (add-hubs @headrm{A3.} 'a3)
    #:go (at-find-right 'a3)
    (a3-concrete 2)
    #:go hi-text-coord-m
    (yblank pico-y-sep)
    ;; recent 20% slowdown on revert to pure Python, a state that never existed in practice
    (ht-append
      smol-x-sep
      (bbox @rm{Sound types*})
      (bbox @rm{541 typed modules}))
    (yblank smol-y-sep)
    (bbox
      (word-append @bodyrm{3.7% boost} @rm{ to CPU efficiency}))
    (yblank smol-y-sep)
    #:go (coord 35/100 54/100 'rt)
    (frame
      (ppict-do
        (blank 400 200)
        #:go (coord 3/10 3/10) (untyped-icon)
        #:go (coord 4/10 6/10) (untyped-icon)
        #:go (coord 2/10 7/10) (typed-icon)
        ;; #:go (coord 7/10 4/10) (typed-icon)
        #:go (coord 8/10 8/10) (untyped-icon)
        ))
    @rm{Mix of typed and untyped}
    #:go (coord 65/100 54/100 'lt)
    (frame
      (ppict-do
        (blank 460 200)
        #:go (coord 2/100 1/2 'lc)
        @coderm{requests ->}
        #:go (coord 40/100 10/100 'lt)
        (frame @coderm{control, P+Cython})
        #:go (coord 40/100 90/100 'lb)
        (frame @coderm{experiment, SP})))
    @rm{Efficiency = requests / sec. at full load}
    )
  (void))

(define (sec:how)
  (center-slide
    "How is Static Python so Fast?")
  (pslide
    ;; TODO icon for each step??!
    ;; https://github.com/facebookincubator/cinder
    ;; img/lang/cinder.svg
    #:go heading-coord-m
    @titlerm2{Step 0. Better Compiler, Better Runtime}
    #:next
    #:go hi-text-coord-m
    ;; TODO staging
    (runtime-support 2)
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 1. Fast Soundness Checks}
    #:next
    #:go hi-text-coord-ll
    (tag-pict
      (vc-append
        smol-y-sep
        (untyped-codeblock* (list @coderm{avg(nums)}))
        (tag-pict down-arrow-pict 'boundary)
        ;; TODO Num? Int?
        ;; TODO pylist?, chklist?
        (typed-codeblock*
          (list
            @coderm{def avg(ns:List[Num]) -> Num:}
            @coderm{  ....})))
      'main)
    #:go (at-find-right 'boundary)
    (tag-pict (bbox @rm{Tag check}) 'lbl)
    #:go (at-find-right 'lbl)
    (ll-append
      @rm{  ++ no traversal, no wrapper}
      @rm{  -- no Python lists allowed!})
    #:go (coord 1/2 1/2 'ct)
    (yblank med-y-sep)
    (bbox @rm{Every sound type has an O(1) tag check})
    ;; prior: Thorn, Nom, Dart 2
    ;; warning: no gradual lattice here
    (yblank tiny-y-sep)
    ;; TODO detour, defer checks to Pyre/pyre
    ;(word-append
    ;  @rm{First-class function types are }
    ;  @rmem{unsound})
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 2. Progressive Types}
    #:next
    (yblank med-y-sep)
    (pstripe 'p0 'p1 'p2)
    #:next
    #:go (at-find-pict 'p1 cc-find 'cc)
    (tag-pict (typed-codeblock* (list @coderm{ChkList[Num]})) 'clist)
    #:go (at-find-pict 'p2 cc-find 'cc)
    (tag-pict (bghost @tcode{Int64}) 'pint)
    #:next
    #:go (at-find-pict 'clist cc-find 'cc #:abs-y (- (stripe-h)))
    (tag-pict (typed-codeblock* (list @coderm{PyList})) 'slist)
    #:set (bvline ppict-do-state 'slist 'clist)
    #:next
    #:go (at-top-left 'p0)
    (types-nametag "Shallow" "Python value-shapes")
    #:go (at-top-left 'p1)
    (types-nametag "Concrete" "sound generics")
    #:go (at-find-pict 'clist cb-find 'ct #:abs-y pico-y-sep)
    (hc-append
      pico-y-sep
      @tcode{ChkDict[String, Num]}
      @tcode{ChkList[T]})
    #:go (at-find-pict 'pint cc-find 'cc #:abs-y (- (stripe-h)))
    (tag-pict @tcode{Int} 'sint)
    #:go (at-find-pict 'slist rt-find 'lt #:abs-x med-x-sep)
    (vc-append
      pico-y-sep
      @tcode{PyDict}
      (hc-append smol-x-sep @tcode{String} @tcode{Bool}))
    #:next
    #:go (at-find-pict 'pint cc-find 'cc)
    (ppict-do @tcode{Int64}
              #:go (coord 1/2 12 'ct #:abs-y pico-y-sep) @tcode{Array[Float32]})
    #:set (bvline ppict-do-state 'sint 'pint)
    #:go (at-top-left 'p2)
    (types-nametag "Primitive" "C values")
    )
  (pslide
    ;; NOTE room for research / improvement
    #:go heading-coord-m
    @titlerm2{Step 3. Limited Dyn Type}
    (yblank smol-y-sep)
    #:alt ((dyn-theory-practice 0))
    #:alt ((dyn-theory-practice 1))
    (dyn-theory-practice 2)
    (yblank pico-y-sep)
    #:alt ((sp-types-enable 0))
    (sp-types-enable 1)
    #:next
    (yblank tiny-y-sep)
    (pvstripe 'stype 'ctype 'ptype)
    #:go (at-bot-mid 'stype) (shallow-box)
    #:go (at-bot-mid 'ctype) (concrete-box)
    #:go (at-bot-mid 'ptype) (primitive-box)
    #:next
    ;; TODO wider stripes, shorter stripes
    ;; TODO label optimizations, maybe: dispatch; fast boundaries/checks; unboxing
    ;; TODO blur after showing
    #:go (at-top-mid 'stype)
    (vc-append
      tiny-y-sep
      ;(untyped-codeblock*
      ;  (list
      ;    @coderm{class A:}
      ;    @coderm{ def f(self):}))
      (typed-codeblock*
        (list
          @coderm{class A:}
          @coderm{ def f(self)->Int:}))
      (typed-codeblock*
        (list
          @coderm{class B(A):}
          @coderm{ def f(self):}
          @coderm{   # Type Error})))
    #:next
    #:go (at-top-mid 'ptype)
    (typed-codeblock*
      (list
        @coderm{x:Int64 = 42}
        @coderm{y = x}
        @coderm{# Type Error}))
    #:next
    #:go (at-top-mid 'ctype)
    (vc-append
      pico-y-sep
      (typed-codeblock*
        (list
          @coderm{def avg(ns:ChkList[Num]):}))
      (untyped-codeblock*
        (list
          @coderm{avg([1,2])}
          @coderm{# Runtime Error})))
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 4. Limited Scope}
    (yblank med-y-sep)
    (bbox
      (lc-append
        @rm{Focus on high-payoff optimizations}
        @rm{rather than feature-completeness}))
    #:next
    (yblank med-y-sep)
    ;; TODO table2
    (defer-to 'python
              (vc-append tiny-y-sep
                         (hc-append tiny-y-sep @ucode{eval} @ucode{first-class class})
                          @ucode{multiple inheritance}))
    (yblank med-y-sep)
    (defer-to 'pyre
              (hc-append tiny-y-sep @tcode{Callable[T0, T1]} @tcode{Setof[T]}))
    )
  (pslide
    #:go hi-text-coord-ll
    #:alt ((step-summary 0))
    (ppict-do
      (step-summary 1)
      #:go (coord 1 1/2 'lc #:abs-x smol-x-sep)
      (vc-append
        pico-y-sep
        (scale (pstripe-icon) 8/10)
        (bbox
          (lc-append
            @rm{Types gradually enable optimizations}
            @bodyrm{Gradual Soundness}))))
    #:next
    #:go (coord 1/2 58/100 'ct)
    #:alt ((gradual-soundness 0))
    (gradual-soundness 1)
    )

  (void))

(define (sec:lesson)
  (center-slide
    "Takeaways")
  (pslide
    #:go heading-coord-m
    @titlerm2{Takeaways}
    #:alt (
    #:go lesson-coord-h
    (ht-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "GT Researchers")
      (vc-append
        tiny-y-sep
        (bbox
          (ll-append
            @rm{Good problems in the Concrete space:}
            @rm{* help adding Concrete types}
            @rm{* fast tags for unions, functions}))
        (bbox
          (ll-append
            @rm{Need to understand perf tradeoffs:}
            @rm{* expressiveness vs perf}
            @rm{* gradual guarantee}))))
    )
    #:alt (
    #:go lesson-coord-m
    (hc-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "Practitioners")
      (bbox
        (ll-append
          @rm{Go progressive types!}
          @rm{* Why not JS? Ruby?}
          @rm{}
          (hc-append smol-x-sep (js-pict) (ruby-pict)))))
    )
    #:alt (
    #:go lesson-coord-l
    (hb-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "Language Designers")
      (bbox
        (ll-append
          @rm{Power of lightweight verification:}
          @rm{* Redex model of Static Python}
          @rm{* Converted 265 tests}
          @rm{* 5 critical soundness bugs, discovered}
          @rm{* 16 correctness issues})))
    )
    #:go lesson-coord-h
    (hc-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "GT Researchers")
      (bbox @rm{Gradual Types vs. Performance}))
    #:go lesson-coord-m
    (hc-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "Practitioners")
      (bbox
        (hc-append smol-x-sep @rm{Sound types pay off})))
    #:go lesson-coord-l
    (hc-append
      smol-x-sep
      (lesson-for (frame (blank 100 100)) "Language Designers")
      (bbox
        @rm{++ Lightweight verification:}))
    #:go bottom-coord-m
    (ppict-do
      (sp-pict)
      #:go (coord 1/2 1 'lc #:abs-x med-x-sep) @coderm{End})
    #;(hc-append med-x-sep (sp-pict) (pstripe-icon))
    )
  (void))

(define (sec:qa)
  (center-slide
    "Q/A")
  (pslide
    @rm{Unions?}
    )
  (void))

;; --- 

(define microbench-data
  ;; lots more in paper: SP JIT SF, T-Max T-Min Orig
  '((deltablue 1 0.59 0.30)
    (fannkuch  1 1.03 0.46)
    (nbody     1 1.09 0.24)
    (richards  1 0.53 0.22)))

(define (python-baseline-bar)
  (hrule 1 #:width 2 #:color python-blue #:alpha 0.9))

(define (runtime-support nn)
  ;; TODO use nn
  (define lhs
    ;; https://docs.python.org/3.8/library/dis.html#python-bytecode-instructions
    (bbox
      (vc-append
        smol-y-sep
        @rm{Type-Aware Bytecode}
        (table2
          #:row-sep pico-y-sep
          #:col-sep tiny-x-sep
          @coderm{CALL_FUNCTION} @rm{Python default}
          @coderm{INVOKE_METHOD} @rm{vtable lookup}
          @coderm{INVOKE_FUNCTION} @rm{direct call}))))
  (define rhs
    (bbox
      (vc-append
        smol-y-sep
        @rm{Cinder Runtime}
        (ll-append
          @rm{VTables}
          @rm{Method-based JIT}))))
  (ht-append med-x-sep lhs rhs))

(define (dyn-theory-practice n)
  (define lhs (dyn-vs-untyped "GT Theory" "=="))
  (define rhs (dyn-vs-untyped "Static Python" "!="))
  (ht-append
    med-x-sep
    ((if (< 1 n) bblur values) lhs)
    ((if (< n 1) bghost values) rhs)))

(define (dyn-vs-untyped name eq)
  (define shim (make-list 1 (bghost @coderm{XXXX})))
  (define lhs (untyped-codeblock* shim))
  (define rhs (typed-codeblock* shim))
  (define llbl @rm{Untyped code})
  (define rlbl @rm{Dyn-Typed code})
  (define hshim (bghost @rm{X}))
  (label-above
    (bbox
      (ht-append
        pico-x-sep
        (label-above lhs llbl)
        (word-append hshim (coderm eq) hshim)
        (label-above rhs rlbl)))
    (bodyrm name)))

(define (microbenchmarks [n 0] #:h [hh #f] #:w [ww #f])
  (define row->title first)
  (define row->python second)
  (define row->tmin third)
  (define row->tmax fourth)
  (define skip-len (length (car microbench-data)))
  (define y-max
    (let* ((nn (apply max (filter real? (flatten microbench-data))))
           (nn (exact-ceiling (* 10 nn)))
           (nn (/ nn 10)))
      nn))
  (parameterize ((plot-x-ticks no-ticks))
    (plot-pict
      (list
        (python-baseline-bar)
        (for/list ((acc (in-list (list row->python row->tmin row->tmax)))
                   (c0 (in-list (list untyped-color shallow-color primitive-color)))
                   (jj (in-naturals)))
          ;; TODO colorblind colors
          (rectangles
            (for/list ((rr (in-list microbench-data))
                       (ii (in-naturals)))
              (define x0 (+ (* ii skip-len) jj))
              (vector (ivl x0 (+ x0 1))
                      (ivl 0  (acc rr))))
            #:color (color%-update-alpha c0 2/10)
            #:line-color c0
            #:alpha (if (< n jj) 0 0.95))))
      #:y-max (+ 1/2 y-max)
      #:title #f
      #:x-label #f
      #:y-label #f
      #:legend-anchor 'no-legend
      #:width (or ww (x%->pixels 7/10))
      #:height (or hh (h%->pixels 6/10)))))

(define (step-summary n)
  (define fmt (if (< n 1) bodyrmlo bodyrm))
  (ll-append
    @rm{0. Better Compiler, Better Runtime}
    @rm{1. Fast Soundness Checks}
    @fmt{2. Progressive Types}
    @rm{3. Limited Dyn Type}
    @rm{4. Limited Overall Scope}))

(define (gradual-soundness n)
  (define lhs
    (label-above
      (ll-append
        @rm{959 typed modules}
        @rm{ 10 with Concrete => fast reads}
        @rm{ 16 with Primitives => unboxed math})
      @rm{March 2023 stats:}))
  (define rhs
  ;; TODO microbench
  ;; - typed = shallow
  ;; - refined = concrete + primitive
  ;; - T speedup = some benefits out of box, wow
  ;; - T slowdown = high cost of checks / low opt
  ;; - R all speedups, nice
    (label-above
      (microbenchmarks 2 #:h (* 2 big-y-sep) #:w (* 2 big-x-sep))
      @rm{Microbenchmarks (1x = Python, lower = faster)}))
  (ht-append med-x-sep lhs ((if (< n 1) bghost values) rhs)))

(define (a1-optional n)
  (define txt @rm{Optional static checks, nothing at run-time})
  (define logo (ts-pict))
  (define extra (hc-append @rm{How to debug?  } (untyped-codeblock* (list @coderm{join(42, "hola", ...)}))))
  (three-part-description n txt logo extra))

(define (a2-deep n)
  (define txt @rm{Static types + contracts})
  (define logo (tr-pict))
  (define extra (hc-append @rm{Performance?  } (join-call-huge)))
  (three-part-description n txt logo extra))

(define (join-call-huge)
  (untyped-codeblock*
    (list
      (word-append @coderm{join(} @codeembf{huge0} @coderm{, } @codeembf{huge1} @coderm{, ...)}))))

(define (a3-concrete n)
  (define txt (word-append
                @bodyrmlo{Progressive static types + tags}
                ; @rm{ (}
                ; (xblank 2)
                ; @bodyrm{gradual soundness}
                ; (xblank 2)
                ; @rm{)}
                ))
  (define logo (sp-pict))
  (define extra (blank))
  (three-part-description n txt logo extra))

(define (three-part-description n txt logo extra)
  (ppict-do
    txt
    #:go (coord 5/100 1 'lt #:abs-y pico-y-sep)
    (ppict-do
      ((if (< n 1) bghost values) logo)
      #:go (coord 1 3/10 'lt #:abs-x tiny-x-sep)
      ((if (< n 2) bghost values) extra))))

(define (center-slide str)
  ;; TODO ... need a "center" slide
  ;; ? rotate paintings?
  (pslide
    #:go center-coord
    (bodyrmem str)))

(define (stripe-h)
  (* 35/100 client-h))

(define (stripe cc [wscale #f])
  (filled-rectangle
    (* (or wscale 1) (+ (* 2 margin) client-w))
    (stripe-h)
    #:color cc
    #:draw-border? #f))

(define (vstripe cc)
  (filled-rectangle
    (* 1/3 client-w)
    (- (* 1/2 client-h) margin)
    #:color cc
    #:draw-border? #f))

(define (at-cc sym)
  (at-find-pict sym cc-find 'cc))

(define (at-top-left sym)
  (at-find-pict sym lt-find 'lt #:abs-x 8 #:abs-y 4))

(define (at-bot-mid sym)
  (at-find-pict sym cb-find 'cb #:abs-y -4))

(define (at-top-mid sym)
  (at-find-pict sym ct-find 'ct #:abs-y tiny-y-sep))

(define (types-nametag name what)
  (bbox
    (word-append
      (bodyrm (string-append name " types"))
      (bodyrmlo (string-append " for " what)))))

(define (pstripe s0 s1 s2)
  (define l0-color shallow-color)
  (define l1-color concrete-color)
  (define l2-color primitive-color)
  (bblur
    #:alpha 6/10
    #:bg #true
    (vc-append
      (tag-pict (stripe l0-color) s0)
      (hc-append
        (tag-pict (stripe l1-color 1/2) s1)
        (tag-pict (stripe l2-color 1/2) s2)))))

(define (pvstripe s0 s1 s2)
  (define l0-color shallow-color)
  (define l1-color concrete-color)
  (define l2-color primitive-color)
  (bblur
    #:alpha 6/10
    #:bg #true
    (ht-append
      (tag-pict (vstripe l0-color) s0)
      (tag-pict (vstripe l1-color) s1)
      (tag-pict (vstripe l2-color) s2))))

(define (pstripe-icon)
  ;; TODO cleanup
  (ppict-do
    (scale (pstripe 'a 'b 'c) 4/10)
    #:go (at-cc 'a) (shallow-box)
    #:go (at-cc 'b) (concrete-box)
    #:go (at-cc 'c) (primitive-box)))

(define (shallow-box)
  (bbox @bodyrm{Shallow}))

(define (concrete-box)
  (bbox @bodyrm{Concrete}))

(define (primitive-box)
  (bbox @bodyrm{Primitive}))

(define (sp-types-enable n)
  ;; TODO center, vertical
  (define sp-yes
    (word-append
      @rm{Types enable } @bodyrmem{optimizations}))
  (define sp-no
    (word-append
      (scale (lc-append
               @rm{Types enable arbitrary migrations}
               @rm{(gradual guarantees)}) 90/100)
      @coderm{   <<   }))
  (bbox (word-append
          ((if (< n 1) bghost values) sp-no)
          sp-yes)))

(define (bvline pp src tgt)
  (define arr (code-arrow src cb-find tgt ct-find (* 1/4 turn) (* 3/4 turn) 0 0 'solid))
  (add-code-line pp arr #:line-width 4 #:color (bbox-frame-color)))

(define (defer-to sym pp)
  (define-values [img name]
    (case sym
      ((python)
       (values (python-pict) "Python"))
      ((pyre)
       (values (pyre-pict) "Pyre"))
      (else
        (raise-argument-error 'defer-to "(or/c 'python 'pyre)" sym))))
  (define txt (hb-append smol-x-sep
                         (word-append
                           @coderm{==>  } @rm{defer to @|name| }) img))
  (hc-append tiny-x-sep  pp txt))

(define (lesson-for pp str)
  (hc-append smol-x-sep pp (rm str)))

(define (three-answers n)
  (hc-append
    smol-x-sep
    ((if (< n 1) values bghost) (py-migration 4))
    (vl-append
      big-y-sep
      ((if (< n 1) values bghost) (add-hubs @headrm{A1.} 'a1))
      ((if (< n 1) values bghost) (add-hubs @headrm{A2.} 'a2))
      (add-hubs @headrm{A3.} 'a3))))

;; -----------------------------------------------------------------------------


(define (do-show)
  ;; (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; [current-page-number-font page-font]
  ;; [current-page-number-color white]
  ;; --
  (parameterize ((current-slide-assembler bg-bg)
                 (pplay-steps 30))
    (sec:title)
    (sec:what)
    (sec:how)
    (sec:lesson)
    (pslide)
;;    (sec:qa)
    (void))
  (void))

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

    #:go hi-text-coord-l
    (three-answers 1)
    #:go (at-find-right 'a3)
    (a3-concrete 2)
    #:next
    #:go heading-coord-m
    @titlerm2{Experience}
    #:next
    #:go hi-text-coord-ll
    (bbox
      @rm{+500 modules with sound types})
    (frame
      (ppict-do
        (blank 400 200)
        #:go (coord 3/10 3/10) (untyped-icon)
        #:go (coord 4/10 6/10) (untyped-icon)
        #:go (coord 2/10 7/10) (typed-icon)
        ;; #:go (coord 7/10 4/10) (typed-icon)
        #:go (coord 8/10 8/10) (untyped-icon)
        ))
    #:next
    #:go hi-text-coord-rr
    (bbox
      @rm{3.9% efficiency boost})
    (frame
      (ppict-do
        (blank 460 200)
        #:go (coord 2/100 1/2 'lc)
        @coderm{requests ->}
        #:go (coord 40/100 10/100 'lt)
        (frame @coderm{control, P+Cython})
        #:go (coord 40/100 90/100 'lb)
        (frame @coderm{experiment, SP})))
    @rm{Efficiency = requests / sec. at full load}

  )))
