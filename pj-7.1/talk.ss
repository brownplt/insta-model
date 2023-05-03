#lang at-exp slideshow

;; TALKING NOTES
;; - can call chklist from untyped --- it's the value constructor not the types, don't have to type the full path!
;; - 

;; TODO
;; [X] thorn, strongscript + nom (M,T) at the end, related work shout out
;; [X] 541 is the old number, be clear old vs new
;; [X] add slide about Pyre: defer ; migrations => lots using Optional types,
;;      more info in paper; types do give benefits
;; [X] show good untyped avg() call; don't need types, do need value constructor

;; 30 min slot
;; 20:00 March 15
;; https://docs.google.com/presentation/d/1zbi3st3HDC29_o79D9vgjwEFo4l5VDKnAN92TXBlLcE/edit#slide=id.g2128e74c6b5_0_10

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
  images/icons/control
  (only-in pict/face face)
  (only-in racket/random random-sample)
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  pict
  ppict/2
  pict-abbrevs
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  (only-in slideshow para bt)
  plot/no-gui (except-in plot/utils min* max*))

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

(define at-sign @"@")

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
(define concrete-pen-color concrete-color)
(define primitive-pen-color primitive-color)
(define untyped-pen-color untyped-color)
(define shallow-brush-color (color%-update-alpha shallow-pen-color 0.4) #;lite-orange #;(hex-triplet->color% #xfdc008))
(define deep-brush-color (color%-update-alpha deep-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define concrete-brush-color (color%-update-alpha concrete-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define primitive-brush-color (color%-update-alpha primitive-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
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
(define hugerm (make-string->text #:font body-font-md #:size (+ 20 body-size) #:color black))
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
  (apply hc-append (pict-width @rm{xx}) pp*))

(define (affiliation-pict)
  (bbox
    (vc-append
      (yblank tiny-y-sep)
      (ht-append smol-x-sep (brown-logo) (meta-logo)))))

(define main-logo-w 200)
(define main-logo-h 100)

(define (brown-logo)
  (main-logo "img/browncs-logo.png"))

(define (meta-logo)
  (main-logo "img/meta-logo.png"))

(define (main-logo str [ww main-logo-w] [hh main-logo-h])
  (freeze (scale-to-fit (bitmap str) ww ww)))

(define (scale-to-square pp dim)
  (scale-to-fit pp dim dim))

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

(define (add-lang str)
  (string-append "lang/" str))

(define (add-src str)
  (string-append "img/" str))

(define add-img add-src)

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

(define (scale-lang-lo pp)
  (scale-to-fit pp 120 80))

(define (lang-lo str)
  (scale-lang-lo (bitmap str)))

(define (symbol->lang-pict sym #:ext [ext #f])
  (lang-lo (add-img (add-lang (format "~a.~a" sym (or ext 'png))))))

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

(define (concrete-code str)
  (concrete-codeblock #:title #f #:lang #f str))

(define (concrete-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang concrete"] . str*)
  (concrete-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (concrete-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color concrete-pen-color #:background-color concrete-brush-color))

(define (primitive-code str)
  (primitive-codeblock #:title #f #:lang #f str))

(define (primitive-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang primitive"] . str*)
  (primitive-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (primitive-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color primitive-pen-color #:background-color primitive-brush-color))

(define (ucode str)
  (untyped-codeblock* (list (coderm str))))

(define (tcode str)
  (typed-codeblock* (list (coderm str))))

(define (ccode str)
  (concrete-codeblock* (list (coderm str))))

(define (untyped-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color untyped-brush-color pp))

(define (typed-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color deep-brush-color pp))

(define (typed-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang typed"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

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

(define (bblur2 pp)
  (bblur pp #:alpha 0.7))

(define (maybe-bblur yes? pp)
  (if yes?  (bblur pp) pp))

(define (bgrect pp)
  (brect pp bg-color))

(define (brect pp cc)
  (filled-rectangle (pict-width pp) (pict-height pp) #:draw-border? #f #:color cc))

(define xsep xblank)
(define ysep yblank)

(define (check-pict h)
  (bitmap (check-icon #:color apple-green #:height h #:material rubber-icon-material)))

(define (stop-pict h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material plastic-icon-material)))

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

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

(define big-swatch-blank (blank (w%->pixels 6/100) small-y-sep))

(define (untyped-icon #:lbl [lbl "U"])
  (center-label
    (untyped-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (typed-icon #:lbl [lbl "T"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (center-label pp lbl)
  (ppict-do
    pp
    #:go (coord 1/2 46/100 'cc)
    (if lbl (scale (headrm lbl) 0.9) (blank))))

(define (tr-pict)
  (racket-pict))

(define (racket-pict)
  (symbol->lang-pict 'racket))

(define (ts-pict)
  (symbol->lang-pict 'typescript))

(define (flow-pict)
  (symbol->lang-pict 'flow))

(define (typed-clojure-pict)
  (ppict-do
    (symbol->lang-pict 'typed-clojure)
    #:go (coord 65/100 1/2 'cc)
    (clojure-pict)))

(define (clojure-pict)
  (symbol->lang-pict 'clojure))

(define (php-pict)
  (symbol->lang-pict 'php))

;; NOTE room for research / improvement
(define (pyre-pict)
  (symbol->lang-pict 'pyre))

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

(define (pyret-pict)
  (symbol->lang-pict 'pyret))

(define sp-pict
  (let ((pp (box #f)))
    (lambda ()
      (or (unbox pp)
          (let ((vv (freeze (scale-lang-lo (static-python-logo #:title? #f)))))
            (set-box! pp vv)
            vv)))))

(define (dart2-pict)
  (symbol->lang-pict 'dart))

(define (js-pict)
  (symbol->lang-pict 'javascript))

(define (safets-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{SafeTS}))

(define (strongscript-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{StrS.}))

(define (thorn-pict)
  (symbol->lang-pict 'thorn))

(define (nom-pict base)
  (define logo
    (freeze
      (ppict-do
        (scale-to-pict (bitmap "img/lang/nom.png") base)
        #:go (coord 0 1 'rb)
        (ben-rule 20 20 #:color white))))
  (define text
    (freeze (scale (bitmap "img/lang/nom-text.png") 9/10)))
  (hc-append text logo))

(define (label-below base . pp*)
  (vc-append 0 base (apply vc-append 2 pp*)))

(define (label-above base . pp*)
  (vc-append 0 (apply vc-append 2 pp*) base))

(define (python-pict)
  (symbol->lang-pict 'python))

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

(define (T<=>U)
  (define-values [tt uu]
    (parameterize ((bbox-x-margin pico-x-sep))
      (values (typed-icon) (untyped-icon))))
  (define arr (code-arrow tt rc-find uu lc-find 0 0 0 0 'solid))
  (let* ((pp (hc-append tiny-x-sep tt uu))
         (pp (add-code-line pp arr))
         (pp (scale pp 7/10)))
    pp))

(define (low-lbl str pp)
  (ppict-do pp #:go (coord 1/2 1 'cb #:abs-y (- 2)) (coderm str)))

(define (low-lbl2 str pp)
  (vc-append pico-y-sep pp (coderm str)))

(define insta-modules
  (let ((cache (box #f)))
    (lambda ()
      (or (unbox cache)
          (let ()
  (define txt
    (bbox
      (vc-append
        tiny-y-sep
        (lr-append
          (word-append
          @rm{+500 modules with }
          @bodyrm{sound types})
          @rm{(upgraded from Pyre)})
        ;; 20k typed exports, 10k untyped imports
        (word-append
          @rm{+30k  } (T<=>U) @rm{ interactions}))))
  (define mod
    (let* ((pp (blank (+ (pict-width txt) (* 2 smol-x-sep)) (* 2 (pict-height txt))))
           (tgt* '((12 28/100 29/100)
                   (06 45/100 72/100)
                   (11 75/100 40/100))))
      (parameterize ((current-pseudo-random-generator (make-pseudo-random-generator)))
        (random-seed 96345)
        (for*/fold ((pp pp))
                   ((tgt (in-list tgt*))
                    (_n (in-range (car tgt))))
          (define x0 (second tgt))
          (define y0 (third tgt))
          (define x (+ x0 (random-offset 20/100)))
          (define y (+ y0 (random-offset 16/100)))
          (define ii (if (zero? (random 3)) (typed-icon) (untyped-icon)))
          (ppict-do pp #:go (coord x y 'cc) ii)))))
  (define pp (vc-append tiny-y-sep txt mod))
  (set-box! cache pp)
  pp)))))

(define (random-offset fraction)
  (define nn (random))
  (- (* nn 2 fraction)
     fraction))

(define (insta-boost n)
  (define txt
    (bbox
      (word-append @bodyrm{3.9% increase}
                   @rm{  in CPU efficiency})))
  (define body
    (if (< n 1)
      (blank)
      (let* ((env (envelope-pict 48 30 #:color "Linen"))
             (top (add-hubs ((if (< n 2) bghost values) (hpictx 5 env)) 'top))
             (ss (hpictx 3 (server-pict 38 55)))
             (lhs (add-hubs ((if (< n 2) values add-stopwatch) (cbox (low-lbl2 "control" ss))) 'lhs))
             (rhs (add-hubs ((if (< n 2) values add-stopwatch) (ebox (low-lbl2 "experiment" ss))) 'rhs))
             (bot (ht-append smol-x-sep lhs rhs))
             (pp (vc-append med-y-sep bot top))
             (arr* (for/list ((tgt (in-list (if (< n 2) '() '(lhs-S rhs-S)))))
                     (code-arrow 'top-N ct-find tgt cb-find (* 1/4 turn) (* 1/4 turn) 1/2 1/2 'solid)))
             (pp (add-code-arrow* pp arr* #:color black)))
        pp)))
  (vc-append tiny-y-sep txt body))

(define (add-stopwatch pp)
  (ppict-do
    pp
    #:go (coord 1 1 'lb #:abs-y pico-y-sep #:abs-x (- pico-y-sep))
    (stopwatch-pict)))

(define (stopwatch-pict)
  ;; (bitmap (clock-icon 10 8 #:height 40))
  (define rr 40)
  (ppict-do
    (disk rr #:color white #:border-color black #:border-width 3)
    #:go (coord 46/100 1/2 'cb) (bvrule (* rr 1/3) #:thickness 2 #:color black)
    #:go (coord 54/100 1/2 'cb) (rotate (bvrule (* rr 1/3) #:thickness 2 #:color black) (* -6/100 turn))))

(define (serverbox pp)
  (parameterize ((bbox-x-margin tiny-x-sep))
    (bbox pp #:color utah-litegrey)))

(define cbox serverbox)
(define ebox serverbox)

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
@coderm|{         sort:bool,}|
@coderm|{         how:Left|Right)}|
@coderm|{    -> DataFrame:}|
@coderm|{  ....}|
))
  (define uu (untyped-codeblock* ucode*))
  (define tt
    (vc-append
      tiny-y-sep
      (typed-codeblock* (list @coderm{DataFrame}))
      (typed-codeblock* (list @coderm{bool}))
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
               (confused-face) @rm{  so many parameters!})))
    #:alt ((py-migration 1))
    (py-migration 2)
           #:go (at-find-pict 'gcode rt-find 'rc #:abs-x (- 4))
           (bbox (happy-face))
    #:next
    #:go (coord 1/2 40/100 'ct)
    (bbox
      (lc-append
        @rm{Great!}
        @rm{}
        (word-append @rm{But, } @bodyrm{what happens} @rm{ when})
        (yblank pico-y-sep)
        (hc-append
          (typed-codeblock* (list @rm{typed code}))
          @rm{  and  }
          (untyped-codeblock* (list @rm{untyped code})))
        (yblank pico-y-sep)
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
    #:go hi-text-coord-l
    #:alt ( (three-answers 1) #:go (at-find-right 'a3) (a3-concrete 2) )
    (bblur2 (three-answers 1)) #:go (at-find-right 'a3) (bblur2 (a3-concrete 2))
    #:go heading-coord-m
    @titlerm2{Experience @|at-sign| Instagram Web Server}
    #:next
    #:go hi-text-coord-ll
    (yblank (- smol-y-sep))
    (insta-modules)
    #:next
    #:go hi-text-coord-rr
    #:alt ((insta-boost 0))
    #:alt ((insta-boost 1))
    (insta-boost 2)
    )
  (void))

(define (sec:how)
  (center-slide
    "How is Static Python so Fast?")
  (pslide
    #:go heading-coord-m
    ;; https://github.com/facebookincubator/cinder
    @titlerm2{Step 0. Better Compiler & Runtime}
    #:next
    (yblank med-y-sep)
    (lc-append
      (freeze (scale (bitmap "img/cinder.png") 45/100))
      @tcoderm{https://github.com/facebookincubator/cinder})
    #:next
    (yblank smol-y-sep)
    ;; TODO staging
    #:alt ((runtime-support 0))
    (runtime-support 1)
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 1. Fast Soundness Checks}
    #:next
    #:go hi-text-coord-ll
    (tag-pict
      (vc-append
        smol-y-sep
        (tag-pict (untyped-codeblock* (list @coderm{avg(nums)})) 'ucode)
        (tag-pict down-arrow-pict 'boundary)
        (typed-codeblock*
          (list
            @coderm{def avg(ns:chklist[int]) -> int:}
            @coderm{  ....})))
      'main)
    #:next
    #:go hi-text-coord-rr
    (bbox
      (lc-append
        @rm{Q. How to enforce soundness?}
        (yblank tiny-y-sep)
        (word-append @rm{A. } @bodyrm{Tag check})
        (yblank tiny-y-sep)
        (parameterize ((bbox-x-margin pico-x-sep))
          (hc-append
            @rm{Is }
            @ucode{nums}
            @rm{ an instance of }
            @tcode{chklist[int]}
            @rm{ ?}))))
    #:next
    (yblank tiny-y-sep)
    (vc-append
      tiny-y-sep
      (hc-append
        (xblank tiny-x-sep)
        (table2
          #:row-sep tiny-y-sep
          #:col-sep tiny-x-sep
          (list
            (check-pict 40) @rm{Fast! No traversal, no wrapper}
            (stop-pict 40) @rm{Rejects built-in Python lists})))
      (vc-append 4
        @rm{Need constructor:}
        @ucode{avg(chklist[int](nums))}))
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 2. Progressive Types}
    #:next
    (yblank smol-y-sep)
    (pstripe 'p0 'p1 'p2)
    #:next
    #:go (at-find-pict 'p1 cc-find 'cc)
    (tag-pict (typed-codeblock* (list @coderm{chklist[int]})) 'clist)
    #:go (at-find-pict 'p2 cc-find 'cc)
    (tag-pict (bghost @tcode{int64}) 'pint)
    #:next
    #:go (at-find-pict 'clist cc-find 'cc #:abs-y (- (stripe-h)))
    (tag-pict (typed-codeblock* (list @coderm{list})) 'slist)
    #:set (bvline ppict-do-state 'slist 'clist)
    #:next
    #:go (at-top-left 'p0)
    (types-nametag "Shallow" "Python value-shapes")
    #:go (at-top-left 'p1)
    (types-nametag "Concrete" "sound generics")
    #:go (at-find-pict 'clist cb-find 'ct #:abs-y pico-y-sep)
    (hc-append
      pico-y-sep
      @tcode{chkdict[string, int]}
      @tcode{chklist[T]})
    #:go (at-find-pict 'pint cc-find 'cc #:abs-y (- (stripe-h)))
    (tag-pict @tcode{int} 'sint)
    #:go (at-find-pict 'slist rt-find 'lt #:abs-x med-x-sep)
    (vc-append
      pico-y-sep
      @tcode{dict}
      (hc-append pico-y-sep @tcode{string} @tcode{bool}))
    #:next
    #:go (at-find-pict 'pint cc-find 'cc)
    (ppict-do @tcode{int64}
              #:go (coord 1/2 1 'lt #:abs-x pico-y-sep #:abs-y pico-y-sep)
              @tcode{Array[float32]})
    #:set (bvline ppict-do-state 'sint 'pint)
    #:go (at-top-left 'p2)
    (types-nametag "Primitive" "C values")
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 3. Limited Dyn Type}
    #:next
    (yblank smol-y-sep)
    #:alt ((dyn-theory-practice 0))
    #:alt ((dyn-theory-practice 1))
    (dyn-theory-practice 2)
    (yblank pico-y-sep)
    (sp-types-enable 1)
    #:next
    (yblank tiny-y-sep)
    (pvstripe 'stype 'ctype 'ptype)
    #:go (at-bot-mid 'stype) (shallow-box "dispatch")
    #:go (at-bot-mid 'ctype) (concrete-box "fast checks")
    #:go (at-bot-mid 'ptype) (primitive-box "unboxing")
    #:next
    #:go (at-top-mid 'stype)
    #:alt ((shallow-dynlimit 0))
    (shallow-dynlimit 1)
    #:go (at-top-mid 'ptype)
    #:alt ((primitive-dynlimit 0))
    (primitive-dynlimit 1)
    #:go (at-top-mid 'ctype)
    (concrete-dynlimit 0)
    )
  (pslide
    #:go heading-coord-m
    @titlerm2{Step 4. Limited Scope}
    #:next
    (yblank med-y-sep)
    (bbox
      (lc-append
        (word-append
          @rm{Focus on high-payoff } @bodyrmem{optimizations})
        @rm{rather than feature-completeness}))
    (yblank med-y-sep)
    #:next
    (defer-to 'python
              (vc-append tiny-y-sep
                         (hc-append tiny-y-sep @ucode{eval} @ucode{first-class class})
                          @ucode{multiple inheritance}))
    (yblank med-y-sep)
    (defer-to 'pyre
              (vc-append tiny-y-sep
                         (hc-append tiny-y-sep @tcode{Callable[T0, T1]} @tcode{Setof[T]})
                         @tcode{Union[T0, T1, T2]}))
    )
  (pslide
    #:go heading-coord-m @titlerm2{How is Static Python so Fast?}
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
    )
  (pslide
    #:go heading-coord-m
    (ppict-do
      @titlerm2{More Experience}
      #:go (coord 1 1/2 'lt #:abs-x med-x-sep)
      (scale (pstripe-icon) 6/10))
    #:next
    #:go (coord 1/2 32/100 'ct)
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
    #:go lesson-coord-h
    #:alt ( (prior-work-concrete (takeaway:gt 1)))
    #:alt ( #:go lesson-coord-m (takeaway:imp 0))
    #:go lesson-coord-l (takeaway:lang 0)
  )
  (pslide
    #:go heading-coord-m
    @titlerm2{The End}
    #:go lesson-coord-h
    (ppict-do
      (vl-append
        smol-y-sep
        (takeaway:gt 2)
        (takeaway:imp 2)
        (takeaway:lang 2))
      #:go (coord 1 1/2 'lc #:abs-x smol-x-sep)
      (vc-append
        tiny-y-sep
        (scale (static-python-logo #:title? #t) 7/10)
        (scale (pstripe-icon) 7/10)))
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
  ;; https://docs.python.org/3.8/library/dis.html#python-bytecode-instructions
  (define lhs
    (if (< nn 0)
      (blank)
      (label-above
        (bbox
          (lc-append
            @rm{V Tables}
            @rm{Method-based JIT}
            @rm{...}))
        @bodyrm{Cinder Runtime})))
  (define rhs
    ((if (< nn 1) bghost values)
     (label-above
       (bbox
         (table2
           #:row-sep pico-y-sep
           #:col-sep tiny-x-sep
           @coderm{CALL_FUNCTION} @rm{Python default}
           @coderm{INVOKE_METHOD} @rm{V Table lookup}
           @coderm{INVOKE_FUNCTION} @rm{direct call}))
       @bodyrm{Type-Aware Bytecode})))
  (ht-append med-x-sep lhs rhs))

(define (dyn-theory-practice n)
  (define lhs (dyn-vs-untyped "GT Theory" "=="))
  (define rhs (dyn-vs-untyped "Static Python" "!="))
  (ht-append
    med-x-sep
    ((if (< 1 n) bblur values) lhs)
    ((if (< n 1) bghost values) rhs)))

(define (dyn-vs-untyped name eq)
  (define shim (make-list 1 (bghost @coderm{xxxx})))
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
  (parameterize ((plot-x-ticks (microbench-ticks '(delta fannk nbody richa)))
                 (plot-y-ticks (exact-overhead-ticks '(0 1/2 1 3/2)))
                 (plot-font-family 'modern)
                 (plot-font-size 22))
    (plot-pict
      (list
        (python-baseline-bar)
        (for/list ((acc (in-list (list row->python row->tmin row->tmax row->tmax)))
                   (c0 (in-list (list untyped-color shallow-color concrete-color primitive-color)))
                   (c1 (in-list (list untyped-brush-color shallow-brush-color concrete-brush-color primitive-brush-color)))
                   (-jj (in-naturals)))
          (define overlap? (> -jj 2))
          (define jj (min -jj 2))
          (rectangles
            (for/list ((rr (in-list microbench-data))
                       (ii (in-naturals)))
              (define x0 (+ (* ii skip-len) jj))
              (vector (ivl x0 (+ x0 1))
                      (ivl 0  (acc rr))))
            #:color c1
            #:style (if overlap? 'bdiagonal-hatch 'solid)
            #:line-width (if overlap? 2 1)
            #:line-color c0
            #:alpha (if (< n jj) 0 0.95))))
      #:y-max (+ 1/2 y-max)
      #:title #f
      #:x-label #f
      #:y-label #f
      #:legend-anchor 'no-legend
      #:width (or ww (x%->pixels 7/10))
      #:height (or hh (h%->pixels 6/10)))))

(define (microbench-ticks name*)
  (define (my-layout ax-min ax-max)
    (for/list ((n (in-list '(3/2 11/2 19/2 27/2))))
      (pre-tick n #true)))
  (define (my-format ax-min ax-max pt*)
    (for/list ((pt (in-list pt*))
               (nn (in-list name*)))
      (format "~a" nn)))
  (ticks my-layout my-format))

(define (exact-overhead-ticks n*)
  (define (my-layout ax-min ax-max)
    (for/list ((n (in-list n*)))
      (pre-tick n #true)))
  (define (my-format ax-min ax-max pt*)
    (for/list ((pt (in-list pt*)))
      (define vv (pre-tick-value pt))
      (format "~ax" (if (integer? vv) vv (exact->inexact vv)))))
  (ticks my-layout my-format))

(define (step-summary n)
  (define fmt (if (< n 1) bodyrmlo bodyrm))
  (bbox
    (ll-append
      @rm{0. Better Compiler & Runtime}
      @rm{1. Fast Soundness Checks}
      @fmt{2. Progressive Types}
      @rm{3. Limited Dyn Type}
      @rm{4. Limited Overall Scope})))

(define (gradual-soundness n)
  (define lhs
   (bbox
    (label-above
      (table2
        #:row-sep pico-y-sep
        #:col-sep tiny-x-sep
        #:col-align cc-superimpose
        (list
          (typed-codeblock* (list (word-append @bodyrm{959 typed} @rm{ modules})))
          (blank)
          (concrete-codeblock* (list (word-append @bodyrm{ 10} @rm{ with } @bodyrm{Concrete})))
          @rm{(fast reads)})
          (primitive-codeblock* (list (word-append @bodyrm{ 16} @rm{ with } @bodyrm{Primitives})))
          @rm{(unboxed math)})
      @rm{Instagram, March 2023:}
      (yblank tiny-y-sep))))
  (define rhs
    ;; - typed = shallow
    ;; - refined = concrete + primitive
    ;; - T speedup = some benefits out of box, wow
    ;; - T slowdown = high cost of checks / low opt
    ;; - R all speedups, nice
    (bbox
      (label-above
        (microbenchmarks 2 #:h (* 2 big-y-sep) #:w (* 2.5 big-x-sep))
        @rm{Microbenchmarks}
        (word-append @rm{1x = Python, } @bodyrm{lower} @rm{ is faster})
        (yblank pico-y-sep))))
  (ht-append smol-x-sep lhs ((if (< n 1) bghost values) rhs)))

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
  (pslide
    #:go center-coord
    (titlerm2 str)))

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
  (ppict-do
    (scale (pstripe 'a 'b 'c) 4/10)
    #:go (at-cc 'a) (shallow-box)
    #:go (at-cc 'b) (concrete-box)
    #:go (at-cc 'c) (primitive-box)))

(define (shallow-box [extra #f])
  (progressivebox "Shallow" extra))

(define (concrete-box [extra #f])
  (progressivebox "Concrete" extra))

(define (primitive-box [extra #f])
  (progressivebox "Primitive" extra))

(define (progressivebox name [extra #f])
  (define pp (bodyrm name))
  (bbox (if extra (hc-append pp @rm{ ~ } (rmem extra)) pp)))

(define (shallow-dynlimit n)
  (maybe-bblur
    (< 0 n)
    (vc-append
      tiny-y-sep
      ;(untyped-codeblock*
      ;  (list
      ;    @coderm{class A:}
      ;    @coderm{ def f(self):}))
      (typed-codeblock*
        (list
          @coderm{class A:}
          @coderm{ def f(self)->int:}))
      (typed-codeblock*
        (list
          @coderm{class B(A):}
          @coderm{ def f(self):}
          @codebf{   # Type Error})))))

(define (primitive-dynlimit n)
  (maybe-bblur
    (< 0 n)
    (typed-codeblock*
      (list
        @coderm{x:int64 = 42}
        @coderm{y = x}
        @codebf{# Type Error}))))

(define (concrete-dynlimit n)
  (maybe-bblur
    (< 0 n)
    (vc-append
      pico-y-sep
      (typed-codeblock*
        (list
          @coderm{def avg(ns:chklist[dyn]):}
          @coderm{  ....}))
      (typed-codeblock*
        (list
          @coderm{avg(chklist[int](1,2))}
          @codebf{# Runtime Error})))))


(define (sp-types-enable n)
  (define sp-yes
    (word-append
      @rm{Types enable } @bodyrmem{optimizations}))
  (define sp-no
    (hc-append
      (scale (lc-append
               @rm{Types enable arbitrary migrations}
               @rm{(gradual guarantees)}) 90/100)
      @coderm{   <<   }))
  (bbox (hc-append
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

(define (lesson-for pp str n)
  (if (< n 2)
    (hc-append smol-x-sep pp (rm str))
    pp))

(define (three-answers n)
  (hc-append
    smol-x-sep
    ((if (< n 1) values bghost) (py-migration 4))
    (vl-append
      big-y-sep
      ((if (< n 1) values bghost) (add-hubs @headrm{A1.} 'a1))
      ((if (< n 1) values bghost) (add-hubs @headrm{A2.} 'a2))
      (add-hubs @headrm{A3.} 'a3))))

(define (envelope-pict ww hh #:color [color utah-litegrey] #:line-width [line-width 2])
  (define (draw dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (send dc set-brush (new brush% [style 'solid] [color color]))
    (send dc set-pen (new pen% [width line-width] [color black]))
    ;; ---
    (define path (new dc-path%))
    (send path rectangle 0 0 ww hh)
    (send dc draw-path path dx dy)
    (send dc draw-lines `((0 . 0) (,(* ww 1/2) . ,(* hh 6/10)) (,ww . 0)) dx dy)
    ;; ---
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (dc draw ww hh))

(define (hpictx n pp)
  (apply hc-append pico-x-sep (make-list n pp)))

(define (server-pict ww hh)
  (define color "dark gray")
  (define line-width 2)
  (define (draw dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (send dc set-brush (new brush% [style 'solid] [color color]))
    (send dc set-pen (new pen% [width line-width] [color black]))
    ;; ---
    (define ygap (* 2/10 hh))
    (define xgap (* 2/10 ww))
    (let ((path (new dc-path%)))
      (send path move-to 0 hh)
      (send path line-to 0 (+ 0 ygap))
      (send path line-to (+ 0 xgap) 0)
      (send path line-to ww 0)
      (send path line-to ww (- hh ygap))
      (send path line-to (- ww xgap) hh)
      (send path close)
      (send dc draw-path path dx dy))
    (send dc draw-lines `((0 . ,ygap) (,(- ww xgap) . ,ygap) (,ww . 0)) dx dy)
    (send dc draw-lines `((,(- ww xgap) . ,ygap) (,(- ww xgap) . ,hh)) dx dy)
    (let ((path (new dc-path%)))
      (for ((ym (in-list '(1/2 1))))
        (send path rounded-rectangle (* 1/2 xgap) (- hh (* ym ygap)) (* 3 xgap) (* 1/8 ygap))
        (send dc draw-path path dx dy)))
    ;; ---
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (dc draw ww hh))

(define (takeaway:gt n)
  (define concrete-rqs
    ((if (< n 1) bghost values)
      (bbox
        (parameterize ((bbox-x-margin pico-x-sep))
          (ll-append
            @rm{Qs for Concrete:}
            (hc-append @rm{  * migrating }
                       @tcode{list}
                       @rm{ to }
                       @ccode{chklist[T]}
                       @rm{ etc.})
            (yblank pico-y-sep)
            (hc-append
              @rm{  * fast tags for } @ccode{Union[T0, T1, T2]}))))))
  (define theory-perf
    (bbox
      @rm{Guarantees vs. Performance?}))
  ((if (< n 2) ht-append hc-append)
    smol-x-sep
    (lesson-for (gt-research-pict) "GT Researchers" n)
    (if (< n 2)
      (vc-append tiny-y-sep theory-perf concrete-rqs)
      (bbox @rm{New research directions}))))

(define (takeaway:imp n)
  (hc-append
    smol-x-sep
    (lesson-for (practitioner-pict) "Practitioners" n)
    (if (< n 2)
      (ppict-do
        (bbox @rm{Why not your language?})
        #:go (coord 1/2 1 'ct #:abs-y pico-y-sep)
        (scale (pstripe-icon) 6/10))
      (bbox @rm{Who's next?}))))

(define (takeaway:lang n)
  (define pp
    (bbox
      (if (< n 2)
        (ll-append
          @rm{Redex model found:}
          ;; @rm{* Redex model of Static Python}
          ;; @rm{* Converted 265 tests}
          (word-append
            @bodyrm{   5} @rm{ critical soundness bugs})
          (word-append
            @bodyrm{  16} @rm{ correctness issues}))
        (ll-append
          @rm{Model found:}
          (word-append @bodyrm{  5} @rm{ soundness + } @bodyrm{16} @rm{ other issues})))))
  ((if (< n 2) hb-append hc-append)
    smol-x-sep
    (lesson-for (designers-pict) "Language Designers" n)
    (ppict-do pp #:go (coord 1 1) (if (< n 2) (racket-pict) (blank)))))

(define (gt-research-pict)
  (define pp
    (ppict-do
      (blank 36 55)
      #:go (coord 0 0 'lt) @hugerm{τ}
      #:go (coord 1 1 'rb) @hugerm{λ}))
  (ppict-do
    (blank 100)
    #:go center-coord (scale pp 3/2)))

(define (practitioner-pict)
  (define ww 70)
  (define pp
    (ppict-do
      (blank ww ww)
      #:go (coord 1 1) (js-pict)
      #:go (coord 0 0) (ruby-pict)
      #:go (coord 1 0) (php-pict)
      #:go (coord 0 1) (clojure-pict)
      ))
  (ppict-do
    (blank 100)
    #:go center-coord (scale pp 8/10)))

(define (designers-pict)
  (define ww 130)
  (ppict-do
    (blank 100)
    #:go center-coord
    (frame (freeze (scale-to-square (bitmap "img/msn.jpeg") ww)))))

(define (prior-work-concrete pp)
  (define tp (thorn-pict))
  (define cc*
    (parameterize ((bbox-x-margin 10) (bbox-y-margin 4))
      (hc-append pico-y-sep (bbox tp) (bbox (nom-pict tp)))))
  (ppict-do
    pp
    #:go (coord 1 1 'ct #:abs-y pico-y-sep #:abs-x (- smol-x-sep))
    (ppict-do
      cc*
      #:go (coord 0 1/2 'rc #:abs-x (- pico-x-sep))
      @rm{Prior work:})))

;; -----------------------------------------------------------------------------


(define (do-show)
  ;; (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; [current-page-number-font page-font]
  ;; [current-page-number-color white]
  ;; --
  (parameterize ((current-slide-assembler bg-bg))
    (sec:title)
    (sec:what)
    (sec:how)
    (sec:lesson)
    (pslide)
    (void))
  (void))

(module+ main
  (do-show))

;; =============================================================================

;; open RQs
;; - debugging with unsoundness; or slow sound mode
;; - more sound types, unions
;; - reduce dyn limits

(module+ raco-pict (provide raco-pict)
         ;;(define client-w 984) (define client-h 728) ;; 4:3
         (define client-w 1320) (define client-h 726) ;; 16:9 sort of, too thin
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)
    #;(make-titlebg client-w client-h)



  )))
