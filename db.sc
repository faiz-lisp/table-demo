
;init

(load "g:/my/libs/chez/lib.sc") ;;ver 1.83
;case os: c:/; or /c/;

(define (version) "V1.06d")

#|
## desc ;;
  - **This is a table demo on Windows with my [Chez-Lib](https://github.com/faiz-lisp/libs.git)**.
  - Current subject: key and word
  - types
    - '([key value] )
    - '([(key) value flag])
    - '([(id) desc flag]  )
    - '([id task status](0 "nil" ok) )
    - '( (macos "10.15.5", id app "1.01", base "1.01") )
## todo
  - add shortcut key in repl: func 1 2 3 -> (func 1 2 3)
  - '[]
## totry
  - hash
## tolearn
  - 1 element data in list costs how much space? 32bit?
## APIs
  - load push remov set-nth! exist~=mem? [get-by 'id]
  - [load "what"] [add xs ...], [del 123 ...]
    - [fix '(...) 'id] [get 123 'id/short/name] [sort-db < 'id] exist~=get 
  - get add reload, sort fix del exist~=get def/defa~=case-lam
  - (add *db* '(123 XS "Xc Sdf") ...)
## func-name
  - db-xxx -> tab-xxx
|#

;pre
;(import (chezscheme))


;alias
(ali reset-rand reset-randseed)
(ali init init-db)
(ali add add->db)
(ali save-db save-tab->file)
(ali save save-db)
(ali sort-db sort-db!)
(ali adds add-and-save->db)
(ali search search-all)
(ali fix fix-db)
(ali quit/save save-and-quit)
(ali q/save save-and-quit)
(ali q q/save) ;todo: ask for confirm
(ali quit-w/o-save exit)
(ali quit-bef-save exit)
(ali q/!save exit)

;db-xxx
(ali db-load      load-db)
(ali db-init      init-db)
(ali db-add       add->db)
(ali db-sort      sort-db!)
(ali db-save      save-db)
(ali db-add&save  add-and-save->db)
(ali db-search    search)
(ali db-fix       fix)
(ali db-chk       chk-db)

;

(reset-rand)

;;

(setq *types* '(key value)) ;init?
(setq *defa-type-id* 1) ;1-based, the key is
(setq *defa-tab-file* "db.dat")
(setq *db* nil  *db0* `[,*types* (ret "return")])
; [id short desc date-time]

;

(def init-db
  (case-lam
    [(xs)   (setq *db* xs)]
    [()     (setq *db* *db0*)] ;xs=*db0*
    ;(setq *db* '([#x123 asd "Azx Sxc Do"]))
) )

;;

(defn/values load-db (file) [*defa-tab-file*] ;tab1 tab2 ...
  (if (file-exists? file)
    (load file) ;
    (init-db)
) )

#|
TODO:
- namespace tables?
- exist push remov nth! 
- (exist 'sn x xs)(push xs Xs nth)(remov 'sn x xs)(nth! xs nth x)(nth xs nth)
|#

;

;(init-db)
(load-db) ;

;x
;(setq a '[(sn id bat)(0A1 10A PO15)(0A2 11A MO25)]) ;...(sn id bat)
;(setq *db* '[(F M ID1234)(E M ID12345)])

(def x->nth
  (case-lam
    [(xs x eql) ;
      (def (_ xs n)
        (if (nilp xs) nil
          (if (eql (car xs) x) n
            [_ (cdr xs) (1+ n)]
      ) ) )
      (_ xs 1)]
    [(xs x) (x->nth xs x eql)]
) )

(def/values (eq-nocase s1 s2 fcase eql) [str-downcase equal] ;def/va?
  (eql (fcase s1) (fcase s2))
)
; (def (eql-nocase x1 x2 fcase  eql)
  ; (if~
    ; [str? x1]
    ; (eq-nocase x1 x2)
    ; [char? x1]
    ; (eq-nocase x1 x2 char-downcase eq)
    ; [sym? x1]
    ; (eql)
; ) )

;todo: any->list

;(sublist xs s e)
;(sublist xs s n p)

;(remov-elems '(#\space #\-) (str->list "a- d"))

(def search-all ;all? downcase? trim?
  (case-lam
    ([x flg xz]
      (letn (
          [ops (lam(x) (remov #\-[remov #\space (str->list[string-downcase(str x)])]))] ;;
          [y (ops x)] ;
          [i (if [num? flg] flg (nth-of flg *types*))] )
        (def (_ xz)
          (if (nilp xz) nil
            (let ([a (car xz)] [d (cdr xz)])
              (if [list-include (ops(nth a i)) y] ;downcase-and-trim
                (cons a [_ d])
                [_ d]
        ) ) ) )
        (_ xz)
    ) )
    ([x i] (search-all x i *db*))
    ([x]  
      (case (ty x) ;;
        ["string" (search-all x 2)]
        ["symbol" (search-all x 1)]
        [else nil]
    ) )
) )

(def get
  (case-lam
    [(db x flg) ;*db* ;'id/short/name ;1/2/3 ;case-lam?
      (let
        ( [n (if [num? flg] flg (x->nth *types* flg))]
          [eql-nocase (lam(x y) [redu~ equal (map [lam(x)(str-downcase(str x))] [li x y])])] ) ;@
        (def (_ db eql)
          (if (nilp db) nil ;
            (let ([a (car db)])
              (if [eql (nth a n) x] ;if str? str-downcase
                a
                [_ (cdr db) eql]
        ) ) ) )
        (case n
          [1    (_ db eq)]
          [2    (_ db eql-nocase)] ;<-eq
          [3    (_ db eql-nocase)] ;<-eql
          [else [get db x 1]]
    ) ) ]
    [(x flg) (get *db* x flg)]
    [(x)
      (get x
        (case (ty x) ;;
          ("string" 2)
          ("symbol" 1)
          (else 3)
    ) ) ] ;
) )
;(get *db* 123 1)
;(get 123)

(dont-return!) ;

(def add->db ;
  (case-lam
    ([x]
      (letn ([pid (car x)] [resl (get pid)])
        (if (ok? resl) nil ;
          (rpush x *db*) 
    ) ) )
    (xs
      (let ([resl (map add->db xs)]) ;map is disordered
        (save) ;sort?
        resl
) ) ) )

(def (add-and-save->db . xs)
  ;(redu add->db xs)
  (let ([resl (map add->db xs)]) ;
    (save)
    resl
) )

(def/va (chk-db [db *db*]) ;need to be synt
  (echol `(len  *db*) '= (len db))
  (echol `(last *db*) '= (last db)) ;echo ng
)

(def/va (db-show [db *db*])
  db
)

;(fix [xs] '(short name) '(17 poi "POIjk") [*defa-type-id*]) ;whole-new/Fal/nil?
;(fix '(17 poi "POIjkl"))
(def fix-db
  (case-lam
    ([xz flgs xs id]
      (letn ([i (if [num? id] id (nth-of id *types*))] [myid (nth xs i)])
        (def (_ xz ret)
          (if [nilp xz] Fal
            (letn ([a (car xz)][curr-id (nth a id)][d (cdr xz)])
              (if [eq curr-id myid]
                (bgn (echol 'OK) ;
                  [setq *db* (append (rev ret) (cons xs d))] ) ;flgs
                [_ d (cons a ret)]
        ) ) ) )
        (_ xz nil)
    ) )
    ([xz flgs xs] (fix-db xz flgs xs 1))
    ([flgs xs] (fix-db *db* flgs xs))
    ([xs] (fix-db nil xs)) ;done
) )

(defn/values sort-db! (get g xs) [car stru< *db*]
  (setq *db*
    (cons [car xs] ;
      (qsort [cdr xs] ;
        (lam (x y)
          (g (get x) (get y))
) ) ) ) )
;(def/va (asd [a 1] [b 2] [c 3]) (li a b c)) ;(asd)

; demo, demo-for-add -> '(pid short name)

;check: exist-sn? sn db, pid?, bat?, sn-id? sn-bat? id-bat?, exist-sn-id-bat? . . . db
;handle&
;show

;how ab backup?
;save&
(defn/values save-tab->file (file) [*defa-tab-file*] ;?

  (sys [str `("@del/s/q " ,file ".bak 1>nul 2>nul")]) ;how ab linefeed for multi-line
  (sys [str `("@ren " ,file " " ,file ".bak 2>nul")])
  
  (save-file `(set! *db* ',*db*) file) ;  
  'OK
)

(def (save-and-quit) ;when side-effect do save action, how is it?
  (save-db)
  (exit)
)
