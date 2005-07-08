
(use-modules (ice-9 optargs)
	     (ice-9 receive)
	     (ice-9 format)
	     (ice-9 pretty-print)
	     (srfi srfi-1))

(read-set! keywords 'prefix)

(defmacro unless (pred . body)
  `(if ,pred
       '()
       (begin
	 ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; dispatch framework ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (defdispatch-helper name ast args)
  (lambda (m)
    (let ((tag (car  m))
	  (fun (cadr m)))
      (if args
	  `(',tag (apply ,fun (list* ,@args (cdr ,ast))))
	  `(',tag (apply ,fun (cdr ,ast)))))))

(defmacro* defdispatch (name ast :key args . maps)
  (let ((maps (if (equal? :args (car maps))
		  (list-tail maps 2)
		  maps)))
    `(define (,name ,@(if args args '()) ,ast)
       (if (pair? ,ast)
	   (case (car ,ast)
	     ,@(map (defdispatch-helper name ast args) maps)
	     (else (throw 'unknown-node ,ast)))
	   (throw 'error "No atoms in this DSL" ,ast)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; get-sample-sizes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdispatch get-sample-sizes tree-descr
  (merge      get-sample-sizes-merge)
  (sample     get-sample-sizes-sample)
  (population get-sample-sizes-pop))

(define (get-sample-sizes-merge time . subtrees) 
  (apply append (map get-sample-sizes subtrees)))

(define (get-sample-sizes-sample size) (list size))

(define* (get-sample-sizes-pop :key name index size epochs subtree)
  (get-sample-sizes subtree))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; get-merge-times ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdispatch get-merge-times tree-descr
  (merge      get-merge-times-merge)
  (sample     get-merge-times-sample)
  (population get-merge-times-pop))

(define (get-merge-times-merge time . subtrees)
  (cons time (apply append (map get-merge-times subtrees))))

(define (get-merge-times-sample size) '())

(define* (get-merge-times-pop :key name index size epochs subtree)
  (get-merge-times subtree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; get-populations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-pop-key-value key pop)
  (let ((res (find-tail (lambda (e) 
			  (equal? e key))
			pop)))
    (if res
	(second res)
	#f)))

(define (get-pop-sizes pops) 
  (map (lambda (pop)
	 (get-pop-key-value :size pop))
       pops))

;; Moej kode!!!!!!
(define (get-pop-epochs pops) 
  (let ((tmp (map (lambda (pop)
		    (cons (get-pop-key-value :index pop)
			  (get-pop-key-value :epochs pop)))
		  pops)))
    
      (apply append 
	     (map (lambda (e)
		    (let ((index (first  e))
			  (epochs (cdr e)))
		      (map (lambda (epoch)
			     (list* (car epoch)
				    index
				    (cdr epoch)))
			   epochs)))
		  tmp))))

(define (find-pop-index name pops)
  (get-pop-key-value :index
		     (find (lambda (pop) 
			     (equal? name (get-pop-key-value :name pop)))
			   pops)))

(define (find-pop-time-frame name pops)
  (get-pop-key-value :time-frame
		     (find (lambda (pop) 
			     (equal? name (get-pop-key-value :name pop)))
			   pops)))

(defdispatch get-populations-helper tree-descr
  (merge      get-populations-helper-merge)
  (sample     get-populations-helper-sample)
  (population get-populations-helper-pop))

(define (get-populations-helper-merge time . subtrees)
  (apply append (map get-populations-helper subtrees)))

(define (get-populations-helper-sample size) '())

(define* (get-populations-helper-pop :key name index size (epochs '()) subtree)
  (cons `(population :name ,name :index ,index :size ,size :epochs ,epochs) 
	(get-populations-helper subtree)))


(define (add-time-frames pops time-frames)  
  (map (lambda (pop time-frame)
	 (if (equal? (third pop) (car time-frame))
	     (append pop (list :time-frame) (cdr time-frame))
	     (throw 'error "This should not happen 7962297043!!!")))
       pops
       time-frames))

(define (check-for-duplicate-names pops)
  (let ((names (filter (lambda (x) x)
		       (map (lambda (pop)
			      (get-pop-key-value :name pop))
			    pops))))
    (unless (equal? (length names) (length (delete-duplicates names)))
	    (throw 'syntax-error "Populations must have unique names" names))))

(define (normalise-epochs pops)
  (map (lambda (pop)
	 (apply (lambda* (tag :key name index size epochs time-frame)
			 (let ((new-epochs (map (lambda (epoch)
						  (if (equal? 'beta (car epoch))
						      `(growth ,(second epoch) 
							     ,(first time-frame) ,(second time-frame))
						      epoch))
						epochs)))
			   `(population
			     :name ,name
			     :index ,index
			     :size ,size
			     :epochs ,(list* `(bottleneck ,size ,(first time-frame) ,(second time-frame))
					     (if new-epochs
						 new-epochs
						 '()))
			     :time-frame ,time-frame)))
		pop))
       pops))

(define (get-populations time tree-descr)
  (reset-index)
  (let* ((pops-no-time           (get-populations-helper tree-descr))
	 (time-frames            (population-time-frames time tree-descr))
	 (pops-no-default-epochs (add-time-frames pops-no-time time-frames)))
    (check-for-duplicate-names pops-no-time)
    (normalise-epochs pops-no-default-epochs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; add-indexes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *index* 0)

(define (reset-index) (set! *index* 0) *index*)

(define (inc-index)
  (let ((i *index*))
    (set! *index* (+ *index* 1))
    i))

(defdispatch add-indexes-helper tree-descr
  (merge      add-indexes-helper-merge)
  (sample     add-indexes-helper-sample)
  (population add-indexes-helper-pop))

(define (add-indexes-helper-merge time . subtrees)
  (let* ((res   (map add-indexes-helper subtrees))
	 (index (caar res))
	 (trees (map (lambda (i/tree) (cdr i/tree)) res)))
    (cons index `(merge ,time ,@trees))))

(define (add-indexes-helper-sample size)
  (cons (inc-index) `(sample ,size)))

(define* (add-indexes-helper-pop :key name size (epochs '()) subtree)
  (let ((i/tree (add-indexes-helper subtree)))
  (cons (car i/tree)
	`(population :name ,name 
		     :index ,(car i/tree)
		     :size ,size
		     :epochs ,epochs
		     :subtree ,(cdr i/tree)))))

(define (add-indexes tree-descr)
  (reset-index)
  (cdr (add-indexes-helper tree-descr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; collect-merges ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdispatch collect-merges tree-descr
  (merge      collect-merges-merge)
  (sample     collect-merges-sample)
  (population collect-merges-pop))

(define (collect-merges-merge time . subtrees)
  (cons (list* 'population-merge 
	       time
	       (map (lambda (pop)
		      (get-population-index pop))
		    subtrees))
	(apply append (map collect-merges subtrees))))

(define (collect-merges-sample size) '())

(define* (collect-merges-pop :key name index size epochs subtree)
  (collect-merges subtree))

(define (get-population-index pop)
  (apply (lambda* (tag :key index :allow-other-keys) index) pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; check-merge-times ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks if nested merges nodes are descending in time

(defdispatch check-merge-times tree-descr :args (current-time)
  (merge      check-merge-times-merge)
  (sample     check-merge-times-sample)
  (population check-merge-times-pop))

(define (check-merge-times-merge current-time time . subtrees)
  (if (or (= -1 current-time) ;;infinity
	  (> current-time time))
      (every (lambda (x) x)
	     (map (lambda (subtree)
		    (check-merge-times time subtree))
		  subtrees))
      #f))

(define (check-merge-times-sample current-time size) #t)

(define* (check-merge-times-pop current-time :key name index size epochs subtree)
  (check-merge-times current-time subtree))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; no-merge-merge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks that a merge node cannot be a direct child of another merge node

(defdispatch no-merge-merge-helper tree-descr :args (mergep)
  (merge      no-merge-merge-merge)
  (sample     no-merge-merge-sample)
  (population no-merge-merge-pop))

(define (no-merge-merge-merge mergep time . subtrees)
  (if mergep
      #f
      (every (lambda (x) x)
	     (map (lambda (subtree)
		    (no-merge-merge-helper #t subtree))
		  subtrees))))

(define (no-merge-merge-sample mergep size) #t)

(define* (no-merge-merge-pop mergep :key name index size epochs subtree)
  (no-merge-merge-helper #f subtree))

(define (no-merge-merge tree-descr)
  (no-merge-merge-helper #f tree-descr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; population-time-frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks that population sizes are consistent

(defdispatch population-time-frames tree :args (start-time)
  (merge      population-time-frames-merge)
  (sample     population-time-frames-sample)
  (population population-time-frames-pop))

(define (population-time-frames-merge start-time time . subtrees)
  (cons time (apply append (map (lambda (subtree) (population-time-frames time subtree)) subtrees))))

(define (population-time-frames-sample start-time size)
  (list 0))

(define* (population-time-frames-pop start-time :key name index size epochs subtree)
  (let ((res (population-time-frames #f subtree)))
    (cons (list name (list (car res) start-time)) (cdr res))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; systax-check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *list* '())
(define (init-list-iterator lst) (set! *list* lst))
(define (peek) (car *list*))
(define (pop) 
  (let ((e (peek))) 
    (if (null? *list*)
	(throw 'error "Trying to pop the empty list")
	(set! *list* (cdr *list*))) 
    e))

(define* (optional-arg test :key default)
  (if (test (peek))
      (pop)
      default))

(define* (mandatory-arg test :key error)
  (if (test (peek))
      (pop)
      (apply throw error)))

(define* (optional-keyword-arg key :key default)
  (if (equal? key (peek))
      (begin (pop) (pop))
      default))

(defdispatch syntax-check-and-normalization tree-descr
  (merge      syntax-check-and-normalization-merge)
  (sample     syntax-check-and-normalization-sample)
  (population syntax-check-and-normalization-pop))

(define* (syntax-check-and-normalization-merge . in)
  (unless (>= (length in) 2)
	  (throw 'syntax-error "Merge takes 3 or more arguments" `(merge ,@in)))
  (let ((time     (first in))
	(subtrees (cdr   in)))
    (unless (number? time)
	    (throw 'systax-error "In merge: time has to be a number" time))
    (unless (every pair? subtrees)
	    (throw 'systax-error "In merge: all subtrees are not nodes" `(merge ,@in)))
    `(merge ,time ,@(map syntax-check-and-normalization subtrees))))

(define* (syntax-check-and-normalization-sample . in)
  (unless (= (length in) 1)
	  (throw 'syntax-error "Sample takes one argument" `(sample ,@in)))
  (let ((size (first  in)))
    (unless (number? size)
	    (throw 'systax-error "In sample: size has to be a number" size))
    `(sample ,size)))

(define (make-local-epochs epochs beta)
  (if (and (pair? epochs)
	   (not (pair? (car epochs))))
      (list* epochs (if beta
			`((beta .beta))
			'()))
      (if beta
	  (list* `(beta ,beta) epochs)
	  epochs)))

(define* (syntax-check-and-normalization-pop . in)
  (init-list-iterator in)
  (let ((name (optional-arg symbol? :default #f))
	(size (mandatory-arg number?
			     :error `(syntax-error
				      "In population: size must be a number" ,`(population ,@in))))
	(beta   (optional-keyword-arg :beta :default #f))
	(epochs (optional-keyword-arg :epochs :default '()))
	(subtree (mandatory-arg pair?
				:error `(syntax-error
					 "In population: error in subtree definition"
					 ,`(population ,@in)))))
    `(population :name ,name 
		 :size ,size 
		 :epochs ,(make-local-epochs epochs beta)
		 :subtree ,(syntax-check-and-normalization subtree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; The compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-migrations migrations pops)
  (map (lambda (migration)
	 (list* 'migration 
		(find-pop-index (second migration) pops)
		(find-pop-index (third migration) pops)
		(list-tail migration 3)))
       migrations))

(define (compile tree migrations)
  (let ((tree2 (add-indexes (syntax-check-and-normalization tree))))
    
    (unless (check-merge-times -1 tree2)
	    (throw 'ast-check "Merge-times not descending"))
    (unless (no-merge-merge tree2)
	    (throw 'ast-check "A merge-node cannot be followed by a merge-node"))

    (let* ((pops         (get-populations -1 tree2))
	   (sample-sizes (get-sample-sizes tree2))
	   (pop-sizes    (get-pop-sizes    pops))
	   (merges       (collect-merges   tree2)))

      `(:sample-sizes ,sample-sizes
		      :epochs ,(append merges
				       (get-pop-epochs pops)
				       (add-migrations migrations pops))))))




(define (simulate ms program . args)
   (let-keywords args #f ((rho   0)
                          (gamma 0)
                          (Q     0)
                          (beta  0)
			  (migration '())
                          (coalescence-callback '())
                          (recombination-callback '())
                          (geneconversion-callback '())
                          (keep-empty-intervals #f)
                          (random-seed 0))
     (if (number? program)
         ;; simple simulation run...
         (c-simulate ms (list program)
                      (list rho gamma Q beta)
                      (list coalescence-callback
                            recombination-callback
                            geneconversion-callback)
                      '() ;; no epochs
                      keep-empty-intervals
                      random-seed)
	 ;; otherwise, compile the program to get epochs and samples
	 (let ((pop-structure (compile program migration)))
	   (let-keywords pop-structure #f ((sample-sizes '()) (epochs '()))
			 (let ((real-epochs (map (lambda (e) (primitive-eval e)) epochs)))
			   (c-simulate ms sample-sizes
				       (list rho gamma Q beta)
				       (list coalescence-callback
					     recombination-callback
					     geneconversion-callback)
				       real-epochs
				       keep-empty-intervals
				       random-seed)))))))
