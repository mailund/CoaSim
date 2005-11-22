
(load "simulate.scm")
(use-modules (ice-9 receive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Small test framework ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro unless (pred . body)
  `(if ,pred
       '()
       (begin
	 ,@body)))

(define (run-test test)
  (catch #t
	 (lambda ()
	   (values (apply test '()) #t))
	 (lambda (key . args)
	   (values (list* key args) #f))))

(define (deftest-fun name test test-printed-ver expected-res)
  (receive (res good-test-run-p) (run-test test)
	   (unless (and good-test-run-p 
			(equal? res expected-res))
		   (force-output)
		   (format #t "===== Test FAILED: ~a =====~%" name)
		   (format #t "Expected:~%~y~%Got:~%~a~%"
			   expected-res res))))

(defmacro deftest (name test expected-res)
  `(deftest-fun ',name (lambda () ,test) ',test ,expected-res))


;;(deftest Test-success 42 42)
;;(deftest Test-fail 42 43)
;;(deftest Test-fail-exeption (throw 'error "hehe") 42)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Test data and tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest compile1
  (compile 
   '(population p1 1 :beta 77 :epochs ((bottleneck 111 1 2)) (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1) (growth 0 77 0 -1) (bottleneck 0 111 1 2))))

(deftest compile2
  (compile 
   '(population p1 5 (merge 11 
			    (population p2 2 :epochs (bottleneck 7 0 5) (sample 2))
			    (population p3 3 (sample 3))))
   '((migration p2 p3 0.2 3 5) (migration p2 p3 0.7)))
  '(:sample-sizes (2 3)
		  :epochs ((population-merge 11 0 1)
			   (bottleneck 0 5 11 -1)
			   (bottleneck 0 2 0 11)
			   (bottleneck 0 7 0 5)
			   (bottleneck 1 3 0 11)
			   (migration 0 1 0.2 3 5)
			   (migration 0 1 0.7 0 11))))

(deftest compile3
  (compile 
   '(population p1 7 
		 (merge 22
			(population p2 2 (sample 2))
			(population p3 5 
				    (merge 11
					   (population p4 2 (sample 2))
					   (population p5 3 (sample 3))))))
   '())
  '(:sample-sizes (2 2 3)
		  :epochs ((population-merge 22 0 1)
			   (population-merge 11 1 2)
			   (bottleneck 0 7 22 -1)
			   (bottleneck 0 2 0 22)
			   (bottleneck 1 5 11 22)
			   (bottleneck 1 2 0 11) 
			   (bottleneck 2 3 0 11))))

(deftest compile4
  (compile 
   '(population 1 (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1))))

(deftest compile5
  (compile 
   '(population 1 :epochs () (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1))))

(deftest compile6
  (compile 
   '(population 1 (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1))))

(deftest compile7
  (compile 
   '(population 1 :beta 55 (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1) (growth 0 55 0 -1))))


(deftest implicit-end
  (compile
   '(population 1 :epochs (bottleneck 2 0.5) (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1) (bottleneck 0 2 0.5 -1))))

(deftest implicit-end-2
  (compile
   '(population 1 (merge 1
			 (population 2 :epochs (growth 2 0.5) (sample 1))
		         (population 4 (sample 1))))
   '())
  '(:sample-sizes (1 1)
    :epochs ((population-merge 1 0 1)
	     (bottleneck 0 1 1 -1)
	     (bottleneck 0 2 0  1)
	     (growth 0 2 0.5 1)
	     (bottleneck 1 4 0 1))))


(deftest implicit-end-3
  (compile
   '(population 1 :epochs (growth 2 0.5) (sample 1))
   '())
  '(:sample-sizes (1) :epochs ((bottleneck 0 1 0 -1) (growth 0 2 0.5 -1))))


(deftest double-bottleneck
  (compile
   '(population 1 :epochs ((bottleneck 2 0.004 0.04) (bottleneck .5 0.04)) (sample 10))
   '())
  '(:sample-sizes (10) :epochs ((bottleneck 0 1   0     -1)
				(bottleneck 0 2   0.004  0.04)
				(bottleneck 0 0.5 0.04  -1))))
  

;;;;(deftest compile4
;;;;  (compile 
;;;;   '(population p1 7 (merge 11
;;;;			    (population p2 2 (sample 2))
;;;;			    (population p3 5 (merge 2
;;;;						    (population p4 2 (sample 2))
;;;;						    (population p5 3 (sample 3)))))))
;;;;  '())
;;;;
;;;;(deftest compile5
;;;;  (compile 
;;;;   '(population p1 7 (merge 11
;;;;			    (population p2 2 (sample 2))
;;;;			    (merge 2
;;;;				   (population p4 2 (sample 2))
;;;;				   (population p5 3 (sample 3))))))
;;;;  '())
;;;;
;;;;(deftest compile6
;;;;  (compile 
;;;;   '(population p1 8 (merge 11
;;;;			    (population p2 2 (sample 2)))))
;;;;  '())
;;;;


(define *epochs-0*
  '(population :name p1 :size 2 :epochs ((bottleneck 2 0 2)) :subtree (sample 2)))

(define *epochs-1* 
  '(population 
    :name p0
    :size 4 
    :subtree (merge 2
		    (population :name p1 :size 2 :subtree (sample 2))
		    (population :name p2 :size 2 :subtree (sample 2)))))

(define *epochs-2*
  '(population :name p1
	       :size 6
	       :subtree (merge 1.5
			       (population :name p2 :size 2 :subtree (sample 2))
			       (population :name p3 :size 4
					   :subtree (merge 1
							   (population :name p4 :size 2
								       :subtree (sample 2))
							   (population :name p5 :size 2
								       :subtree (sample 2)))))))


(deftest population-time-frames-test1
  (population-time-frames 7 *epochs-0*)
  '((p1 (0 7))))

(deftest population-time-frames-test2
  (population-time-frames 7 *epochs-1*)
  '((p0 (2 7)) (p1 (0 2)) (p2 (0 2))))

(deftest population-time-frames-test3
  (population-time-frames 7 *epochs-2*)
  '((p1 (1.5 7)) (p2 (0 1.5)) (p3 (1 1.5)) (p4 (0 1)) (p5 (0 1))))




(deftest get-pop-sizes-test
  (get-pop-sizes (get-populations -1 *epochs-2*))
  '(6 2 4 2 2))

(deftest find-pop-index-test
  (find-pop-index 'p4 (get-populations -1 (add-indexes *epochs-2*)))
  1)


(deftest get-merge-times0
  (get-merge-times *epochs-0*)
  '())

(deftest get-merge-times1 
  (get-merge-times *epochs-1*)
  '(2))

(deftest get-merge-times2
  (get-merge-times *epochs-2*)
  '(1.5 1))



(deftest get-populations1
  (get-populations -1 *epochs-0*)
  '((population :name p1 :index #f :size 2 :epochs ((bottleneck 2 0 -1) (bottleneck 2 0 2))
		:time-frame (0 -1))))

(deftest get-populations2
  (get-populations -1 *epochs-1*)
  '((population :name p0 :index #f :size 4 :epochs ((bottleneck 4 2 -1)) :time-frame (2 -1))
    (population :name p1 :index #f :size 2 :epochs ((bottleneck 2 0 2)) :time-frame (0 2))
    (population :name p2 :index #f :size 2 :epochs ((bottleneck 2 0 2)) :time-frame (0 2))))

(deftest get-populations3
  (get-populations -1 *epochs-2*)
  '((population :name p1 :index #f :size 6 :epochs ((bottleneck 6 1.5 -1)) :time-frame (1.5 -1))
    (population :name p2 :index #f :size 2 :epochs ((bottleneck 2 0 1.5)) :time-frame (0 1.5))
    (population :name p3 :index #f :size 4 :epochs ((bottleneck 4 1 1.5)) :time-frame (1 1.5))
    (population :name p4 :index #f :size 2 :epochs ((bottleneck 2 0 1)) :time-frame (0 1))
    (population :name p5 :index #f :size 2 :epochs ((bottleneck 2 0 1)) :time-frame (0 1))))




(deftest collect-merges0
  (collect-merges (add-indexes *epochs-0*))
  '())

(deftest collect-merges1
  (collect-merges (add-indexes *epochs-1*))
  '((population-merge 2 0 1)))

(deftest collect-merges2
  (collect-merges (add-indexes *epochs-2*))
  '((population-merge 1.5 0 1) (population-merge 1 1 2)))



(deftest check-merge-times0
  (check-merge-times -1 *epochs-0* )
  #t)

(deftest check-merge-times1
  (check-merge-times -1 *epochs-1*)
  #t)

(deftest check-merge-times2
  (check-merge-times -1 *epochs-2* )
  #t)

(deftest check-merge-times-error1
  (check-merge-times -1
   '(population :name p1 :size 1 
		:subtree (merge 2
				(population :name p2 :size 2 :subtree (sample 2))
				(population :name p3 :size 4
					    :subtree (merge 2
							    (population :name p4 :size 2
									:subtree(sample 2))
							    (population :name p5 :size 2
									:subtree (sample 2)))))))
  #f)

(deftest check-merge-times-error2
  (check-merge-times -1
   '(population :name p1 :size 2
		:subtree (merge 1
				(population :name p2 :size 2 :subtree (sample 2))
				(population :name p3 :size 4 
					    :subtree (merge 2
							    (population :name p4 :size 2 
									:subtree (sample 2))
							    (population :name p5 :size 2
									:subtree (sample 2)))))))
  #f)



(deftest no-merge-merge0
  (no-merge-merge *epochs-0*)
  #t)

(deftest no-merge-merge1
  (no-merge-merge *epochs-1*)
  #t)

(deftest no-merge-merge2
  (no-merge-merge *epochs-2*)
  #t)

(deftest no-merge-merge-error1
  (no-merge-merge
   '(population :name p1 :size 1
		:subtree(merge 2
			       (population :name p2 :size 2 :subtree (sample 2))
			       (merge 2
				      (population :name p4 :size 2 :subtree (sample 2))
				      (population :name p5 :size 2 :subtree (sample 2))))))
  #f)


