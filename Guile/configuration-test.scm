
(define region
  (list (snp-marker   0.0 0.10 0.90)
	(snp-marker   0.1 0.10 0.90)
	(snp-marker   0.2 0.10 0.90)
	(trait-marker 0.3 0.18 0.22)
	(snp-marker   0.4 0.10 0.90)
	(snp-marker   0.5 0.10 0.90)))

(define no-leaves 100)
(define rho 50)
(define Q 0)
(define G 0)
(define growth 0)

(define parameters
  (parameters no-leaves region rho Q G growth))


	
