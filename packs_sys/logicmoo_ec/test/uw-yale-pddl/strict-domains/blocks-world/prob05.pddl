(define (problem mcd-tower-invert)
    (:domain mcd-blocksworld)
  (:objects A B C D E)
  (:init (block A) (block B) (block C) (block D) (block E) (block Table)
	 (clear a) (on a b) (on b c) (on c d) (on d e)(on e table)
	 (clear table))
  (:goal (and (on b c) (on c d) (on d e) (on e a))))