#lang racket
(require "another-a-mini-kanren/another-a-mini-kanren.rkt")

(define count-t-in-c-neq-others
  (lambda (l)
    (letrec ((t-counter
              (lambda (l acc h)
                (fresh (a b)
                       (any
                        (all (nullo l)
                             (== acc h))
                        (all (caro l a)
                             (cdro l b)
                             (eqo a 't)
                             (t-counter b (+ acc 1) h))
                        (all (caro l a)
                             (cdro l b)
                             (neg (eqo a 't))
                             (t-counter b acc h)))))))
      (fresh (a b c d e f g h i j k)
             (all
              (caro l a) ;1st list
              (cdro l b)
              (caro b c) ;2nd list
              (cdro b d)
              (caro d e) ;3rd list
              (cdro a f) ;Remove 'a
              (cdro c g) ;Remove 'b
              (cdro e h) ;Remove 'c
              (t-counter f 0 i)
              (t-counter g 0 j)
              (t-counter h 0 k)
              (neg (eqo k j))
              (neg (eqo k i))
              )))))

;    (student math english history)
;ex. ('a 't 't 't) or ('a 'f 'f 'f)
(fresh (i)
       (run* (i)
             (all-uniqueo
              (fresh (h)
                     (run* (h) (all (== h (list
                                           (list 'a (_) (_) 'f)
                                           (list 'b (_) (_) (_))
                                           (list 'c (_) (_) (_))
                                           ))
            
                                    ;Ma => Mb
                                    (any (all (memo (list 'a 't (_) (_)) h)
                                              (memo (list 'b 't (_) (_)) h))
                                         (all (memo (list 'a 'f (_) (_)) h)
                                              (memo (list 'b 't (_) (_)) h))
                                         (all (memo (list 'a 'f (_) (_)) h)
                                              (memo (list 'b 'f (_) (_)) h)))

                                    ;Ea <=> Ec
                                    (any (all (memo (list 'a (_) 't (_)) h)
                                              (memo (list 'c (_) 't (_)) h))
                                         (all (memo (list 'a '(_) 'f (_)) h)
                                              (memo (list 'c (_) 'f (_)) h)))

                                    ;Mb => Ma
                                    (any (all (memo (list 'b 't (_) (_)) h)
                                              (memo (list 'a 't (_) (_)) h))
                                         (all (memo (list 'b 'f (_) (_)) h)
                                              (memo (list 'a 't (_) (_)) h))
                                         (all (memo (list 'b 'f (_) (_)) h)
                                              (memo (list 'a 'f (_) (_)) h)))

                                    ;Ha V ~Hc
                                    (any (memo (list 'a (_) (_) 't) h)
                                         (memo (list 'c (_) (_) 'f) h))

                                    ;~Eb => ~Ea
                                    (any (all (memo (list 'b (_) 'f (_)) h)
                                              (memo (list 'a (_) 'f (_)) h))
                                         (all (memo (list 'b (_) 't (_)) h)
                                              (memo (list 'a (_) 't (_)) h))
                                         (all (memo (list 'b (_) 't (_)) h)
                                              (memo (list 'a (_) 'f (_)) h)))

                                    ;Ma V Ea V Ha
                                    (any (memo (list 'a 't (_) (_)) h)
                                         (memo (list 'a (_) 't (_)) h)
                                         (memo (list 'a '(_) (_) 't) h))

                                    ;Mb V Eb V Hb
                                    (any (memo (list 'b 't (_) (_)) h)
                                         (memo (list 'b (_) 't (_)) h)
                                         (memo (list 'b '(_) (_) 't) h))

                                    ;Mc V Ec V Hc
                                    (any (memo (list 'c 't (_) (_)) h)
                                         (memo (list 'c (_) 't (_)) h)
                                         (memo (list 'c '(_) (_) 't) h))

                                    ;Ma V Mb V Mc
                                    (any (memo (list 'a 't (_) (_)) h)
                                         (memo (list 'b 't (_) (_)) h)
                                         (memo (list 'c 't (_) (_)) h))

                                    ;Ea V Eb V Ec
                                    (any (memo (list 'a (_) 't (_)) h)
                                         (memo (list 'b (_) 't (_)) h)
                                         (memo (list 'c (_) 't (_)) h))

                                    ;Ha V Hb V Hc
                                    (any (memo (list 'a (_) (_) 't) h)
                                         (memo (list 'b (_) (_) 't) h)
                                         (memo (list 'c (_) (_) 't) h))

                                    ;Mc V ~Mc
                                    (any (memo (list 'c 't (_) (_)) h)
                                         (memo (list 'c 'f (_) (_)) h))

                                    (count-t-in-c-neq-others h)
                                    
                                    ))) i)))