; Exercise 4.14.
;
; Eva Lu Ator and Louis Reasoner are each experimenting with
; the metacircular evaluator. Eva types in the definition of map, and runs some
; test programs that use it. They work fine. Louis, in contrast, has installed
; the system version of map as a primitive for the metacircular evaluator. When
; he tries it, things go terribly wrong. Explain why Louis's map fails even
; though Eva's works.
; ------------------------------------------------------------

; The difference between the two approaches is that when map procedure
; is defined by definition in REPL it is built as compound procedure and
; application for compound procedure is called.
;
; In the case when map is defined as primitive and executed as apply-primitive,
; passed function will not be processed and passed to map as implementation of
; the procedure but will be passed as procedure object which our underlying
; implementation an't use as parameter in the system defined map procedure.
