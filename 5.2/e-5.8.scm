; Exercise 5.8.  The following register-machine code is ambiguous,
; because the label here is defined more than once:

start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there

; With the simulator as written, what will the contents of register a be
; when control reaches there? Modify the extract-labels procedure so
; that the assembler will signal an error if the same label name is used
; to indicate two different locations.
; ------------------------------------------------------------ 

; The way it is implemented, since we are consing the label list, last
; one will be always found first, so value of a will end up as being 4

; Adding a check if label is already in the list of pairs is easy.
; We just have to use (assoc) procedure for extracting the potential
; label and render error in that case.

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (if (assoc next-inst labels) ; adding a check if label already exists in the list
              (error "Multiple usage of the same label -- EXTRACT_LABELS" next-inst)
              (receive insts
                     (cons (make-label-entry next-inst insts)
                           labels)))
            (receive (cons (make-instruction next-inst)
                           insts)
                     labels)))))))
