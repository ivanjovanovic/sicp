; Exercise 2.72.
;
; Consider the encoding procedure that you designed in exercise
; 2.68. What is the order of growth in the number of steps needed to encode a
; symbol? Be sure to include the number of steps needed to search the symbol
; list at each node encountered. To answer this question in general is
; difficult. Consider the special case where the relative frequencies of the n
; symbols are as described in exercise 2.71, and give the order of growth (as a
; function of n) of the number of steps needed to encode the most frequent and
; least frequent symbols in the alphabet.
; ------------------------------------------------------------

; Considering in the case of previous that we need O(N) steps to find the lowest element
; and we need O(n) to search through the list of simbols. We come to the 
; order of growth O(n^2)

