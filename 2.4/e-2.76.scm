; Exercise 2.76.
;
; As a large system with generic operations evolves, new
; types of data objects or new operations may be needed. For each of the
; three strategies -- generic operations with explicit dispatch,
; data-directed style, and message-passing-style -- describe the changes
; that must be made to a system in order to add new types or new
; operations. Which organization would be most appropriate for a system
; in which new types must often be added? Which would be most
; appropriate for a system in which new operations must often be added?
; ------------------------------------------------------------

; 1) Explicit dispatch on type
;
; a) Adding new data type
;  
; - Implementer of new data type must tag every type of data
; - Implementer must provide all the specific methods for his data type
; - Consumer has to update every generic procedure on higher level for new 
; type
; - Consumer has to implement predicates for every new data type

; b) Adding new operation on the consumer side as generic procedure
;
; - All implementer have to implement new operation.
;
; 2) Data directed through a table
;
; a) Adding new type
;
; - Implementer has to add entries to a global dispatch table
;
; b) Adding new operation
;
; - Implementer has to add entry to a global dispatch table
;
; 3) Message passing
;
; a) Adding new type
;
; - Nothing has to be done, type itself contains all the generic
; operations that are possible to be done on it.
; 
; b) Adding new operation
;
; - All the types must implement inside themselves new operations into
; the existing types.

; If types are to be added often, message passing style is prefered,
; since you don't have to manage anything except implementing the type
; itself.
; 
; For adding the operations often, it would be somehow easier with
; message passing as well since you have to implement operation anyway,
; but comparing to data directed style, you don't have to register it
; anywhere.
