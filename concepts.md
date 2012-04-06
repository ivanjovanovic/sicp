## data asbtractions

## procedural abstractions

## conventional interfaces


## Making a serializer.
Serializer is like the factory to make more concrete type of the
procedure which is passed.
We pass certain procedure, and get back more restricted type which is
serialized procedure.

    (define (make-serializer)
      (let ((mutex (make-mutex)))
        (lambda (p)
          (define (serialized-p . args)
            (mutex 'aquire)
            (let ((val (apply p args)))
              (mutex 'release)
              val))
          serialized-p))))

NOTE:
Here we can see one of the most standard uses of the higher-order
functions. We pass one procedure to another one which produce new
procedure which is combined with certain additional operations around
the passed one.
