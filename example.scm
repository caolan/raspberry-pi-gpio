(use raspberry-pi-gpio)

(setup-gpio)

(set-edge 7 'falling)
(set-edge 8 'both)

(define (loop)
  (let ((pin (receive-gpio-event)))
    (printf "Interrup on pin ~S, value: ~S~n" pin (digital-read pin))
    (loop)))

(loop)
