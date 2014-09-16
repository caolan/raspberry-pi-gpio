(use raspberry-pi-gpio)

(setup-gpio)

(set-edge 7 'falling)
(set-edge 8 'falling)

(define (loop)
  (let ((pin (receive-gpio-event)))
    (printf "Interrup on pin ~S~n" pin)
    (loop)))

(loop)
