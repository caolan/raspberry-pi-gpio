(use concurrent-native-callbacks mailbox-threads)

(foreign-declare "#include \"deps/wiringPi/wiringPi/wiringPi.h\"")

(define INT_EDGE_FALLING (foreign-value "INT_EDGE_FALLING" int))

(define t (current-thread))

(define callbacks (make-vector 64))

(define-syntax make-callbacks
  (syntax-rules ()
    ((_ ()) '())
    ((_ (n . rest))
     (begin (define-concurrent-native-callback
              ((n 'raspberry-pi-gpio))
              (thread-send t (string->number (substring n 1))))
            (vector-set! callbacks
                         (- (string->number (substring n 1)) 1)
                         (foreign-value n (function void)))
            (make-callbacks rest)))))

(make-callbacks
  (      "f1"  "f2"  "f3"  "f4"  "f5"  "f6"  "f7"  "f8"  "f9"
   "f10" "f11" "f12" "f13" "f14" "f15" "f16" "f17" "f18" "f19"
   "f20" "f21" "f22" "f23" "f24" "f25" "f26" "f27" "f28" "f29"
   "f30" "f31" "f32" "f33" "f34" "f35" "f36" "f37" "f38" "f39"
   "f40" "f41" "f42" "f43" "f44" "f45" "f46" "f47" "f48" "f49"
   "f50" "f51" "f52" "f53" "f54" "f55" "f56" "f57" "f58" "f59"
   "f60" "f61" "f62" "f63" "f64"))

;(print (foreign-value f1 (function void)))
(print (vector-ref callbacks (- 64 1)))
(print (foreign-value f64 (function void)))

(define setup-gpio
  (foreign-lambda int "wiringPiSetupGpio"))

(define (on-falling pin)
  ((foreign-lambda* int ((int pin) (int type) ((function void (void)) f))
     "int r = wiringPiISR(pin, type, f);
      C_return(r);")
   pin INT_EDGE_FALLING (vector-ref callbacks (- pin 1))))

(setup-gpio)
(on-falling 7)
(on-falling 8)

(let msgloop ((msg (thread-receive)))
  (printf "interrupt on ~S~n" msg)
  (msgloop (thread-receive)))
