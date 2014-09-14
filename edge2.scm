(use concurrent-native-callbacks mailbox-threads)

(foreign-declare "#include \"deps/wiringPi/wiringPi/wiringPi.h\"")
(foreign-declare "#include \"edge2-callbacks.h\"")

(define INT_EDGE_FALLING (foreign-value "INT_EDGE_FALLING" int))

(define t (current-thread))

;; static void (*isrFunctions [64])(void) ;
;; isrFunctions [pin] = function ;

;(define-syntax make-callbacks
;  (syntax-rules ()
;    ((_ ()) '())
;    ((_ (n . rest))
;     (begin (define-concurrent-native-callback (n)
;              (thread-send t (string->number (substring n 1))))
;            (make-callbacks rest)))))
;
;(make-callbacks
;  (      "f1"  "f2"  "f3"  "f4"  "f5"  "f6"  "f7"  "f8"  "f9"
;   "f10" "f11" "f12" "f13" "f14" "f15" "f16" "f17" "f18" "f19"
;   "f20" "f21" "f22" "f23" "f24" "f25" "f26" "f27" "f28" "f29"
;   "f30" "f31" "f32" "f33" "f34" "f35" "f36" "f37" "f38" "f39"
;   "f40" "f41" "f42" "f43" "f44" "f45" "f46" "f47" "f48" "f49"
;   "f50" "f51" "f52" "f53" "f54" "f55" "f56" "f57" "f58" "f59"
;   "f60" "f61" "f62" "f63" "f64"))

(define-concurrent-native-callback ((f1 'raspberry-pi-gpio)) (thread-send t 1))
(define-concurrent-native-callback ((f2 'raspberry-pi-gpio)) (thread-send t 2))
(define-concurrent-native-callback ((f3 'raspberry-pi-gpio)) (thread-send t 3))
(define-concurrent-native-callback ((f4 'raspberry-pi-gpio)) (thread-send t 4))
(define-concurrent-native-callback ((f5 'raspberry-pi-gpio)) (thread-send t 5))
(define-concurrent-native-callback ((f6 'raspberry-pi-gpio)) (thread-send t 6))
(define-concurrent-native-callback ((f7 'raspberry-pi-gpio)) (thread-send t 7))
(define-concurrent-native-callback ((f8 'raspberry-pi-gpio)) (thread-send t 8))

(foreign-code
  "callbacks [0] = f1;
   callbacks [1] = f2;
   callbacks [2] = f3;
   callbacks [3] = f4;
   callbacks [4] = f5;
   callbacks [5] = f6;
   callbacks [6] = f7;
   callbacks [7] = f8;")

;   callbacks [8] = f9;
;   callbacks [9] = f10;
;   callbacks [10] = f11;
;   callbacks [11] = f12;
;   callbacks [12] = f13;
;   callbacks [13] = f14;
;   callbacks [14] = f15;
;   callbacks [15] = f16;
;   callbacks [16] = f17;
;   callbacks [17] = f18;
;   callbacks [18] = f19;
;   callbacks [19] = f20;
;   callbacks [20] = f21;
;   callbacks [21] = f22;
;   callbacks [22] = f23;
;   callbacks [23] = f24;
;   callbacks [24] = f25;
;   callbacks [25] = f26;
;   callbacks [26] = f27;
;   callbacks [27] = f28;
;   callbacks [28] = f29;
;   callbacks [29] = f30;
;   callbacks [30] = f31;
;   callbacks [31] = f32;
;   callbacks [32] = f33;
;   callbacks [33] = f34;
;   callbacks [34] = f35;
;   callbacks [35] = f36;
;   callbacks [36] = f37;
;   callbacks [37] = f38;
;   callbacks [38] = f39;
;   callbacks [39] = f40;
;   callbacks [40] = f41;
;   callbacks [41] = f42;
;   callbacks [42] = f43;
;   callbacks [43] = f44;
;   callbacks [44] = f45;
;   callbacks [45] = f46;
;   callbacks [46] = f47;
;   callbacks [47] = f48;
;   callbacks [48] = f49;
;   callbacks [49] = f50;
;   callbacks [50] = f51;
;   callbacks [51] = f52;
;   callbacks [52] = f53;
;   callbacks [53] = f54;
;   callbacks [54] = f55;
;   callbacks [55] = f56;
;   callbacks [56] = f57;
;   callbacks [57] = f58;
;   callbacks [58] = f59;
;   callbacks [59] = f60;
;   callbacks [60] = f61;
;   callbacks [61] = f62;
;   callbacks [62] = f63;
;   callbacks [63] = f64;")

;(print (foreign-value f1 (function void)))
;(print (vector-ref callbacks (- 64 1)))
;(print (foreign-value f64 (function void)))

(define setup-gpio
  (foreign-lambda int "wiringPiSetupGpio"))

(define (on-falling pin)
  ((foreign-lambda* int ((int pin) (int type))
     "int r = wiringPiISR(pin, type, callbacks[pin - 1]);
      C_return(r);")
   pin INT_EDGE_FALLING))

(setup-gpio)
(on-falling 7)
(on-falling 8)

(let msgloop ((msg (thread-receive)))
  (printf "interrupt on ~S~n" msg)
  (msgloop (thread-receive)))
