;; CHICKEN Scheme bindings to the wiringPi C library
;;
;; NOTE: This requires you to have wiringPi installed, including its
;; command-line "gpio" tool if you want to use set-edge and recieve-gpio-event
;; for gpio interrupts

(module raspberry-pi-gpio
  (
   setup-gpio
   setup-system
   setup-virtual
   setup-physical
   board-rev
   pin-mode
   pull-up-dn-control
   pwm-write
   digital-write
   digital-read
   analog-write
   analog-read
   set-edge
   receive-gpio-event
   HIGH
   LOW
   )

(import scheme chicken foreign)
(use mailbox-threads concurrent-native-callbacks srfi-18 lolevel)

(foreign-declare "#include <wiringPi.h>")

;; stores handler functions to be attached to gpio interrupt events in wiringPi
(foreign-declare "static void (*callbacks [64])(void);")

;; used for receiving gpio interrupt events
(define t (current-thread))

(define INPUT (foreign-value "INPUT" int))
(define OUTPUT (foreign-value "OUTPUT" int))
(define PWM_OUTPUT (foreign-value "PWM_OUTPUT" int))
(define GPIO_CLOCK (foreign-value "GPIO_CLOCK" int))

(define PUD_OFF (foreign-value "PUD_OFF" int))
(define PUD_DOWN (foreign-value "PUD_DOWN" int))
(define PUD_UP (foreign-value "PUD_UP" int))

(define HIGH (foreign-value "HIGH" int))
(define LOW (foreign-value "LOW" int))

(define INT_EDGE_FALLING (foreign-value "INT_EDGE_FALLING" int))
(define INT_EDGE_RISING (foreign-value "INT_EDGE_RISING" int))
(define INT_EDGE_BOTH (foreign-value "INT_EDGE_BOTH" int))
(define INT_EDGE_SETUP (foreign-value "INT_EDGE_SETUP" int))

(define setup-gpio
  (foreign-lambda int "wiringPiSetupGpio"))

(define setup-system
  (foreign-lambda int "wiringPiSetupSys"))

(define setup-virtual
  (foreign-lambda int "wiringPiSetup"))

(define setup-physical
  (foreign-lambda int "wiringPiSetupPhys"))

(define board-rev
  (foreign-lambda int "piBoardRev"))

(define (pin-mode pin mode)
  (let ((int_mode
          (case mode (('input) INPUT)
                     (('output) OUTPUT)
                     (('pwm-output) PWM_OUTPUT)
                     (('gpio-clock) GPIO_CLOCK)
                     (else (abort "Unknown mode")))))
    ((foreign-lambda* void ((int pin) (int mode)) "pinMode(pin, mode);")
     pin int_mode)))

(define (pull-up-dn-control pin pud)
  (let ((int_pud
          (case pud (('off) PUD_OFF)
                    (('down) PUD_DOWN)
                    (('up) PUD_UP)
                    (else (abort "Unknown PUD value")))))
    ((foreign-lambda* void ((int pin) (int pud)) "pullUpDnControl(pin, pud);")
     pin int_pud)))

;; (pwm-write pin value)
(define pwm-write
  (foreign-lambda void "pwmWrite" int int))

;; (digital-write pin value)
(define digital-write
  (foreign-lambda void "digitalWrite" int int))

;; (digital-read pin) => HIGH / LOW
(define digital-read
  (foreign-lambda int "digitalRead" int))

;; (analog-read pin) => int
(define analog-read
  (foreign-lambda int "analogRead" int))

;; (analog-write pin value)
(define analog-write
  (foreign-lambda void "analogWrite" int int))


;; wiringPi uses "static void (*isrFunctions [64])(void)" for storing callbacks
;; on pins, so we're just going to set up a concurrent-native-callback for each
;; possible option

;; TODO: can we avoid all this repetition using a macro?

(define-concurrent-native-callback ((f1 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 1)))
(define-concurrent-native-callback ((f2 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 2)))
(define-concurrent-native-callback ((f3 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 3)))
(define-concurrent-native-callback ((f4 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 4)))
(define-concurrent-native-callback ((f5 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 5)))
(define-concurrent-native-callback ((f6 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 6)))
(define-concurrent-native-callback ((f7 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 7)))
(define-concurrent-native-callback ((f8 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 8)))
(define-concurrent-native-callback ((f9 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 9)))
(define-concurrent-native-callback ((f10 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 10)))
(define-concurrent-native-callback ((f11 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 11)))
(define-concurrent-native-callback ((f12 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 12)))
(define-concurrent-native-callback ((f13 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 13)))
(define-concurrent-native-callback ((f14 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 14)))
(define-concurrent-native-callback ((f15 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 15)))
(define-concurrent-native-callback ((f16 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 16)))
(define-concurrent-native-callback ((f17 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 17)))
(define-concurrent-native-callback ((f18 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 18)))
(define-concurrent-native-callback ((f19 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 19)))
(define-concurrent-native-callback ((f20 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 20)))
(define-concurrent-native-callback ((f21 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 21)))
(define-concurrent-native-callback ((f22 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 22)))
(define-concurrent-native-callback ((f23 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 23)))
(define-concurrent-native-callback ((f24 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 24)))
(define-concurrent-native-callback ((f25 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 25)))
(define-concurrent-native-callback ((f26 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 26)))
(define-concurrent-native-callback ((f27 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 27)))
(define-concurrent-native-callback ((f28 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 28)))
(define-concurrent-native-callback ((f29 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 29)))
(define-concurrent-native-callback ((f30 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 30)))
(define-concurrent-native-callback ((f31 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 31)))
(define-concurrent-native-callback ((f32 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 32)))
(define-concurrent-native-callback ((f33 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 33)))
(define-concurrent-native-callback ((f34 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 34)))
(define-concurrent-native-callback ((f35 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 35)))
(define-concurrent-native-callback ((f36 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 36)))
(define-concurrent-native-callback ((f37 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 37)))
(define-concurrent-native-callback ((f38 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 38)))
(define-concurrent-native-callback ((f39 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 39)))
(define-concurrent-native-callback ((f40 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 40)))
(define-concurrent-native-callback ((f41 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 41)))
(define-concurrent-native-callback ((f42 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 42)))
(define-concurrent-native-callback ((f43 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 43)))
(define-concurrent-native-callback ((f44 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 44)))
(define-concurrent-native-callback ((f45 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 45)))
(define-concurrent-native-callback ((f46 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 46)))
(define-concurrent-native-callback ((f47 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 47)))
(define-concurrent-native-callback ((f48 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 48)))
(define-concurrent-native-callback ((f49 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 49)))
(define-concurrent-native-callback ((f50 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 50)))
(define-concurrent-native-callback ((f51 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 51)))
(define-concurrent-native-callback ((f52 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 52)))
(define-concurrent-native-callback ((f53 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 53)))
(define-concurrent-native-callback ((f54 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 54)))
(define-concurrent-native-callback ((f55 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 55)))
(define-concurrent-native-callback ((f56 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 56)))
(define-concurrent-native-callback ((f57 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 57)))
(define-concurrent-native-callback ((f58 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 58)))
(define-concurrent-native-callback ((f59 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 59)))
(define-concurrent-native-callback ((f60 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 60)))
(define-concurrent-native-callback ((f61 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 61)))
(define-concurrent-native-callback ((f62 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 62)))
(define-concurrent-native-callback ((f63 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 63)))
(define-concurrent-native-callback ((f64 'raspberry-pi-gpio)) (thread-send t (list 'interrupt 64)))

;; Add the concurrent-native-callbacks to the callbacks array defined in C above
;; We can then access them indexed by pin number when attaching the interrupt
;; handler function from C

(foreign-code
  "callbacks [0] = f1;
   callbacks [1] = f2;
   callbacks [2] = f3;
   callbacks [3] = f4;
   callbacks [4] = f5;
   callbacks [5] = f6;
   callbacks [6] = f7;
   callbacks [7] = f8;
   callbacks [8] = f9;
   callbacks [9] = f10;
   callbacks [10] = f11;
   callbacks [11] = f12;
   callbacks [12] = f13;
   callbacks [13] = f14;
   callbacks [14] = f15;
   callbacks [15] = f16;
   callbacks [16] = f17;
   callbacks [17] = f18;
   callbacks [18] = f19;
   callbacks [19] = f20;
   callbacks [20] = f21;
   callbacks [21] = f22;
   callbacks [22] = f23;
   callbacks [23] = f24;
   callbacks [24] = f25;
   callbacks [25] = f26;
   callbacks [26] = f27;
   callbacks [27] = f28;
   callbacks [28] = f29;
   callbacks [29] = f30;
   callbacks [30] = f31;
   callbacks [31] = f32;
   callbacks [32] = f33;
   callbacks [33] = f34;
   callbacks [34] = f35;
   callbacks [35] = f36;
   callbacks [36] = f37;
   callbacks [37] = f38;
   callbacks [38] = f39;
   callbacks [39] = f40;
   callbacks [40] = f41;
   callbacks [41] = f42;
   callbacks [42] = f43;
   callbacks [43] = f44;
   callbacks [44] = f45;
   callbacks [45] = f46;
   callbacks [46] = f47;
   callbacks [47] = f48;
   callbacks [48] = f49;
   callbacks [49] = f50;
   callbacks [50] = f51;
   callbacks [51] = f52;
   callbacks [52] = f53;
   callbacks [53] = f54;
   callbacks [54] = f55;
   callbacks [55] = f56;
   callbacks [56] = f57;
   callbacks [57] = f58;
   callbacks [58] = f59;
   callbacks [59] = f60;
   callbacks [60] = f61;
   callbacks [61] = f62;
   callbacks [62] = f63;
   callbacks [63] = f64;")

;; converts an edge type symbol to the wiringPi int for use as
;; an argument to the wiringPiISR function
(define (edge->int type)
  (case type
    ((falling) INT_EDGE_FALLING)
    ((rising) INT_EDGE_RISING)
    ((both) INT_EDGE_BOTH)
    ((setup) INT_EDGE_SETUP)
    (else
      (abort (sprintf "Unknown edge type: ~S" type)))))

;; sets up an interrupt handler for the pin and given edge using wiringPiISR
(define (set-edge pin type)
  (let ((edge-type-int (edge->int type)))
    ((foreign-lambda* int ((int pin) (int type))
       "int r = wiringPiISR(pin, type, callbacks[pin - 1]);
        C_return(r);")
     pin edge-type-int)))

;; blocks until a gpio interrupt occurs on a pin that had set-edge called on it
(define (receive-gpio-event)
  (thread-receive))

)
