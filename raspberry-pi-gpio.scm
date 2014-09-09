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
   HIGH
   LOW
   on-falling
   on-rising
   dispatch-events
   )

(import scheme chicken foreign)
(use concurrent-native-callbacks lolevel)

(foreign-declare "#include \"deps/wiringPi/wiringPi/wiringPi.h\"")

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
;(define INT_EDGE_BOTH (foreign-value "INT_EDGE_BOTH" int))
;(define INT_EDGE_SETUP (foreign-value "INT_EDGE_SETUP" int))

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

(define (on-falling pin callback)
  ;; TODO: define a callback for each event (rising/falling) on each pin
  ;; (can we use wiringPi to find out available pin numbers?) - then use
  ;; the appropriate conccurent native callback to send a message to the current
  ;; mailbox thread to provide the pin information - this could be an alternative
  ;; to using a callback, eg:
  ;;
  ;; (listen-rising 7)
  ;; (listen-falling 8)
  ;;
  ;; (define (loop)
  ;;   (let-values ((pin edge) (recieve-gpio-event))
  ;;     (do-stuff pin edge)
  ;;     (loop)))
  ;;
  ;; ...I think I prefer the callback API, but preferably executed in the current
  ;; thread using the mailbox approach
  ;;
  (define-concurrent-native-callback ((f 'raspbery-pi-gpio)) (callback))
  ((foreign-lambda* int ((int pin) (int type))
    "int r = wiringPiISR(pin, type, &f);
     C_return(r);")
   pin INT_EDGE_FALLING))

(define (on-rising pin callback)
  (define-concurrent-native-callback ((f 'raspberry-pi-gpio)) (callback))
  ((foreign-lambda* int ((int pin) (int type))
    "int r = wiringPiISR(pin, type, &f);
     C_return(r);")
   pin INT_EDGE_FALLING))

(define (dispatch-events)
  (dispatch 'raspberry-pi-gpio))

)
