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
   digital-write/time
   digital-read
   analog-write
   analog-read
   set-edge
   receive-gpio-event
   current-time-raw
   HIGH
   LOW
   )

(import scheme chicken foreign)
(use srfi-18 posix)

(foreign-declare "#include <pthread.h>")
(foreign-declare "#include <wiringPi.h>")

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

(: setup-gpio (-> fixnum))
(define setup-gpio
  (foreign-lambda int "wiringPiSetupGpio"))

(: setup-system (-> fixnum))
(define setup-system
  (foreign-lambda int "wiringPiSetupSys"))

(: setup-virtual (-> fixnum))
(define setup-virtual
  (foreign-lambda int "wiringPiSetup"))

(: setup-physical (-> fixnum))
(define setup-physical
  (foreign-lambda int "wiringPiSetupPhys"))

(: board-rev (--> fixnum))
(define board-rev
  (foreign-lambda int "piBoardRev"))

(: pin-mode (fixnum symbol -> *))
(define (pin-mode pin mode)
  (let ((int_mode
	 (case mode
	   ((input) INPUT)
	   ((output) OUTPUT)
	   ((pwm-output) PWM_OUTPUT)
	   ((gpio-clock) GPIO_CLOCK)
	   (else (abort "Unknown mode")))))
    ((foreign-lambda* void ((int pin) (int mode)) "pinMode(pin, mode);")
     pin int_mode)))

(: pull-up-dn-control (fixnum symbol -> *))
(define (pull-up-dn-control pin pud)
  (let ((int_pud
	 (case pud
	   ((off) PUD_OFF)
	   ((down) PUD_DOWN)
	   ((up) PUD_UP)
	   (else (abort "Unknown PUD value")))))
    ((foreign-lambda* void ((int pin) (int pud)) "pullUpDnControl(pin, pud);")
     pin int_pud)))

;; (pwm-write pin value)
(: pwm-write (fixnum fixnum -> *))
(define pwm-write
  (foreign-lambda void "pwmWrite" int int))

;; (digital-write pin value)
(: digital-write (fixnum fixnum -> *))
(define digital-write
  (foreign-lambda void "digitalWrite" int int))

;; (digital-read pin) => HIGH / LOW
(: digital-read (fixnum -> fixnum))
(define digital-read
  (foreign-lambda int "digitalRead" int))

;; (analog-read pin) => int
(: analog-read (fixnum -> fixnum))
(define analog-read
  (foreign-lambda int "analogRead" int))

;; (analog-write pin value)
(: analog-write (fixnum fixnum -> *))
(define analog-write
  (foreign-lambda void "analogWrite" int int))

;; converts an edge type symbol to the wiringPi int for use as
;; an argument to the wiringPiISR function
(: edge->int (symbol --> fixnum))
(define (edge->int type)
  (case type
    ((falling) INT_EDGE_FALLING)
    ((rising) INT_EDGE_RISING)
    ((both) INT_EDGE_BOTH)
    ((setup) INT_EDGE_SETUP)
    (else
      (abort (sprintf "Unknown edge type: ~S" type)))))

;; wiringPi uses "static void (*isrFunctions [64])(void)" for storing
;; callbacks on pins, so we're just going to set up a C code for each
;; possible option

;; stores handler functions to be attached to gpio interrupt events in wiringPi
 (foreign-declare
 #<<EOF

#include <errno.h>

#include <time.h>

struct wipisig {
 short int pin;
 struct timespec time;
};

static int the_pi_pipe = 0;

static int Cpi_ISR(int n) {
  int v=digitalRead(n);
  struct wipisig s;
  /* Using RAW time, since adjustments by NTP will do more harm than good on physical measurements */
  clock_gettime(CLOCK_MONOTONIC_RAW, &s.time);
  s.pin = v==HIGH ? n : -n;
  /* Return value will be ignored here.  Maybe that's bad? */
  return write(the_pi_pipe, &s, sizeof(struct wipisig));
}

#define DEFISR(n) static void Cpi_ISR##n() { Cpi_ISR(n); }

#define REGISR(n) Cpi_ISR##n

DEFISR(1)  DEFISR(2)  DEFISR(3)  DEFISR(4)  DEFISR(5)  DEFISR(6)  DEFISR(7)  DEFISR(8)
DEFISR(9)  DEFISR(10) DEFISR(11) DEFISR(12) DEFISR(13) DEFISR(14) DEFISR(15) DEFISR(16)
DEFISR(17) DEFISR(18) DEFISR(19) DEFISR(20) DEFISR(21) DEFISR(22) DEFISR(23) DEFISR(24)
DEFISR(25) DEFISR(26) DEFISR(27) DEFISR(28) DEFISR(29) DEFISR(30) DEFISR(31) DEFISR(32)
DEFISR(33) DEFISR(34) DEFISR(35) DEFISR(36) DEFISR(37) DEFISR(38) DEFISR(39) DEFISR(40)
DEFISR(41) DEFISR(42) DEFISR(43) DEFISR(44) DEFISR(45) DEFISR(46) DEFISR(47) DEFISR(48)
DEFISR(49) DEFISR(50) DEFISR(51) DEFISR(52) DEFISR(53) DEFISR(54) DEFISR(55) DEFISR(56)
DEFISR(57) DEFISR(58) DEFISR(59) DEFISR(60) DEFISR(61) DEFISR(62) DEFISR(63) DEFISR(64)

static void (*callbacks [64])(void) = {
REGISR(1),  REGISR(2),  REGISR(3),  REGISR(4),  REGISR(5),  REGISR(6),  REGISR(7),  REGISR(8),
REGISR(9),  REGISR(10), REGISR(11), REGISR(12), REGISR(13), REGISR(14), REGISR(15), REGISR(16),
REGISR(17), REGISR(18), REGISR(19), REGISR(20), REGISR(21), REGISR(22), REGISR(23), REGISR(24),
REGISR(25), REGISR(26), REGISR(27), REGISR(28), REGISR(29), REGISR(30), REGISR(31), REGISR(32),
REGISR(33), REGISR(34), REGISR(35), REGISR(36), REGISR(37), REGISR(38), REGISR(39), REGISR(40),
REGISR(41), REGISR(42), REGISR(43), REGISR(44), REGISR(45), REGISR(46), REGISR(47), REGISR(48),
REGISR(49), REGISR(50), REGISR(51), REGISR(52), REGISR(53), REGISR(54), REGISR(55), REGISR(56),
REGISR(57), REGISR(58), REGISR(59), REGISR(60), REGISR(61), REGISR(62), REGISR(63), REGISR(64)
};

/* This thread does not do much.  It waits (at high priority) for the (locked) mutex to be
   locked again, i.e. forever.
   However signals should be dispatched here faster than to the CHICKEN thread, which may
   be busy.  Future version may want to use this thread to tie digitalWrite closer to the
   time of when the pin value was changed.
 */
static PI_THREAD(pi_sig_th)
{
  pthread_mutex_t mutex;
  pthread_mutex_init(&mutex, NULL);
  pthread_mutex_lock(&mutex);
  piHiPri(99); // ignored if failing - still hoping to get signals delivered faster.
  for(;;) {
    pthread_mutex_lock(&mutex);
  }
}

static time_t raw_startup_time_seconds;

static int C_wiringPiInit(int fd)
{
  struct timespec tv;
  pthread_t th;
  if(clock_gettime(CLOCK_MONOTONIC_RAW, &tv) != 0) return 0;
  the_pi_pipe = fd;
  raw_startup_time_seconds = tv.tv_sec;
  return piThreadCreate(pi_sig_th) == 0;
}


static int wipisigread(int fd, int *v, int *sec, int *nsec) {
  struct wipisig s;
  if(read(fd, &s, sizeof(struct wipisig)) < 0 ) {
    return -1;
  }
  *sec = s.time.tv_sec - raw_startup_time_seconds;
  *nsec =  s.time.tv_nsec;
  *v = s.pin < 0 ? LOW : HIGH;
  return s.pin < 0 ? -s.pin : s.pin;
}

/* Reader for the RAW reference time. */
static int CpiRawTime(int *nsec) {
  struct timespec tv;
  if(clock_gettime(CLOCK_MONOTONIC_RAW, &tv) != 0) return 0;
  *nsec = tv.tv_nsec;
  return tv.tv_sec - raw_startup_time_seconds;
}

static int digitalWriteWithTime(int pin, int val, int *nsec) {
 digitalWrite(pin, val);
 return CpiRawTime(nsec);
}

EOF
)

;; (digital-write pin value)
(: digital-write/time (fixnum fixnum -> fixnum fixnum))
(define (digital-write/time pin value)
  (let-location
   ((ns int))
   (let ((sec ((foreign-lambda int "digitalWriteWithTime" int int (c-pointer int))
	       pin value (location ns))))
     (values sec ns))))

(define-values (pi-pipe-rd pi-pipe-wr) (create-pipe))

(define init-pi-pipe (foreign-lambda int "C_wiringPiInit" int))

(##sys#file-nonblocking! pi-pipe-rd)
(##sys#file-nonblocking! pi-pipe-wr)

(init-pi-pipe pi-pipe-wr)

;; sets up an interrupt handler for the pin and given edge using wiringPiISR
(: set-edge (fixnum symbol -> fixnum))
(define (set-edge pin type)
  (let ((edge-type-int (edge->int type)))
    ((foreign-lambda* int ((int pin) (int type))
       "int r = wiringPiISR(pin, type, callbacks[pin - 1]);
        C_return(r);")
     pin edge-type-int)))

(define-foreign-variable _errno int "errno")

;; blocks until a gpio interrupt occurs on a pin that had set-edge called on it
(: receive-gpio-event (-> fixnum fixnum fixnum fixnum))
(define receive-gpio-event
  (let ((b (make-string 1)))
    (lambda ()
      (let-location
       ((pinval int) (intsec int) (intns int))
       (let loop ()
	 (let ((x ((foreign-lambda int "wipisigread" int (c-pointer int) (c-pointer int) (c-pointer int))
		   pi-pipe-rd (location pinval) (location intsec) (location intns))))
	   (if (fx< x 0)
	       (cond
		((or (eq? _errno errno/wouldblock)
		     (eq? _errno errno/again))
		 (thread-wait-for-i/o! pi-pipe-rd #:input)
		 (loop))
		((eq? _errno errno/intr)
		 (##sys#dispatch-interrupt loop))
		(else (error "receive-gpio-event unhandled error code" _errno)))
	       (values x pinval intsec intns))))))))

(: current-time-raw (-> fixnum fixnum))
(define (current-time-raw)
  (let-location
   ((ns int))
   (let ((sec ((foreign-lambda int "CpiRawTime" (c-pointer int)) (location ns))))
     (values sec ns))))

)
