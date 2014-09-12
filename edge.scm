(use srfi-18 posix)

(foreign-declare "#include <poll.h>")
(foreign-declare "#include <sys/ioctl.h>")

(define (set-edge pin type)
  (process-wait (process-run "gpio" (list "edge" (number->string pin) type))))

(define (wait-for-edge pin)
  (define fd (file-open (sprintf "/sys/class/gpio/gpio~S/value" pin) open/rdwr))

  ;; clear any initial pending interrupt
  (define clear-pending
    (foreign-lambda* void ((int fd))
      "uint8_t c;
       int i, count;
       ioctl(fd, FIONREAD, &count);
       for (i = 0; i < count; i++) {
         read(fd, &c, 1);
       }"))

  ;; do a dummy read to clear the interrupt
  ;; a one character read appears to be enough
  (define clear-interrupt
    (foreign-lambda* void ((int fd))
      "uint8_t c;
       (void)read(fd, &c, 1);"))

  (define wait-for-interrupt
    (foreign-lambda* int ((int fd))
      "int x;
       int timeout = -1;
       struct pollfd polls;
       // Setup poll structure
       polls.fd     = fd ;
       polls.events = POLLPRI ;	// Urgent data!
       // Wait for it ...
       x = poll (&polls, 1, timeout);
       C_return(x);"))

  (clear-pending fd)
  (wait-for-interrupt fd)
  (clear-interrupt fd))

(set-edge 7 "falling")
(wait-for-edge 7)
(print "got interrupt on pin 7")

(set-edge 8 "falling")
(wait-for-edge 8)
(print "got interrupt on pin 8")
