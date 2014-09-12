;(use posix srfi-18)
(use posix)

;(foreign-declare "#include <stdio.h>")
;(foreign-declare "#include <stdint.h>")
(foreign-declare "#include <poll.h>")
(foreign-declare "#include <sys/ioctl.h>")

(define fd (file-open "/sys/class/gpio/gpio7/value" open/rdwr))

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
;(thread-wait-for-i/o! fd #:input)
(clear-interrupt fd)
(printf "got interrupt")
