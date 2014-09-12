(use posix srfi-18)

;(foreign-declare "#include <stdio.h>")
;(foreign-declare "#include <stdint.h>")
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

(clear-pending fd)
(thread-wait-for-i/o! fd #:input)
(clear-interrupt fd)
(printf "got interrupt")
