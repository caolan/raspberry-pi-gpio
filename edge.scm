(use posix)

(foreign-declare "#include <poll.h>")
(foreign-declare "#include <sys/ioctl.h>")


(define (set-edge)
  (define pin 7)
  (let ((fd (file-open "/sys/class/gpio/export" open/wronly)))
    (file-write fd (string-append (number->string pin) "\n"))
    (file-close fd))
  (let ((fd (file-open "/sys/class/gpio/gpio7/direction" open/wronly)))
    (file-write fd "in\n")
    (file-close fd))
  (let ((fd (file-open "/sys/class/gpio/gpio7/edge" open/wronly)))
    (file-write fd "falling\n")
    (file-close fd))
  (let ((uid (current-user-id)) (gid (current-group-id)))
    (change-file-owner "/sys/class/gpio/gpio7/value" uid gid)
    (change-file-owner "/sys/class/gpio/gpio7/edge" uid gid)))

(define (wait-for-edge)
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
  (clear-interrupt fd))

(set-edge)
(wait-for-edge)
(printf "got interrupt")
