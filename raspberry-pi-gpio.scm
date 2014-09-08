(module raspberry-pi-gpio
  (
   board-rev
   )

(import scheme chicken foreign)
(use lolevel)

(foreign-declare "#include \"deps/wiringPi/wiringPi/wiringPi.h\"")

(foreign-code "wiringPiSetupGpio();")

(define board-rev
  (foreign-lambda int "piBoardRev"))

)
