#include <stdio.h>
#include <stdint.h>
#include <poll.h>
#include <fcntl.h>
#include <sys/ioctl.h>

int main (int argc, char *argv []) {
    int fd = -1;
    int x;
    int i;
    int count;
    uint8_t c;
    struct pollfd polls;
    int mS = -1;

    if (fd == -1) {
        if ((fd = open("/sys/class/gpio/gpio7/value", O_RDWR)) < 0) {
            printf("Error opening fd");
            return 1;
        }
    }

    // clear any initial pending interrupt
    ioctl(fd, FIONREAD, &count);
    for (i = 0; i < count; i++) {
        read(fd, &c, 1);
    }

    // setup pull structure
    polls.fd = fd;
    polls.events = POLLPRI; // Urgent data!

    // wait for it
    x = poll(&polls, 1, -1);

    // do a dummy read to clear the interrupt
    // a one character read appears to be enough
    (void)read(fd, &c, 1);

    printf("got interrupt: %d", x);
    return 0;
}
