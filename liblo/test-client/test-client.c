/*
 * test-client.c -- Roger Dannenberg, 2006
 *
 *  based on example_client.c by Steve Harris, Uwe Koloska
 *
 *  Usage: test-client [mode], where mode is:
 *    h -- help
 *    ? -- help
 *    t -- triangle (up/down ramp), default behavior
 *    i -- interactive, type returns to advance
 */
 
#include <stdio.h>
#include <stdlib.h>
#ifdef WIN32
#define usleep(x) Sleep((x)/1000)
#else
#include <unistd.h>
#endif

#include "lo/lo.h"

int main(int argc, char *argv[])
{
    /* an address to send messages to. sometimes it is better to let the server
     * pick a port number for you by passing NULL as the last argument */
    lo_address t = lo_address_new(NULL, "7770");
    char mode = 't';
    float x = 0.0;
    float delta = 0.1;
    int n = 0; /* slider number */

    if (argc == 2) {
        mode = argv[1][0];
        if (mode == '?' || mode == 'h' ||
            (mode != 'i' && mode != 't')) {
            printf("usage: test-client [?hti]\n");
            printf("    default (t) is triangle, (i)nteractive sends msg after each return\n");
            exit(1);
        }
    }

    printf("lo_address_new done\n");

    /* send messages to /slider with two arguments, report any
     * errors */
    while (1) {
        if (lo_send(t, "/slider", "if", n, x) == -1) {
            printf("OSC error %d: %s\n", lo_address_errno(t), lo_address_errstr(t));
            break;
        } else {
            printf("/slider %d %g\n", n, x);
        }
        x = x + delta;
        if (x > 1.0 - delta * 0.5) {
            x = 1.0;
            delta = -delta;
        } else if (x < -delta * 0.5) {
            x = 0.0;
            delta = -delta;
        }
        if (mode == 'i') {
            while (getchar() != '\n') ;
        } else {
            usleep(100000);
        }
    }

    printf("done calling lo_send\n");

    return 0;
}

