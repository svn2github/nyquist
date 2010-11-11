/*
 * ser-to-osc.cpp -- Roger Dannenberg, 2006
 *
 *  Usage: ser-to-osc inputdevice
 *
 * Note: this is C++ only because the Windows version uses the Serial class
 * for USB-Serial I/O. Hopefully compiling this as C++ will not cause
 * problems on Mac and Linux.
 *
 */
 
#include <stdio.h>
#include <stdlib.h>
#ifdef WIN32
#else
#include <unistd.h>
#include <ctype.h>
#include "string.h"
#endif

#define _WIN32_WINNT 0x1000
#include "lo/lo.h"

#ifdef WIN32
#include "Serial.h"

enum { EOF_Char = 27 };

CSerial serial;

typedef CSerial *port_type;

#define MAX_LINE 256
char inp_line[MAX_LINE];
int inp_len;

CSerial *port_open(char *inputname)
{
    LONG lLastError = serial.Open(inputname, 0, 0, false);
    if (lLastError != ERROR_SUCCESS) return NULL;
    // Setup the serial port (9600,8N1, which is the default setting)
    lLastError = serial.Setup(CSerial::EBaud9600, CSerial::EData8,
                              CSerial::EParNone, CSerial::EStop1);
	if (lLastError != ERROR_SUCCESS) return NULL;
    // Register only for the receive event
    lLastError = serial.SetMask(CSerial::EEventBreak |
								CSerial::EEventCTS   |
								CSerial::EEventDSR   |
								CSerial::EEventError |
								CSerial::EEventRing  |
								CSerial::EEventRLSD  |
								CSerial::EEventRecv);
	if (lLastError != ERROR_SUCCESS) return NULL;
	// Use 'non-blocking' reads, because we don't know how many bytes
	// will be received. This is normally the most convenient mode
	// (and also the default mode for reading data).
    lLastError = serial.SetupReadTimeouts(CSerial::EReadTimeoutNonblocking);
	if (lLastError != ERROR_SUCCESS) return NULL;
    inp_len = 0;
    return &serial;
}


char *port_getln(CSerial *serial, size_t *len_ptr)
{
    // Keep reading data, until an EOF (CTRL-Z) has been received
	bool done = false;
    LONG lLastError;
    *len_ptr = 0;
    while (!done) {
	    // Wait for an event
		lLastError = serial->WaitEvent();
		if (lLastError != ERROR_SUCCESS) return NULL;
		// Save event
		const CSerial::EEvent eEvent = serial->GetEventType();
		// Handle break event
		if (eEvent & CSerial::EEventBreak) {
			// printf("\n### BREAK received ###\n");
		}
		// Handle CTS event
		if (eEvent & CSerial::EEventCTS) {
			// printf("\n### Clear to send %s ###\n", serial->GetCTS()?"on":"off");
		}
		// Handle DSR event
		if (eEvent & CSerial::EEventDSR) {
			// printf("\n### Data set ready %s ###\n", serial->GetDSR()?"on":"off");
		}
		// Handle error event
		if (eEvent & CSerial::EEventError) {
			printf("\n### ERROR: ");
			switch (serial->GetError()) {
			    case CSerial::EErrorBreak: 		
                    printf("Break condition");			
                    break;
			    case CSerial::EErrorFrame:
                    printf("Framing error");
                    break;
    			case CSerial::EErrorIOE:
                    printf("IO device error");
                    break;
	    		case CSerial::EErrorMode:
                    printf("Unsupported mode");
                    break;
		    	case CSerial::EErrorOverrun:
                    printf("Buffer overrun");
                    break;
			    case CSerial::EErrorRxOver:
                    printf("Input buffer overflow");
                    break;
    			case CSerial::EErrorParity:
                    printf("Input parity error");
                    break;
	    		case CSerial::EErrorTxFull:
                    printf("Output buffer full");
                    break;
		    	default:
                    printf("Unknown");
                    break;
			}
			printf(" ###\n");
		}

		// Handle ring event
		if (eEvent & CSerial::EEventRing) {
			// printf("\n### RING ###\n");
		}
		// Handle RLSD/CD event
		if (eEvent & CSerial::EEventRLSD) {
			// printf("\n### RLSD/CD %s ###\n", serial->GetRLSD()?"on":"off");
		}
		// Handle data receive event
		if (eEvent & CSerial::EEventRecv) {
			// Read data, until there is nothing left
            while (true) {
			// Read data from the COM-port
			    DWORD dwBytesRead = 0;
				lLastError = serial->Read(inp_line + inp_len, 1, &dwBytesRead);
				if (lLastError != ERROR_SUCCESS) return NULL;
                if (dwBytesRead == 0) break;
                // printf("Read %c (%d)\n", inp_line[inp_len], inp_line[inp_len]);
                if (inp_line[inp_len] == '\n' ||
                    // inp_line[inp_len] == '\r' ||
                    inp_line[inp_len] == EOF_Char ||
                    inp_len >= MAX_LINE - 2) {
                    inp_line[inp_len + 1] = '\0';
                    *len_ptr = inp_len + 1;
                    inp_len = 0;
                    done = true;
                    break;
                }
                inp_len++;
			}
		}
	}
    return inp_line;
}


void port_close(CSerial *serial)
{
    serial->Close();
}


#else
typedef FILE *port_type;

FILE *port_open(char *inputname)
{
    return fopen(inputname, "r");
}

char *port_getln(FILE *input, size_t *len_ptr)
{
    // linux does not have fgetln (must be bsd only)
    // return fgetln(input, len_ptr);
    static char line[256];
    char *result = fgets(line, 256, input);
    if (result) *len_ptr = strlen(line);
    return result;
}

void port_close(FILE *input)
{
    fclose(input);
}
#endif

int main(int argc, char *argv[])
{
    /* an address to send messages to. sometimes it is better to let the server
     * pick a port number for you by passing NULL as the last argument */
    lo_address t = lo_address_new(NULL, "7770");
    char *inputname = NULL;
    port_type input = NULL;
    int verbose = 1;
    int argptr = 1;
    while (argptr < argc) {
        char *arg = argv[argptr];
        if (*arg == '-') {
            if (arg[1] == 'q') {
                verbose = 0;
            }
        } else if (!inputname) {
            inputname = arg;
        }
        argptr++;
    }
    if (inputname) input = port_open(inputname);
    if (!input) {
        printf("Could not open %s\n", inputname);
        exit(EXIT_FAILURE);
    }

    /* send messages to /slider with two arguments, report any errors */
    while (1) {
        /* get input data, report errors */
        size_t len;
        char *s = port_getln(input, &len);
        int slider_no, value;
        float x;
        char channel[64]; /* buffer to hold "Channel" */
        if (!s) exit(EXIT_SUCCESS);
        /* we don't need the newline, but we do want an EOS */
        s[len - 1] = 0;
        len--;
        while (isspace(s[len - 1])) s[--len] = 0; /* trim white space */
        if (verbose) printf("%s: ", s);
        if (sscanf(s, "%s %d %d", channel, &slider_no, &value) != 3) {
            puts("Error: expected 2 integers\n");
            fflush(stdout);
        }
        x = value / 255.0F;
        if (x > 1) x = 1;
        if (x < 0) x = 0;
        if (verbose) printf("/slider %d %g\n", slider_no, x);
        if (lo_send(t, "/slider", "if", slider_no, x) == -1) {
            printf("OSC error %d: %s\n", lo_address_errno(t), lo_address_errstr(t));
            break;
        }
    }
    port_close(input);
    return 0; /* make compiler happy */
}

