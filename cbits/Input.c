#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <termios.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <math.h>
#include "Input.h"

// maximum amount that weights can change from the average before they're reported as a reliable one
const double MAX_WEIGHT_JITTER = 10;

const double MIN_REPORT_WEIGHT = 500;
const int MIN_CHANGE_TO_REPORT_NEW = 500;

#define WEIGHT_RECORD_SIZE 10
const int NUM_IGNORED_OLD_RECORDS = 5; // must be >= 0
const int MIN_DIFF_BETWEEN_REPORTS = 10;

const char *readPacket (char *packet, double *output)
{
	*output = 0;

	unsigned char xorLow = 0;
	unsigned char xorHigh = 0;

	char *read = packet;
	char c;

	// start byte
	if (*read++ != 2)
		return "Packet doesn't start with a '2'";

	// sign
	int sign = 0;
	c = *read++;
	xorLow ^= (c & 15);  // 0b00001111
	xorHigh ^= (c & 240); // 0b11110000

	switch (c) {
	case '+': sign = 1; break;
	case '-': sign = -1; break;
	default: return "Sign not a + or -";
	}

	// weight
	for (int j = 6; j-- > 0; ) {
		c = *read++;
		if (c < '0' || c > '9')
			return "Part of weight is not a number";
		*output += (c - '0') * pow (10, j);
		xorLow ^= (c & 15);
		xorHigh ^= (c & 240); 
	} 

	*output *= sign;

	c = *read++;

	xorLow ^= (c & 15);
	xorHigh ^= (c & 240);

	if (c < '0' || c > '4')
		return "Decimal point index not a number in the range [0..4]";

	*output /= pow (10, c - '0');

	unsigned int xorHighCheck, xorLowCheck;

	sscanf (read, "%x%x", &xorHighCheck, &xorLowCheck);

	/*	if (xorLowCheck != xorLow)
		throw ReadError ("xor low bits inconsistent with checksum");
	if (xorHighCheck != xorHigh)
		throw ReadError ("xor high bits inconsistent with checksum");
	*/
	return NULL;
}	

/* Report the current average of the last WEIGHT_RECORD_SIZE - NUM_IGNORED_OLD_RECORDS records, and whether there is a new axle weight.

   Axle weight is detected by determining by checking that each of these apply:
    - all weight records are within MAX_WEIGHT_JITTER of the average of all weight records (so we don't report uncertain weightings)
    - weight to report is greater than MIN_REPORT_WEIGHT (so we don't report an empty scale)
    - we are looking for a new weight (either in the initial state, or if we previously had weight jitter)
 */

void checkRecord (void (*reportWeight)(double, int), double *records, int *readingsSinceLastReport, double *lastReported)
{
	double sum = 0;
	for (int i = 0; i < WEIGHT_RECORD_SIZE; i++) {
		sum += records[i];
	}
	double average = sum / WEIGHT_RECORD_SIZE;
	int jitter = 0;
	for (int i = 0; i < WEIGHT_RECORD_SIZE; i++) {
		if (fabs (average - records[i]) > MAX_WEIGHT_JITTER) {
			jitter = 1;
			break;
		}
	}

	// leave out old readings to get a more correct reported average when approaching stability (i.e. pretty much any time in real use)
	double reportSum = 0;
	for (int i = 0; i < WEIGHT_RECORD_SIZE - NUM_IGNORED_OLD_RECORDS; i++) {
		reportSum += records[i];
	}

	double reportAverage = reportSum / (WEIGHT_RECORD_SIZE - NUM_IGNORED_OLD_RECORDS);

 	int doReport = (reportAverage > MIN_REPORT_WEIGHT && *readingsSinceLastReport > MIN_DIFF_BETWEEN_REPORTS && !jitter && *lastReported == 0) ? 1 : 0;
	if (!jitter) *readingsSinceLastReport = 0; // either we're reporting a weight or are holding steady on an already reported weight
	
	if (doReport)
		*lastReported = reportAverage;

	reportWeight (reportAverage, doReport);
}

void recordWeight (void (*reportWeight)(double, int), double *records, double newWeight, int *readingsSinceLastReport) 
{
	static double lastReported;
	if (MIN_CHANGE_TO_REPORT_NEW < fabs (newWeight - lastReported))
		lastReported = 0;
	memmove (records + 1, records, (WEIGHT_RECORD_SIZE - 1) * sizeof (double)); 
	records[0] = newWeight;
	checkRecord (reportWeight, records, readingsSinceLastReport, &lastReported);
}



void input (void (*reportProcessedWeight)(double, int), void (*reportRawWeight)(double)) {
	int serial;

	srand (time (0));

	char *filenames[] = {"/dev/ttyS0", "/dev/ttyS1", "/dev/ttyUSB0"};
	for (int i = 0; i < 3; i++) {
		printf("%s\n", filenames[i]);
		serial = open (filenames[i], O_RDWR | O_NOCTTY | O_NDELAY);
		if (serial != -1)
			continue;
	}

	if (serial == -1) {
		perror ("unable to open any TTY device out of S0, S1 and USB0 - ");
		fprintf (stderr, "%s", "Just outputting some random data then.\n");
		while (1) {
			sleep (1);
			double w = (double) (rand() % 1000);
			(*reportProcessedWeight) (w, 1);
			(*reportRawWeight) (w);
		}
	}

	struct termios tio;
	memset (&tio, 0, sizeof (tio));
	tio.c_cflag = B1200 | // 1200 baud
		CREAD | // allow reading
		CLOCAL | // don't use modem control
		CS8; // 8 bits per character, 1 stopbit
	tio.c_cc[VMIN] = 23; // just enough to fit at most one 12-byte packet
	// todo: less lazy input? (use a larger buffer, parse multiple packets per read)

	tcsetattr (serial, TCSAFLUSH, &tio);

	fcntl (serial, F_SETFL, 0);

	double weightRecord[WEIGHT_RECORD_SIZE] = {0};
	int readingsSinceLastReport = 0;

	while (1) {
		char buf[256] = {0};
		int nread = 0;
		struct timeval tv;
		gettimeofday (&tv, NULL);
		
		nread = read (serial, buf, 23);

		if (nread == -1) {
			perror ("unable to read from serial device - ");
			exit (1);
		}

		if (nread < 23)
			continue;

		// find where the packet begins
		int start = -1;
		for (int i = 0; i < 12; i++) {
			if (buf[i] == 2) {
				start = i;
				break;
			}
		}
		
		if (start == -1)
			continue;

		// check for transmission errors within the packet
		int ok = 1;
		for (int i = start; i < start + 12; i++) {
			if (buf[i] == 0) {
				ok = 0;
				break;
			}
		}
		if (!ok)
			continue;
		
		double weight;
		const char *error = readPacket (buf + start, &weight);

		if (error) {
			printf ("%s\n", error);
			continue;
		}

		reportRawWeight (weight);
		recordWeight (reportProcessedWeight, weightRecord, weight, &readingsSinceLastReport);
		readingsSinceLastReport++;
	}
}

