//
//  internal.h
//  
//
//
//

#ifndef _internal_h
#define _internal_h

#include <math.h>

// hanning window
// 'x' is the independent variable of the window function.
// 'x' is actually i/window_length, it's from 0 to 1-(1/window_length)
float hann(double x);

// hamming window
// 'x' is the independent variable of the window function.
// 'x' is actually i/window_length, it's from 0 to 1-(1/window_length)
float hamm(double x);

// FFT shift
// VectorLength must be an even integer
void OneDimensionFFTshift(float vector[],int VectorLength);

#endif
