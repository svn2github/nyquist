#include "instr.h"
#include "Instrmnt.h"
#include "Clarinet.h"
#include "Saxofony.h"
#include "stdlib.h"
#include "string.h"

/* C interface to Instrmnt */

struct instr {
	Instrmnt *instrObjPtr;
};


struct instr *initInstrument(int instr_type, int sample_rate) {
	struct instr *in = (struct instr *) malloc(sizeof(struct instr));
        Stk::setSampleRate(sample_rate);
	switch(instr_type) {
		case CLARINET: 
			in->instrObjPtr = new Clarinet(10.0);
			break;
		case SAXOFONY:
			in->instrObjPtr = new Saxofony(10.0);
			break;
		default:
			return NULL;
	}
	return in;
}

int deleteInstrument(struct instr* in) {
	delete(in->instrObjPtr);
	free(in);
	return 0;
}

//! Start a note with the given frequency and amplitude.
int noteOn(struct instr* in, MY_FLOAT frequency, MY_FLOAT amplitude) {
	in->instrObjPtr->noteOn(frequency, amplitude);
	return 0;
}

//! Stop a note with the given amplitude (speed of decay).
int noteOff(struct instr* in, MY_FLOAT amplitude) {
	in->instrObjPtr->noteOff(amplitude);
	return 0;
}

//! Set instrument parameters for a particular frequency.
int setFrequency(struct instr* in, MY_FLOAT frequency) {
	in->instrObjPtr->setFrequency(frequency);
	return 0;
}

//! Return the last output value.
/*
MY_FLOAT lastOut(struct instr* in) {
	return in->instrObjPtr->lastOut();
}
*/

//! Compute one output sample.
MY_FLOAT tick(struct instr* in) {
	return in->instrObjPtr->tick();
}

//! Computer \e vectorSize outputs and return them in \e vector.
MY_FLOAT *multTicks(struct instr* in, MY_FLOAT *vector, unsigned int vectorSize) {
	return in->instrObjPtr->tick(vector, vectorSize);
}

//! Perform the control change specified by \e number and \e value (0.0 - 128.0).
int controlChange(struct instr* in, int number, MY_FLOAT value) {
	in->instrObjPtr->controlChange(number, value);
	return 0;
}

