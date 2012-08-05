/* sliders.h -- support for graphical sliders in Nyquist IDE */

/* probably these 3 should be elsewhere */

#define SLIDERS_MAX 1024
extern float slider_array[SLIDERS_MAX];
void set_slider(int index, float value);

