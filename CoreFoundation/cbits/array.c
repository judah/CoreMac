#include "array.h"

void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const void **values) {
  CFArrayGetValues(theArray, CFRangeMake(0, len), values);
}

