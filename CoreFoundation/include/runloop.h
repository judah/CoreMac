#include <CoreFoundation/CoreFoundation.h>
#include "HsFFI.h"

void queueFunctionForRunLoop(CFRunLoopRef r, HsStablePtr f);
