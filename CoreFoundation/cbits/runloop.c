#include "runloop.h"

void queueFunctionForRunLoop(CFRunLoopRef r, HsStablePtr f) {
    CFRunLoopPerformBlock(r,kCFRunLoopDefaultMode,
                          ^{ runHSAction(f); }
                         );
}
