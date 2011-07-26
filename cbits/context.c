#include <ApplicationServices/ApplicationServices.h>

void c_CGContextFillRect(CGContextRef c, CGRect *r) {
    CGContextFillRect(c,*r);
}

void c_CGContextSetTextMatrix(CGContextRef c, CGAffineTransform *t) {
    CGContextSetTextMatrix(c,*t);
}
