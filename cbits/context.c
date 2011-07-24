#include <ApplicationServices/ApplicationServices.h>

void c_CGContextFillRect(CGContextRef c, CGFloat originX, CGFloat originY, 
                        CGFloat sizeWidth, CGFloat sizeHeight) {
    CGRect r;
    r.origin.x = originX;
    r.origin.y = originY;
    r.size.width = sizeWidth;
    r.size.height = sizeHeight;
    CGContextFillRect(c,r);
}
