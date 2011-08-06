#include <ApplicationServices/ApplicationServices.h>

// c2hs doesn't like the C99 "bool" type
int c_CGFontGetGlyphAdvances(CGFontRef f, CGGlyph *gs, size_t count, int advances[]) {
    return CGFontGetGlyphAdvances(f,gs,count,advances);
}
