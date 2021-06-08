#include <string.h>

void reverse(const char* src, char* dst)
{
    dst += strlen(src);

    while (src < dst) {
        *dst-- = *src++;
    }
}
