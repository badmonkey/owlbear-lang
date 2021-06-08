#include <string.h>

void reverse(const char* src, char* dst)
{
    int i, j, len;

    j = 0;
    len = strlen(src);

    for (i = len - 1; i >= 0; i--) {
            dst[j++] = src[i];
        }
    dst[i] = '\0';
}
