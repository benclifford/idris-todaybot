
#include "ffitest.h"
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

char *foo(char *str, void (*callback)(char *)) {
 printf("in C file of BENC ffi test (v2)\n");
 printf("String is >>>%s<<<\n", str);

 printf("function pointer is %p\n", callback);

 printf("About to call function pointer\n");
 (*callback)("CALLBODY1");
 (*callback)(str);
 printf("Function pointer call returned\n");

 return "HIBACK";

}

int ft_curl_global_init(int v) {
  return curl_global_init(v);
}

/* TODO: should be size_t not int when size_t is
detected properly in idris */

void *alloc_bytes(int count) {
  void *ptr = malloc(count);
  printf("c-side: alloc_bytes: %d bytes, ptr = %p\n", count, ptr);
  return malloc(count);
}

void poke_byte(void *base, int offset, int value) {
  char *b = base + offset;
  *b = value;
}

void poke_ptr(void **base, void *value) {
  *base = value;
}

void dump_buffer(char **buffer) {
  printf("buffer: %s\n", *buffer);
}
