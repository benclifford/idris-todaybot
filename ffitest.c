
#include "ffitest.h"
#include <stdio.h>
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

