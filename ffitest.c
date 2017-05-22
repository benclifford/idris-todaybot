
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
  printf("c-side: alloc_bytes: allocated %d bytes at address %p\n", count, ptr);
  return ptr;
}

void poke_byte(void *base, int offset, int value) {
  char *b = base + offset;
  *b = value;
}

void poke_long(long *base, long value) {
  printf("c: poke_int poking %ld into address %p\n", value, base);
  *base = value;
}

void poke_ptr(void **base, void *value) {
  *base = value;
}

void *peek_ptr(void **base) {
  return *base;
}

void dump_buffer(char **buffer) {
  printf("c-side: dump_buffer: %s\n", *buffer);
}

char *cast_to_string_helper(char **buffer) {
  return *buffer;
}

void *get_null_pointer() {
  return NULL;
}

void *add_ptr_offset(void *base, int offset) {
  return base+offset;
}

int get_tm_year(struct tm *t) {
  printf("c: get_tm_year: tm_year is %d\n", t->tm_year);
  return t->tm_year;
}

int get_tm_mon(struct tm *t) {
  return t->tm_mon;
}

int get_tm_mday(struct tm *t) {
  return t->tm_mday;
}

void tzinfo_benc() {
  printf("tzinfo benc\n");
/*
       extern char *tzname[2];
       extern long timezone;
       extern int daylight;
*/
  printf("timezone = %ld\n", timezone);
  printf("daylight = %d\n", daylight);
  printf("tzname[0] = %p %s\n", tzname[0], tzname[0]);
  printf("tzname[1] = %p %s\n", tzname[1], tzname[1]);

  printf("tzinfo benc done\n");
}

struct tm *localtime_r_benc(const time_t *timep, struct tm *result) {
  printf("BENC localtime_r wrapper\n");
  printf("BENC: timep = %p\n", timep);
  printf("BENC: result = %p\n", result);
  return localtime_r(timep, result);
}

