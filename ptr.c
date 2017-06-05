
#include "ptr.h"
#include <stdio.h>
#include <stdlib.h>

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

