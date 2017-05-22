#include <time.h>

char *foo(char *str, void (*callback)(char *));

int ft_curl_global_init(int);

void *alloc_bytes(int count);

void poke_byte(void *base, int offset, int value);

void poke_long(long *base, long value);

void poke_ptr(void **base, void *value);
void *peek_ptr(void **base);

void dump_buffer(char **buffer);

char *cast_to_string_helper(char **buffer);

void *get_null_pointer();

void *add_ptr_offset(void *base, int offset);

int get_tm_year(struct tm *t);
int get_tm_mon(struct tm *t);
int get_tm_mday(struct tm *t);

void tzinfo_benc();

struct tm *localtime_r_benc(const time_t *timep, struct tm *result);

