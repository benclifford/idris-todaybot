char *foo(char *str, void (*callback)(char *));

int ft_curl_global_init(int);

void *alloc_bytes(int count);

void poke_byte(void *base, int offset, int value);

void poke_ptr(void **base, void *value);

void dump_buffer(char **buffer);

char *cast_to_string_helper(char **buffer);

