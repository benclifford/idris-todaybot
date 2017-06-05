#include <time.h>

char *foo(char *str, void (*callback)(char *));

int ft_curl_global_init(int);

int get_tm_year(struct tm *t);
int get_tm_mon(struct tm *t);
int get_tm_mday(struct tm *t);

void tzinfo_benc();

struct tm *localtime_r_benc(const time_t *timep, struct tm *result);

