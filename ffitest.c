
#include "ffitest.h"
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int ft_curl_global_init(int v) {
  return curl_global_init(v);
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

