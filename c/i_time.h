/*******************************************************
 * File: 
 *      i_time.h
 * Description:
 *      time calculation methods
 * Author: 
 *      w.x
 * History:
 *      2013.08.01 file created
 * 
 *
*******************************************************/


#ifndef _I_TIME_H
#define _I_TIME_H

#include <time.h>
#include <limits.h>

#define DATE_TYPE_YEAR  0
#define DATE_TYPE_MON   1
#define DATE_TYPE_DAY   2
#define DATE_TYPE_HOUR  3
#define DATE_TYPE_MIN   4
#define DATE_TYPE_SEC   5

#define FMT_DATE        "%Y%m%d"
#define FMT_TIME        "%Y-%m-%d %H:%M:%S"

#define GMT_TO_CST      8

#ifndef INT_MAX
#define INT_MAX         2147483647
#endif

#define DIFF_ERROR      INT_MAX

#define SEC_PER_YEAR    (365*24*3600)
#define MIN_PER_YEAR    (365*24*60)

/* years of max diff seconds */
#define SEC_MAX_YEAR    (INT_MAX/SEC_PER_YEAR - 1)
/* years of max diff minutes */
#define MIN_MAX_YEAR    (INT_MAX/MIN_PER_YEAR - 1)


#define TIME_LEN        64

/* check if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is). */
#define leapyear(year) \
    ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))

/* How many days int each month (0-11). */
static const unsigned short int _mon_mday[2][12] = 
    {
        /* Normal years.  */
        { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
        /* Leap years.  */
        { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
    };


int diff_tm(const struct tm *tm2, const struct tm *tm1, int dt_type);

struct tm *modi_tm(struct tm *tm2, const struct tm *tm1, int dt_off, int dt_type);

void copy_tm(struct tm *tm2, const struct tm *tm1);

int diff_time(const char *t2, const char *t1, const char *fmt, int dt_type);
int diff_time2(const char *t2, const char *fmt2, const char *t1, const char *fmt1, int dt_type);

char *modi_time(char *t2, char *t1, const char *fmt, int dt_off, int dt_type);
char *modi_time2(char *t2, const char *fmt2, const char *t1, const char *fmt1, int dt_off, int dt_type);

char *get_date(char *date);
char *get_datetime(char *datetime);
char *get_datefmt(char *datetime, char *fmt);

#endif

