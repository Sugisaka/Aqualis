/*=============================================================================================*/
/* Project name: test2 */
/* Project version: aaa */
/*---------------------------------------------------------------------------------------------*/
/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */
/* Aqualis version: aaa */
/* Generated date: 2023/10/17 23:13:14 */
/*=============================================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>
#undef I
#define uj _Complex_I
int ic001;
int ic002;
int i001;
int i002;
double d001;
double d002;
double d003;
double complex z001;
double complex z002;
double *d1001;
int d1001_size[1]={-1};
double *d1002;
int d1002_size[1]={-1};
double *d1003;
int d1003_size[1]={-1};
double *d2001;
int d2001_size[2]={-1,-1};
double *d2002;
int d2002_size[2]={-1,-1};
double *d2003;
int d2003_size[2]={-1,-1};
int main()
{
  /* %%%test1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* 001 */
    i002 = 1;
    /* 002 */
    i001 = i002+1;
    /* 003 */
    i001 = i001+i002;
    /* 004 */
    i001 = (i001+i002)*(i001+i002);
    /* 005 */
    d001 = 1;
    /* 006 */
    d002 = d001/d002/d002;
    /* 007 */
    i001 = i001-i002;
    /* 008 */
    d001 = (double)(i001)/(double)(i002);
    /* 009 */
    i001 = i001/i002;
    /* 010 */
    printf("%8d\n",i002);
    printf("%8d\n",i002);
    printf("%8d\n",i002+1);
    /* 011 */
    i002 = -1;
    /* 012 */
    i001 = 1-i002;
    /* 013 */
    i001 = i002-i001;
    /* 014 */
    i001 = -(i001+i002)*(i001+i002);
    /* 015 */
    d001 = (double)(-(i001+i002))/(double)(i001+i002);
    /* 016 */
    d002 = -d001/d002/d002;
    /* 017 */
    i001 = -(i001+i002);
    /* 018 */
    d001 = (double)(-i001)/(double)(i002);
    /* 019 */
    i001 = -i001/i002;
    /* 020 */
    d002 = sin(-(d001+d002)/d001);
    d002 = cos(d001);
    d002 = tan(d001);
    d002 = exp(d001);
    d002 = log(d001);
    d002 = log10(d001);
    d002 = sqrt(d001);
    d002 = asin(d001);
    d002 = acos(d001);
    d002 = atan(d001);
    d002 = atan2(d001,d002);
    d002 = floor(d001);
    d002 = ceil(d001);
  /* %%%end test1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    
  /* %%%test2%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* ***debug array1 allocate check: 2***************************** */
    if(d1001_size[0] != -1)
    {
      printf("ERROR2 array d1001 is already allocated\n");
    }
    /* **************************************************** */
    d1001_size[0] = 10;
    d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
    /* ***debug array1 allocate check: 3***************************** */
    if(d1002_size[0] != -1)
    {
      printf("ERROR3 array d1002 is already allocated\n");
    }
    /* **************************************************** */
    d1002_size[0] = 10;
    d1002=(double *)malloc(sizeof(double)*d1002_size[0]);
    /* ***debug array1 allocate check: 4***************************** */
    if(d1003_size[0] != -1)
    {
      printf("ERROR4 array d1003 is already allocated\n");
    }
    /* **************************************************** */
    d1003_size[0] = 10;
    d1003=(double *)malloc(sizeof(double)*d1003_size[0]);
    /* 021 */
    /* ***debug array1 access check: 5***************************** */
    if(d1001_size[0] != d1002_size[0])
    {
      printf("ERROR5 operator '+' array size mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 6***************************** */
    if(d1003_size[0] != d1001_size[0])
    {
      printf("ERROR6 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=d1003_size[0]; ic001++)
    {
      /* ***debug array1 access check: 7***************************** */
      if(d1003_size[0] == -1)
      {
        printf("ERROR7 array d1003 is not allocated\n");
      }
      else if((ic001 < 1) || (d1003_size[0] < ic001))
      {
        printf("ERROR7 array d1003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1003_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 8***************************** */
      if(d1001_size[0] == -1)
      {
        printf("ERROR8 array d1001 is not allocated\n");
      }
      else if((ic001 < 1) || (d1001_size[0] < ic001))
      {
        printf("ERROR8 array d1001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1001_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 9***************************** */
      if(d1002_size[0] == -1)
      {
        printf("ERROR9 array d1002 is not allocated\n");
      }
      else if((ic001 < 1) || (d1002_size[0] < ic001))
      {
        printf("ERROR9 array d1002 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1002_size[0]);
      }
      /* **************************************************** */
      d1003[ic001-1] = d1001[ic001-1]+d1002[ic001-1];
    }
    /* 022 */
    /* ***debug array1 access check: 10***************************** */
    if(3 != 3)
    {
      printf("ERROR10 operator '+' array size mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 11***************************** */
    if(3 != 3)
    {
      printf("ERROR11 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=3; ic001++)
    {
      /* ***debug array1 access check: 12***************************** */
      if(d1003_size[0] == -1)
      {
        printf("ERROR12 array d1003 is not allocated\n");
      }
      else if((ic001 < 1) || (d1003_size[0] < ic001))
      {
        printf("ERROR12 array d1003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1003_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 13***************************** */
      if(d1001_size[0] == -1)
      {
        printf("ERROR13 array d1001 is not allocated\n");
      }
      else if((ic001+3 < 1) || (d1001_size[0] < ic001+3))
      {
        printf("ERROR13 array d1001 illegal access. index %8d is out of range (1:%8d)\n",ic001+3,d1001_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 14***************************** */
      if(d1002_size[0] == -1)
      {
        printf("ERROR14 array d1002 is not allocated\n");
      }
      else if((ic001+3 < 1) || (d1002_size[0] < ic001+3))
      {
        printf("ERROR14 array d1002 illegal access. index %8d is out of range (1:%8d)\n",ic001+3,d1002_size[0]);
      }
      /* **************************************************** */
      d1003[ic001-1] = d1001[ic001+2]+d1002[ic001+2];
    }
    /* 023 */
    /* ***debug array1 access check: 15***************************** */
    if(d1001_size[0] != d1002_size[0])
    {
      printf("ERROR15 operator '+' array size mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 16***************************** */
    if(d1001_size[0] != d1001_size[0])
    {
      printf("ERROR16 operator '+' array size mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 17***************************** */
    if(d1003_size[0] != d1001_size[0])
    {
      printf("ERROR17 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=d1003_size[0]; ic001++)
    {
      /* ***debug array1 access check: 18***************************** */
      if(d1003_size[0] == -1)
      {
        printf("ERROR18 array d1003 is not allocated\n");
      }
      else if((ic001 < 1) || (d1003_size[0] < ic001))
      {
        printf("ERROR18 array d1003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1003_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 19***************************** */
      if(d1001_size[0] == -1)
      {
        printf("ERROR19 array d1001 is not allocated\n");
      }
      else if((ic001 < 1) || (d1001_size[0] < ic001))
      {
        printf("ERROR19 array d1001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1001_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 20***************************** */
      if(d1001_size[0] == -1)
      {
        printf("ERROR20 array d1001 is not allocated\n");
      }
      else if((ic001 < 1) || (d1001_size[0] < ic001))
      {
        printf("ERROR20 array d1001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1001_size[0]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 21***************************** */
      if(d1002_size[0] == -1)
      {
        printf("ERROR21 array d1002 is not allocated\n");
      }
      else if((ic001 < 1) || (d1002_size[0] < ic001))
      {
        printf("ERROR21 array d1002 illegal access. index %8d is out of range (1:%8d)\n",ic001,d1002_size[0]);
      }
      /* **************************************************** */
      d1003[ic001-1] = d1001[ic001-1]*(d1001[ic001-1]+d1002[ic001-1]);
    }
    /* ***debug array1 deallocate check: 22***************************** */
    if(d1003_size[0] == -1)
    {
      printf("ERROR22 cannot deallocate array d1003\n");
    }
    /* **************************************************** */
    d1003_size[0] = -1;
    free(d1003);
    /* ***debug array1 allocate check: 23***************************** */
    if(d2001_size[0] != -1)
    {
      printf("ERROR23 array d2001 is already allocated\n");
    }
    /* **************************************************** */
    d2001_size[0] = 10;
    d2001_size[1] = 5;
    d2001=(double *)malloc(sizeof(double)*d2001_size[0]*d2001_size[1]);
    /* ***debug array1 allocate check: 24***************************** */
    if(d2002_size[0] != -1)
    {
      printf("ERROR24 array d2002 is already allocated\n");
    }
    /* **************************************************** */
    d2002_size[0] = 10;
    d2002_size[1] = 5;
    d2002=(double *)malloc(sizeof(double)*d2002_size[0]*d2002_size[1]);
    /* ***debug array1 allocate check: 25***************************** */
    if(d2003_size[0] != -1)
    {
      printf("ERROR25 array d2003 is already allocated\n");
    }
    /* **************************************************** */
    d2003_size[0] = 10;
    d2003_size[1] = 5;
    d2003=(double *)malloc(sizeof(double)*d2003_size[0]*d2003_size[1]);
    /* 024 */
    /* ***debug array1 access check: 26***************************** */
    if(d2001_size[0] != d2002_size[0])
    {
      printf("ERROR26 array size1 mismatch\n");
    }
    if(d2001_size[1] != d2002_size[1])
    {
      printf("ERROR26 array size2 mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 27***************************** */
    if(d2003_size[0] != d2001_size[0])
    {
      printf("ERROR27 operator '<==' array size mismatch\n");
    }
    if(d2003_size[1] != d2001_size[1])
    {
      printf("ERROR27 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=d2003_size[0]; ic001++)
    {
      for(ic002=1; ic002<=d2003_size[1]; ic002++)
      {
        /* ***debug array1 access check: 28***************************** */
        if((d2003_size[0] == -1) || (d2003_size[1] == -1))
        {
          printf("ERROR28 array d2003 is not allocated\n");
        }
        else if((ic001 < 1) || (d2003_size[0] < ic001))
        {
          printf("ERROR28 array d2003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2003_size[0]);
        }
        else if((ic002 < 1) || (d2003_size[1] < ic002))
        {
          printf("ERROR28 array d2003 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2003_size[1]);
        }
        /* **************************************************** */
        /* ***debug array1 access check: 29***************************** */
        if((d2001_size[0] == -1) || (d2001_size[1] == -1))
        {
          printf("ERROR29 array d2001 is not allocated\n");
        }
        else if((ic001 < 1) || (d2001_size[0] < ic001))
        {
          printf("ERROR29 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2001_size[0]);
        }
        else if((ic002 < 1) || (d2001_size[1] < ic002))
        {
          printf("ERROR29 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2001_size[1]);
        }
        /* **************************************************** */
        /* ***debug array1 access check: 30***************************** */
        if((d2002_size[0] == -1) || (d2002_size[1] == -1))
        {
          printf("ERROR30 array d2002 is not allocated\n");
        }
        else if((ic001 < 1) || (d2002_size[0] < ic001))
        {
          printf("ERROR30 array d2002 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2002_size[0]);
        }
        else if((ic002 < 1) || (d2002_size[1] < ic002))
        {
          printf("ERROR30 array d2002 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2002_size[1]);
        }
        /* **************************************************** */
        d2003[(ic002-1)*d2003_size[0]+ic001-1] = d2001[(ic002-1)*d2001_size[0]+ic001-1]+d2002[(ic002-1)*d2002_size[0]+ic001-1];
      }
    }
    /* 025 */
    /* ***debug array1 access check: 31***************************** */
    if(3 != 3)
    {
      printf("ERROR31 operator '+' array size mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 32***************************** */
    if(3 != 3)
    {
      printf("ERROR32 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=3; ic001++)
    {
      /* ***debug array1 access check: 33***************************** */
      if((d2003_size[0] == -1) || (d2003_size[1] == -1))
      {
        printf("ERROR33 array d2003 is not allocated\n");
      }
      else if((ic001 < 1) || (d2003_size[0] < ic001))
      {
        printf("ERROR33 array d2003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2003_size[0]);
      }
      else if((3 < 1) || (d2003_size[1] < 3))
      {
        printf("ERROR33 array d2003 illegal access. index %8d is out of range (1:%8d)\n",3,d2003_size[1]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 34***************************** */
      if((d2001_size[0] == -1) || (d2001_size[1] == -1))
      {
        printf("ERROR34 array d2001 is not allocated\n");
      }
      else if((ic001+3 < 1) || (d2001_size[0] < ic001+3))
      {
        printf("ERROR34 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic001+3,d2001_size[0]);
      }
      else if((3 < 1) || (d2001_size[1] < 3))
      {
        printf("ERROR34 array d2001 illegal access. index %8d is out of range (1:%8d)\n",3,d2001_size[1]);
      }
      /* **************************************************** */
      /* ***debug array1 access check: 35***************************** */
      if((d2002_size[0] == -1) || (d2002_size[1] == -1))
      {
        printf("ERROR35 array d2002 is not allocated\n");
      }
      else if((ic001+3 < 1) || (d2002_size[0] < ic001+3))
      {
        printf("ERROR35 array d2002 illegal access. index %8d is out of range (1:%8d)\n",ic001+3,d2002_size[0]);
      }
      else if((3 < 1) || (d2002_size[1] < 3))
      {
        printf("ERROR35 array d2002 illegal access. index %8d is out of range (1:%8d)\n",3,d2002_size[1]);
      }
      /* **************************************************** */
      d2003[2*d2003_size[0]+ic001-1] = d2001[2*d2001_size[0]+ic001+2]+d2002[2*d2002_size[0]+ic001+2];
    }
    /* 026 */
    /* ***debug array1 access check: 36***************************** */
    if(d2001_size[0] != d2002_size[0])
    {
      printf("ERROR36 array size1 mismatch\n");
    }
    if(d2001_size[1] != d2002_size[1])
    {
      printf("ERROR36 array size2 mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 37***************************** */
    if(d2001_size[0] != d2001_size[0])
    {
      printf("ERROR37 array size1 mismatch\n");
    }
    if(d2001_size[1] != d2001_size[1])
    {
      printf("ERROR37 array size2 mismatch\n");
    }
    /* **************************************************** */
    /* ***debug array1 access check: 38***************************** */
    if(d2003_size[0] != d2001_size[0])
    {
      printf("ERROR38 operator '<==' array size mismatch\n");
    }
    if(d2003_size[1] != d2001_size[1])
    {
      printf("ERROR38 operator '<==' array size mismatch\n");
    }
    /* **************************************************** */
    for(ic001=1; ic001<=d2003_size[0]; ic001++)
    {
      for(ic002=1; ic002<=d2003_size[1]; ic002++)
      {
        /* ***debug array1 access check: 39***************************** */
        if((d2003_size[0] == -1) || (d2003_size[1] == -1))
        {
          printf("ERROR39 array d2003 is not allocated\n");
        }
        else if((ic001 < 1) || (d2003_size[0] < ic001))
        {
          printf("ERROR39 array d2003 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2003_size[0]);
        }
        else if((ic002 < 1) || (d2003_size[1] < ic002))
        {
          printf("ERROR39 array d2003 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2003_size[1]);
        }
        /* **************************************************** */
        /* ***debug array1 access check: 40***************************** */
        if((d2001_size[0] == -1) || (d2001_size[1] == -1))
        {
          printf("ERROR40 array d2001 is not allocated\n");
        }
        else if((ic001 < 1) || (d2001_size[0] < ic001))
        {
          printf("ERROR40 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2001_size[0]);
        }
        else if((ic002 < 1) || (d2001_size[1] < ic002))
        {
          printf("ERROR40 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2001_size[1]);
        }
        /* **************************************************** */
        /* ***debug array1 access check: 41***************************** */
        if((d2001_size[0] == -1) || (d2001_size[1] == -1))
        {
          printf("ERROR41 array d2001 is not allocated\n");
        }
        else if((ic001 < 1) || (d2001_size[0] < ic001))
        {
          printf("ERROR41 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2001_size[0]);
        }
        else if((ic002 < 1) || (d2001_size[1] < ic002))
        {
          printf("ERROR41 array d2001 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2001_size[1]);
        }
        /* **************************************************** */
        /* ***debug array1 access check: 42***************************** */
        if((d2002_size[0] == -1) || (d2002_size[1] == -1))
        {
          printf("ERROR42 array d2002 is not allocated\n");
        }
        else if((ic001 < 1) || (d2002_size[0] < ic001))
        {
          printf("ERROR42 array d2002 illegal access. index %8d is out of range (1:%8d)\n",ic001,d2002_size[0]);
        }
        else if((ic002 < 1) || (d2002_size[1] < ic002))
        {
          printf("ERROR42 array d2002 illegal access. index %8d is out of range (1:%8d)\n",ic002,d2002_size[1]);
        }
        /* **************************************************** */
        d2003[(ic002-1)*d2003_size[0]+ic001-1] = d2001[(ic002-1)*d2001_size[0]+ic001-1]*(d2001[(ic002-1)*d2001_size[0]+ic001-1]+d2002[(ic002-1)*d2002_size[0]+ic001-1]);
      }
    }
    /* ***debug array1 deallocate check: 43***************************** */
    if(d2003_size[0] == -1)
    {
      printf("ERROR43 cannot deallocate array d2003\n");
    }
    /* **************************************************** */
    /* 027 */
    i001 = 1;
    /* 028 */
    i001 = 1;
    i002 = 1;
    i002 = 8*i001;
    /* aaa */
    d001 = (double)(4*i001)/2.0E0;
    /* bbb */
    i002 = 2*i001;
    /* ccc */
    /* ***debug array1 deallocate check: 44***************************** */
    if(d2002_size[0] == -1)
    {
      printf("ERROR44 cannot deallocate array d2002\n");
    }
    /* **************************************************** */
    /* ***debug array1 deallocate check: 45***************************** */
    if(d2001_size[0] == -1)
    {
      printf("ERROR45 cannot deallocate array d2001\n");
    }
    /* **************************************************** */
    /* ***debug array1 deallocate check: 46***************************** */
    if(d1002_size[0] == -1)
    {
      printf("ERROR46 cannot deallocate array d1002\n");
    }
    /* **************************************************** */
    d1002_size[0] = -1;
    free(d1002);
    /* ***debug array1 deallocate check: 47***************************** */
    if(d1001_size[0] == -1)
    {
      printf("ERROR47 cannot deallocate array d1001\n");
    }
    /* **************************************************** */
    d1001_size[0] = -1;
    free(d1001);
  /* %%%end test2%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    
  /* %%%test3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* 001 */
    i001 = 2*i001;
    /* 002 */
    i001 = 0;
    /* 003 */
    d001 = 1;
    /* 004 */
    i001 = 1;
    /* 005 */
    d001 = 2*(i001+d001);
    /* 006 */
    d001 = 4*i001;
    /* 007 */
    d001 = 0;
    /* 008 */
    d001 = 0;
    /* 009 */
    d001 = 1;
    /* 010 */
    d001 = 1;
    /* 011 */
    d001 = 1;
  /* %%%end test3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    
  /* %%%test4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    /* 001 */
    i001 = -2;
    d001 = sin((double)(i001));
    printf("%27.17e\n",d001);
    d001 = cos((double)(i001));
    printf("%27.17e\n",d001);
    d001 = tan((double)(i001));
    printf("%27.17e\n",d001);
    d001 = exp((double)(i001));
    printf("%27.17e\n",d001);
    d001 = log((double)(i001));
    printf("%27.17e\n",d001);
    d001 = log10((double)(i001));
    printf("%27.17e\n",d001);
    d001 = sqrt((double)(i001));
    printf("%27.17e\n",d001);
    d001 = asin((double)(i001));
    printf("%27.17e\n",d001);
    d001 = acos((double)(i001));
    printf("%27.17e\n",d001);
    d001 = atan((double)(i001));
    printf("%27.17e\n",d001);
    d001 = atan2((double)(i001),d001);
    printf("%27.17e\n",d001);
    /* 002 */
    d001 = 1.2E0;
    d003 = sin(d001);
    printf("%27.17e\n",d003);
    d003 = cos(d001);
    printf("%27.17e\n",d003);
    d003 = tan(d001);
    printf("%27.17e\n",d003);
    d003 = exp(d001);
    printf("%27.17e\n",d003);
    d003 = log(d001);
    printf("%27.17e\n",d003);
    d003 = log10(d001);
    printf("%27.17e\n",d003);
    d003 = sqrt(d001);
    printf("%27.17e\n",d003);
    d003 = asin(d001);
    printf("%27.17e\n",d003);
    d003 = acos(d001);
    printf("%27.17e\n",d003);
    d003 = atan(d001);
    printf("%27.17e\n",d003);
    d003 = atan2(d001,d002);
    printf("%27.17e\n",d003);
    d003 = floor(d001);
    printf("%27.17e\n",d003);
    d003 = ceil(d001);
    printf("%27.17e\n",d003);
    /* 003 */
    z001 = 1.0E0-uj*2.0E0;
    z002 = csin(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = ccos(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = ctan(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = cexp(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = clog(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = csqrt(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = casin(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = cacos(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
    z002 = catan(z001);
    printf("%27.17e%27.17e\n",creal(z002),cimag(z002));
  /* %%%end test4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    
  return 0;
}
