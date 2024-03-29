/*=============================================================================================*/
/* Project name: test6br */
/* Project version: aaa */
/*---------------------------------------------------------------------------------------------*/
/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */
/* Aqualis version: aaa */
/* Generated date: 2023/11/25 7:07:26 */
/*=============================================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>
#undef I
#define uj _Complex_I
char byte_tmp;
char t001[100];
FILE* f001;
int i001;
int i002;
int i003;
int i004;
int i005;
int i006;
int i007;
double d001;
double d002;
double d003;
double d004;
double d005;
double complex z001;
double complex z002;
int main()
{
  i001 = 1;
  i002 = 2;
  d001 = 1.234E0;
  z001 = 5.6E0+uj*7.8E0;
  sprintf(t001,"testA%08d_%08d.bin",i001,i002);
  f001 = fopen(t001,"rb");
  fread(&i004,sizeof(i004),1,f001);
  fread(&i005,sizeof(i005),1,f001);
  fread(&i006,sizeof(i006),1,f001);
  printf("%8d%8d\n",i004,i001);
  printf("%8d%8d\n",i005,i001+1);
  printf("%8d%8d\n",i006,i001+2);
  fread(&i004,sizeof(i004),1,f001);
  fread(&i005,sizeof(i005),1,f001);
  fread(&i006,sizeof(i006),1,f001);
  printf("%8d%8d\n",i004,i001+3);
  printf("%8d%8d\n",i005,i001+4);
  printf("%8d%8d\n",i006,i001+5);
  fread(&d002,sizeof(d002),1,f001);
  fread(&d004,sizeof(d004),1,f001);
  fread(&d005,sizeof(d005),1,f001);
  z002 = d004+uj*d005;
  printf("%27.17e%27.17e\n",d001,d002);
  printf("%27.17e%27.17e%27.17e%27.17e\n",creal(z001),cimag(z001),creal(z002),cimag(z002));
  fread(&d002,sizeof(d002),1,f001);
  fread(&d004,sizeof(d004),1,f001);
  fread(&d005,sizeof(d005),1,f001);
  z002 = d004+uj*d005;
  printf("%27.17e%27.17e\n",sin(d001),d002);
  printf("%27.17e%27.17e%27.17e%27.17e\n",creal(cexp(z001)),cimag(cexp(z001)),creal(z002),cimag(z002));
  fread(&d002,sizeof(d002),1,f001);
  printf("%27.17e%27.17e\n",d001+cos(d001),d002);
  fread(&d002,sizeof(d002),1,f001);
  fread(&d003,sizeof(d003),1,f001);
  printf("%27.17e%27.17e\n",fabs(z001),d002);
  printf("%27.17e%27.17e\n",pow(fabs(z001),2),d003);
  fclose(f001);
  return 0;
}
