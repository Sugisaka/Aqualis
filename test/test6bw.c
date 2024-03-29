/*=============================================================================================*/
/* Project name: test6bw */
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
double d001;
double d002;
double d003;
double complex z001;
int main()
{
  i001 = 1;
  i002 = 2;
  d001 = 1.234E0;
  z001 = 5.6E0+uj*7.8E0;
  sprintf(t001,"testA%08d_%08d.bin",i001,i002);
  f001 = fopen(t001,"wb");
  i003 = i001;
  fwrite(&i003,sizeof(i003),1,f001);
  i003 = i001+1;
  fwrite(&i003,sizeof(i003),1,f001);
  i003 = i001+2;
  fwrite(&i003,sizeof(i003),1,f001);
  i003 = i001+3;
  fwrite(&i003,sizeof(i003),1,f001);
  i003 = i001+4;
  fwrite(&i003,sizeof(i003),1,f001);
  i003 = i001+5;
  fwrite(&i003,sizeof(i003),1,f001);
  d002 = d001;
  fwrite(&d002,sizeof(d002),1,f001);
  d002 = creal(z001);
  d003 = cimag(z001);
  fwrite(&d002,sizeof(d002),1,f001);
  fwrite(&d003,sizeof(d003),1,f001);
  d002 = sin(d001);
  fwrite(&d002,sizeof(d002),1,f001);
  d002 = creal(cexp(z001));
  d003 = cimag(cexp(z001));
  fwrite(&d002,sizeof(d002),1,f001);
  fwrite(&d003,sizeof(d003),1,f001);
  d002 = d001+cos(d001);
  fwrite(&d002,sizeof(d002),1,f001);
  d002 = fabs(z001);
  fwrite(&d002,sizeof(d002),1,f001);
  d002 = pow(fabs(z001),2);
  fwrite(&d002,sizeof(d002),1,f001);
  fclose(f001);
  return 0;
}
