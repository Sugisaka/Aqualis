/*=============================================================================================*/
/* Project name: test6tr */
/* Project version: aaa */
/*---------------------------------------------------------------------------------------------*/
/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */
/* Aqualis version: aaa */
/* Generated date: 2023/10/17 23:15:13 */
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
double complex z001;
double complex z002;
double *d1001;
int d1001_size[1]={-1};
int main()
{
  i001 = 1;
  i002 = 2;
  d001 = 1.234E0;
  z001 = 5.6E0+uj*7.8E0;
  sprintf(t001,"testA%08d_%08d.dat",i001,i002);
  f001 = fopen(t001,"r");
  d1001_size[0] = 0;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%d%d%d",&i004,&i005,&i006);
  d1001_size[0] = -1;
  free(d1001);
  printf("%8d%8d\n",i004,i001);
  printf("%8d%8d\n",i005,i001+1);
  printf("%8d%8d\n",i006,i001+2);
  d1001_size[0] = 0;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%d%d%d",&i004,&i005,&i006);
  d1001_size[0] = -1;
  free(d1001);
  printf("%8d%8d\n",i004,i001+3);
  printf("%8d%8d\n",i005,i001+4);
  printf("%8d%8d\n",i006,i001+5);
  d1001_size[0] = 2;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%lf%lf%lf",&d002,&d1001[0],&d1001[1]);
  z002 = d1001[0]+uj*d1001[1];
  d1001_size[0] = -1;
  free(d1001);
  printf("%27.17e%27.17e\n",d001,d002);
  printf("%27.17e%27.17e%27.17e%27.17e\n",creal(z001),cimag(z001),creal(z002),cimag(z002));
  d1001_size[0] = 2;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%lf%lf%lf",&d002,&d1001[0],&d1001[1]);
  z002 = d1001[0]+uj*d1001[1];
  d1001_size[0] = -1;
  free(d1001);
  printf("%27.17e%27.17e\n",sin(d001),d002);
  printf("%27.17e%27.17e%27.17e%27.17e\n",creal(cexp(z001)),cimag(cexp(z001)),creal(z002),cimag(z002));
  d1001_size[0] = 0;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%lf",&d002);
  d1001_size[0] = -1;
  free(d1001);
  printf("%27.17e%27.17e\n",d001+cos(d001),d002);
  d1001_size[0] = 0;
  d1001=(double *)malloc(sizeof(double)*d1001_size[0]);
  fscanf(f001,"%lf%lf",&d002,&d003);
  d1001_size[0] = -1;
  free(d1001);
  printf("%27.17e%27.17e\n",cabs(z001),d002);
  printf("%27.17e%27.17e\n",pow(cabs(z001),2),d003);
  fclose(f001);
  return 0;
}
