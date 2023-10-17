/*=============================================================================================*/
/* Project name: test3_1 */
/* Project version: aaa */
/*---------------------------------------------------------------------------------------------*/
/* Generated by Aqualis (algorithm and equation analyzer for lightwave simulation) */
/* Aqualis version: aaa */
/* Generated date: 2023/10/17 23:13:35 */
/*=============================================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>
#undef I
#define uj _Complex_I
typedef struct _testClass1
{
  int x1;
  double y1;
  double complex x2;
} testClass1;
int *xx;
int xx_size[1]={ -1 };
testClass1 *d;
int d_size[1]={ -1 };
testClass1 c;
int ic001;
int *i1001;
int i1001_size[1]={-1};
int main()
{
  c.x1 = 1;
  c.y1 = 2.0E0;
  c.x2 = 3.0E0+uj*4.0E0;
  printf("%8d\n",c.x1);
  printf("%27.17e\n",c.y1);
  printf("%27.17e%27.17e\n",creal(c.x2),cimag(c.x2));
  d_size[0] = 4;
  d=(testClass1 *)malloc(sizeof(testClass1)*d_size[0]);
  xx_size[0] = 8;
  xx=(int *)malloc(sizeof(int)*xx_size[0]);
  for(ic001=1; ic001<=d_size[0]; ic001++)
  {
    d[ic001-1].x1 = 1;
    d[ic001-1].y1 = 2.0E0;
    d[ic001-1].x2 = 3.0E0+uj*4.0E0;
  }
  i1001_size[0] = 10;
  i1001=(int *)malloc(sizeof(int)*i1001_size[0]);
  i1001[0] = 1;
  i1001[1] = 2;
  i1001[2] = 3;
  i1001[3] = 4;
  i1001_size[0] = -1;
  free(i1001);
  i1001_size[0] = 20;
  i1001=(int *)malloc(sizeof(int)*i1001_size[0]);
  i1001[0] = 1;
  i1001[1] = 2;
  i1001[2] = 3;
  i1001[3] = 4;
  i1001_size[0] = -1;
  free(i1001);
  return 0;
}