#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "convert.h"

unsigned int buffer[64],number2[64],number[64];


static void addB(unsigned int n,unsigned int b)
{
  unsigned int buff,c=1;
  int i = (int) n-1;
  for (;i>=0;i--)
  {
    buff=number[i]+number2[i]+c;
    number[i]=buff%b;
    c=buff/b;
  }
}
static void complementB(unsigned int n,unsigned int b)
{
  unsigned int i;
  for(i=0;i<n;i++)
  {
    number[i]=b-1-number[i];
  }
}


static int compare (const void *a,const void *b)
{
  return ( *(int*)a - *(int*)b );
}
static int compare2 (const void *a,const void *b)
{
  return ( *(int*)b - *(int*)a );
}
static void bigrandom_X(unsigned int n,unsigned int b)
{
  unsigned int k = 0,i;
  // First digit must be non-zero:
  srand(time(NULL));
  do
  {
    k = rand() % b;
  } while(k == 0);

  number[0]=k;
  for(i = 1; i < n; i++)
  {
    k = rand() % b;
    number[i]=k;
  }
  for (;i<64;i++)
  {
    number[i]=0;
  }
}


static unsigned int magic(unsigned int n, unsigned int b)
{
  unsigned int i;
  //, random_start;
  //random_start=bigrandom_Base_X(n,b);
  bigrandom_X(n,b);
  /*  for(i=0;i<64;i++)
      {
      printf("%u ",number[i]);
      }
      */
  while(1)
  {
    //printf("UnSorted:\n");
    for (i=0;i<n;i++)
    {
      if(buffer[i]!=number[i]) break;
      else return 1;
    }
    for(i=0;i<64;i++)
    {
      buffer[i]=number[i];
      //printf("%u ",number[i]);
    }
    qsort(number,n,sizeof(unsigned int),compare);
    //printf("\nSorted:\n");

    for(i=0;i<64;i++)
    {
      buffer[i]=number[i];
      number2[i]=number[i];
      //      printf("%u ",number[i]);
    }
    qsort(number2,n,sizeof(unsigned int),compare2);
    //printf("\nSecond\n");
    /*
       for(i=0;i<64;i++)
       {
    //    number[i]=number[i]-number2[i];
    printf("%u ",number2[i]);
    }
    printf("\nComplement base %u\n",b-1);
    */
    complementB(n,b);
    /*    for(i=0;i<n;i++)
          {
          printf("%u ",number[i]);
          }
          */
    addB(n,b);
    /*        printf("\nResult of sub:\n");
              for(i=0;i<64;i++)
              {
              printf("%u ",number[i]);
              }
              printf("\n");
              */
  }
  printf("\nSame one\n");
  return 0;
}
int main(int argc, char* argv[])
{
  unsigned int b;
  unsigned int n;
  unsigned int i;
  unsigned int answer=0;
  if (argc != 3) {
    printf("Usage: magic n b\n"); return 1;
  }
  sscanf(argv[1], "%u", &n);
  sscanf(argv[2], "%u", &b);
  if(magic(n,b)==1)
  {
  for(i=0;i<n;i++)
  {
    answer+=number[i]*pow(b,n-i-1);
  }
  printf("%u",answer);
  }
  else printf("DAMN\n");
  //printf("\n", magic(n,b));
  printf("\n");
  return 0;
}