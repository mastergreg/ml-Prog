#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "convert.h"

unsigned int buffer[64],number2[64],number[64];


static void addB_1(unsigned int n,unsigned int b)
{
  unsigned int buff,c;
  if(n%2==1) 
  {
    c=b*b-1;
  }
  else
  {
    c=b-1;
  }
  int i = (int) n-1;
  for (;i>=0;i--)
  {
    buff=buffer[i]+c;
    buffer[i]=buff%b;
    c=buff/b;
    if (c==0) break;
  }
}

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
  unsigned int i;
  //unsigned int k = 0,i;
  // First digit must be non-zero:
  /*  srand(time(NULL));
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
      */ for (i=0;i<n;i++)
  {
    number[i]=(i*i)%b;
  }
  for (i=n;i<64;i++)
  {
    number[i]=0;
  }

}


static int ismagic(unsigned int n,unsigned int b)
{
  unsigned int i;
  for(i=0;i<n;i++)
  {
    number[i]=buffer[i];
    number2[i]=number[i];
  }
  qsort(number,n,sizeof(unsigned int),compare);
  qsort(number2,n,sizeof(unsigned int),compare2);
  complementB(n,b);
  addB(n,b);
  for (i=0;i<n;i++)
  {
    if(buffer[i]!=number[i]) 
    {
      addB_1(n,b);
      return 0;
    }
  }
  return 1;
}

static unsigned int magic(unsigned int n, unsigned int b)
{
  unsigned int i;
  //bigrandom_X(n,b);
  addB_1(n,b);
  while(ismagic(n,b)==0)
  {
  }
    /*
    printf("\n");
          for(i=0;i<n;i++)
          {
          printf("%u",number[i]);
          }
  printf("\nSame one\n");*/
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
  sscanf(argv[1], "%u", &b);
  sscanf(argv[2], "%u", &n);
  if(magic(n,b)==0)
  {
    for(i=0;i<n;i++)
    {
      printf("%u",number[i]);
      answer+=number[i]*pow(b,n-i-1);
    }
    printf("\n%u",answer);
  }
  else printf("DAMN\n");
  //printf("\n", magic(n,b));
  printf("\n");
  return 0;
}
