#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

unsigned int magi[64],buffer[64],number2[64],number[64];


static void next_test(unsigned int n,unsigned int b)
{
  unsigned int buff,c=1,ndiv2;
  if (n%2==1)
  {
    ndiv2=n/2+1;
  }
  else
  {
    ndiv2=n/2;
  }
/*
  if(n%2==1) 
  {
    c=b*b-1;
  }
  else
  {
    c=b-1;
  }
  */
  int i = (int) n-1;
  for (;i>=0;i--)
  {
    buff=buffer[i]+c;
    buffer[i]=buff%b;
    c=buff/b;
    if (c==0) break;
  }
  for(i=0;i<(int)n-1;i++)
  {
    if(buffer[i+1]<buffer[i])
    {
      buffer[i+1]=buffer[i];
    }
      }
  if(buffer[ndiv2]==1)
  {
    printf("0\n");
    exit(0);
  }
}

static void addComplement(unsigned int n,unsigned int b)
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
/*
static void  extra_test( int n, int b)
{
  int i,diff1,diff2,ndiv2=(n/2)-1;
  for (i=0;i<ndiv2;i++)
  {
    //    printf("number[i] %u\n",number[i]);
    diff1=buffer[n-i-1]-buffer[i];
    diff2=buffer[n-i-2]-buffer[i+1];
    if(diff1<diff2)
    {
      printf("failed extra test\n");
      //  printf("%u - %u %u - %u\n\n",number[n-i-1],number[i],number[n-i-2],number[i+1]);
      next_test(n,b);
      extra_test(n,b);
    }
  }
}
*/
static int ismagic(unsigned int n,unsigned int b)
{
  unsigned int i;
//  extra_test(n,b);
  for(i=0;i<n;i++)
  {
    number[i]=buffer[i];
  }
  for(i=0;i<n;i++)
  {
//    printf("%u ",number[i]);
    number2[i]=number[n-i-1];
  }
  complementB(n,b);
  addComplement(n,b);
  for(i=0;i<n;i++)
  {
    magi[i]=number[i];
  }
  qsort(number,n,sizeof(unsigned int),compare);
/*  if (extra_test(n,b)==1) 
  {
    return 0;
  }*/
  for(i=0;i<n;i++)
  {
//    printf("%u ",number[i]);
    number2[i]=number[n-i-1];
  }
  complementB(n,b);
  addComplement(n,b);
  for (i=0;i<n;i++)
  {
    if(magi[i]!=number[i]) 
    {
      return 0;
    }
  }
  return 1;
}

static unsigned int magic(unsigned int n, unsigned int b)
{
  next_test(n,b);
  while(ismagic(n,b)==0)
  {
      next_test(n,b);
  }
   return 0;
}
int main(int argc, char* argv[])
{
  unsigned int b;
  unsigned int n;
  unsigned int i;
  //unsigned long long int answer=0;
  if (argc != 3) 
  {
    printf("Usage: magic b n\n"); 
    return 1;
  }
  sscanf(argv[1], "%u", &b);
  sscanf(argv[2], "%u", &n);
  if(magic(n,b)==0)
  {
    for(i=0;i<n;i++)
    {
      printf("%u ",number[i]);
      //answer+=number[i]*pow(b,n-i-1);
    }
    //printf("\n%llu",answer);
  }
  else printf("DAMN\n");
  //printf("\n", magic(n,b));
  printf("\n");
  return 0;
}
