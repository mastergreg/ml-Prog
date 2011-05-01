#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

unsigned int buffer[64],*bff,*num2,number2[64],*num,number[64];


static unsigned long long int diffn(unsigned int n,unsigned int b)
{
  unsigned int i;
  unsigned int diff=0;
  unsigned int d=n/2;
  for (i=1;i<=d;i++)
  {
    diff+=((unsigned int)((pow(b,n-1-i-i)-1)*pow(b,i)))*(number[i]-number[n-i-1]);
  }
  diff+=((unsigned int)(pow(b,n-1)-1))*(number[0]-number[n-1]);
  return diff;
}

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
/*
static void addB(unsigned int n,unsigned int b)
{
  unsigned int buff,c=1;
  int i = (int) n-1;
  num=&number[i];
  num2=&number2[i];
  for (;i>=0;i--)
  {
    buff=*num+*num2+c;
    *num=buff%b;
    c=buff/b;
    num--;
    num2--;
  }
}
static void complementB(unsigned int n,unsigned int b)
{
  unsigned int i;
  num=number;
  for(i=0;i<n;i++)
  {
    *num=b-1-*num;
    num++;
  }
}

*/
static int compare (const void *a,const void *b)
{
  return ( *(int*)b - *(int*)a );
}

static int ismagic(unsigned int n,unsigned int b)
{
  unsigned int i;
  unsigned long long int diffbuff,diff=0;
  num=number;
  bff=buffer;
  for(i=0;i<n;i++)
  {
    *num=*bff;
    num++;
    bff++;
  }
  qsort(number,n,sizeof(unsigned int),compare);
  
  diff=diffn(n,b);
  diffbuff=diff;
  //printf("%u\n",diff);
  //printf("\n%u\n",diff);
  for(i=n;i>0;i--)
  {
    //printf("%u ",buffer[i-1]);
    if (buffer[i-1]!=diff%b)
    {
      return 0;
    }
    else
    {
      diff/=b;
    }
  }
/*
  for(i=0;i<n;i++)
  {
    number2[i]=number[n-i-1];
  }
  complementB(n,b);
  addB(n,b);
  for (i=0;i<n;i++)
  {
    if(buffer[i]!=number[i]) 
    {
      return 0;
    }
  }
  */
  printf("Diffbuff: %llu\n",diffbuff);
  return 1;
}

static unsigned int magic(unsigned int n, unsigned int b)
{
  addB_1(n,b);
  while(ismagic(n,b)==0)
  {
      addB_1(n,b);
  }
   return 0;
}
int main(int argc, char* argv[])
{
  unsigned int b;
  unsigned int n;
  unsigned int i;
  unsigned int answer=0;
  if (argc != 3) {
    printf("Usage: magic b n\n"); return 1;
  }
  sscanf(argv[1], "%u", &b);
  sscanf(argv[2], "%u", &n);
  if(magic(n,b)==0)
  {
    for(i=0;i<n;i++)
    {
      printf("%u ",buffer[i]);
      answer+=buffer[i]*pow(b,n-i-1);
    }
    printf("\n%u",answer);
  }
  else printf("DAMN\n");
  //printf("\n", magic(n,b));
  printf("\n");
  return 0;
}
