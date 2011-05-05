#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <gmp.h>
unsigned int magi[64],buffer[64],number2[64],number[64];


static void next_test(unsigned int n,unsigned int b)
{
  unsigned int buff,c=1,ndiv2=(n%2==1) ? n/2 : n/2-1;
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
    number[i]=buffer[i];
    if(buffer[i+1]<buffer[i])
    {
      buffer[i+1]=buffer[i];
    }
  }
  number[n-1]=buffer[n-1];
  if(buffer[ndiv2]==1)
  {
    printf("0\n");
    exit(0);
  }
}
static void print_number(unsigned int n,unsigned int b)
{
  mpz_t bigint,rop;
  mpz_init(bigint);
  mpz_init(rop);
  unsigned long int i;
  for(i=0;i<n;i++)
  {
      mpz_ui_pow_ui(rop,b,i);
      mpz_mul_ui(rop,rop,number[n-i-1]);
      mpz_add(bigint,bigint,rop); 
  }
  gmp_printf("%Zd\n",bigint);
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
  int i;
  for(i = (int) n ; i >= 0;i--)
  {
    number2[n-i-1] = number[i];
    number[i] = b-1-number[i];
  }
}


static int compare (const void *a,const void *b)
{
  return ( *(int*)a - *(int*)b );
}
static int ismagic(unsigned int n,unsigned int b)
{
  unsigned int i;
  complementB(n,b);
  addComplement(n,b);
  for(i=0;i<n;i++)
  {
    magi[i]=number[i];
  }
  qsort(number,n,sizeof(unsigned int),compare);
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
  if (argc != 3) 
  {
    printf("Usage: magic b n\n"); 
    return 1;
  }
  sscanf(argv[1], "%u", &b);
  sscanf(argv[2], "%u", &n);
  if(magic(n,b)==0)
  {
   print_number(n,b);
  }
  return 0;
}
