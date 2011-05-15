#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gmp.h>
#include <stdint.h>
uint32_t buffer[64],number[64],magi[64],number2[64];
unsigned int ndiv2;

static void next_test(unsigned int n,unsigned int b)
{
  unsigned int i = n-1;
  for (;i>=ndiv2;i--)
  {
    buffer[i]++;
    if (i==ndiv2) 
    {  
      printf("0\n");
      exit(0);
    }
    if (buffer[i]<b)
    {
      break;
    }
    else
    {
      buffer[i]=0;
    }
    
  }
  for(;i<n;i++)
  {
    if(buffer[i]<buffer[i-1])
    {
      buffer[i]=buffer[i-1];
    }
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
      mpz_mul_ui(rop,rop,number2[n-i-1]);
      mpz_add(bigint,bigint,rop); 
  }
  gmp_printf("%Zd\n",bigint);
}
static void subtractBuffer(unsigned int n,unsigned int b)
{
  int buff,c=0;
  int i;
  //memcpy(number2,buffer,n*4);
  for (i = (int) n-1;i>=0;i--)
  {
    buff=buffer[n-i-1]-buffer[i]-c;
    if(buff<0) 
    {
      number[i]=buff+b;
      c=1;
    }
    else
    {
      number[i]=buff;
      c=0;
    }
  }
  memcpy(magi,number,n*4);
}

static void subtract(unsigned int n,unsigned int b)
{
  int buff,c=0;
  int i;
  //memcmp(number2,number,n*4);
  for (i = (int) n-1;i>=0;i--)
  {
    buff=number[n-i-1]-number[i]-c;
    if(buff<0) 
    {
      number2[i]=buff+b;
      c=1;
    }
    else
    {
      number2[i]=buff;
      c=0;
    }
  }
}

static int compare (const void *a,const void *b)
{
  return ( *(int*)a - *(int*)b );
}

static unsigned int magic(unsigned int n, unsigned int b)
{
  ndiv2=(n%2==1) ? n/2 : n/2-1;
  do
  {
    next_test(n,b);
    subtractBuffer(n,b);
    qsort(number,n,4,compare);
    subtract(n,b);
  }
  while(memcmp(magi,number2,n*4)!=0);
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
  magic(n,b);
  print_number(n,b);
  return 0;
}
