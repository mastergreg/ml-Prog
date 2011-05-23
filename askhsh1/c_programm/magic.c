#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <gmp.h>
#include <stdint.h>
uint32_t buffer[64],number[64],magi[64],number2[64];
int count_sort_array[20];
unsigned int ndiv2;

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

static unsigned int magic(unsigned int n, unsigned int b)
{
  ndiv2=(n%2==1) ? n/2 : n/2-1;
  unsigned int i;
  int buff,c=0;
  int ii,j;
  unsigned int bdiv2=b/2+2;
  do
  {
    //next_test(n,b);
      for (i=n-1;i>=ndiv2;i--)
      {
        buffer[i]++;
        if (i==ndiv2) 
        {  
          printf("0\n");
          exit(0);
        }
        if (buffer[i]<bdiv2)
        {
          break;
        }
        else
        {
          buffer[i]-=bdiv2;
        }
        
      }
      for(;i<n;i++)
      {
        if(buffer[i]<buffer[i-1])
        {
          buffer[i]=buffer[i-1];
        }
      }
      
    //subtractBuffer(n,b);
      //for (j=(int)b-1;j>=0;j--) count_sort_array[j]=0;
      for (ii = (int) n-1;ii>=0;ii--)
      {
        buff=buffer[n-ii-1]-buffer[ii]-c;
        if(buff<0) 
        {
          number[ii]=buff+b;
          c=1;
        }
        else
        {
          number[ii]=buff;
          c=0;
        }
        count_sort_array[number[ii]]++;
      }
      memcpy(magi,number,n*4);
      //distribution sort

      for (ii=(int) n-1,j=(int) b-1;j>=0;j--)
      {
        while(count_sort_array[j]>0)
        {
          number[ii]=j;
          ii--;
          count_sort_array[j]--;
        }
      }
    //qsort(number,n,4,compare);
    //subtract(n,b);
      c=0;
      for (ii = (int) n-1;ii>=0;ii--)
      {
        buff=number[n-ii-1]-number[ii]-c;
        if(buff<0) 
        {
          number2[ii]=buff+b;
          c=1;
        }
        else
        {
          number2[ii]=buff;
          c=0;
        }
      }
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
