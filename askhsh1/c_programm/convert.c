#include "convert.h"
unsigned int convert_to_base_10(unsigned int n,unsigned int b)
{
  unsigned int i = 0;
  unsigned int result = 0;
  while (n>0)
  {
    result+=(n%10)*pow(b,i);
    n=n/10;
    i++;
  }
  return result;
}
unsigned int convert_to_base_X(unsigned int n,unsigned int b)
{
  unsigned int i = 0;
  unsigned int result = 0;
  while (n>0)
  {
    result+=(n%b)*pow(10,i);
    n=n/b;
    i++;
  }
  return result;
}
unsigned int sortNumS(long n,unsigned int base) 
{
  unsigned int ans=0,i=0;
  for ( unsigned int l = base-1; ; --l )
  {
    unsigned int rem = n % base;
    unsigned int tx = n / base;
    printf("%d > ",l);
    while ( rem!=0 || tx!=0 )
    {
      if ( rem == l ) 
      {
        ans+=rem*pow(10,i++);
      }
      rem = tx % 10;
      tx = tx / 10;
    }
    if (l==0) break;
  }

  return ans;

}
unsigned int sortNumL(long n,unsigned int base) 
{
  unsigned int ans=0,i=0;
  for (int l = 0; abs(l) < base; ++l )
  {
    int rem = n % 10;
    int tx = n / 10;
    while ( rem || tx )
    {
      if ( rem == l ) 
      {
        ans+=rem*pow(10,i++);
      }
      rem = tx % 10;
      tx = tx / 10;
    }
  }
  return ans;

}
unsigned int subtract(unsigned int n1,unsigned int n2, unsigned int b)
{
  unsigned int ans=0;
  n1=convert_to_base_10(n1,b);
  n2=convert_to_base_10(n2,b);
  ans=convert_to_base_X(n1-n2,b);
  return ans;
}
unsigned int bigrandom_Base_X(unsigned int n,unsigned int b)
{
  unsigned int k = 0,i;
  // First digit must be non-zero:
  do
  {
    k = rand() % b;
  } while(k == 0);
  for(i = 1; i < n; i++)
  {
    k *= 10; 
    k += rand() % b;
  }
  return k;
}
