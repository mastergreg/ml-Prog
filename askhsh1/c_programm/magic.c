#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
static unsigned int addB(unsigned int n1,unsigned int n2,unsigned int b)
{
  unsigned int ans=0,i=0,c=0,buff=0;
  while(n1!=0 || n2!=0)
  {
    buff=n1%10+n2%10+c;
    ans+=(buff%b)*pow(10,i++);
    c=buff/b;
    n1=n1/10;
    n2=n2/10;
  }
  return ans;
}
static unsigned int complementB(unsigned int n,unsigned int b)
{
  unsigned int buff=0,i=0,ans=0;
  while(n!=0)
  {
    buff=n%10;
    n=n/10;
    ans+=(b-1-buff)*pow(10,i++);
  }
  return ans;
}
static unsigned int sortNum(long n) {
  while (true) {
    long a = n % 10, p = 9;
    bool s = false;
    for (long r = n / 10; r; r/= 10) {
      long b = r % 10;
      if (a > b) {
        n -= p * (b - a);
        s = true;
      } else a = b;
      p *= 10;
    }
    if (!s) return n;
  }
}
static unsigned int subtract(unsigned int n1,unsigned int n2, unsigned int b)
{
  unsigned int i=0, ans=0;
  n2=complementB(n2,b);
  ans=addB(n1,n2,b);
  ans=addB(ans,1,b);
  return ans;
}
static unsigned int magic(unsigned int n, unsigned int b)
{
  return 0;
}
static unsigned int convert_to_base_10(unsigned int n,unsigned int b)
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
/*
 * convert to base 10 calculate convert to base b display result
 *
 *
 static unsigned int sub(unsigned int big,unsigned int small, unsigned int b)
 {
 unsigned int
 }


*/

int main(int argc, char* argv[])
{
  unsigned int b;
  unsigned int n;

  if (argc != 3) {
    printf("Usage: magic n b\n"); return 1;
  }
  sscanf(argv[1], "%u", &n);
  sscanf(argv[2], "%u", &b);
  printf("%u\n", subtract(n,n,b));
  return 0;
}
