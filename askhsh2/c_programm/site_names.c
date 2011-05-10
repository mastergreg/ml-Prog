#include <stdio.h>
#include <stdlib.h>
#include <string.h>
struct trie
{
  unsigned int level;
  unsigned int counter;
  unsigned int children;
  char character;
  struct trie ** childarray;
};
typedef struct trie tri;
unsigned int max;

void fill(tri * head,char i)
{
  tri *current=head;
  tri *newchild;
  tri **newchildarray;
  unsigned int children,buf;
  while(i!='\n')
  {
    children=current->children;
    for(;children>0;children--)
    {
      if (current->childarray[children-1]->character==i)
      {
        current->childarray[children-1]->counter++;
        current=current->childarray[children-1];
        break;
      }
    }
    if (children==0)
    {
      newchild=(tri *)calloc(1,sizeof(tri));
      newchildarray=(tri **)calloc(current->children+1,sizeof(tri));
      memcpy(newchildarray,current->childarray,(current->children)*sizeof(tri));
      free(current->childarray);
      current->childarray=newchildarray;
      current->childarray[current->children]=newchild;
      newchild->level=current->level+1;
      newchild->character=i;
      newchild->counter=1;
      current->children++;
      current=newchild;
    }
    buf=current->level*current->counter;
    max = (max>buf)?max:buf;
    i=getchar();
  }
}

int main(int argc, char **argv)
{
  char i;
  tri * head;
  head=(tri *)calloc(1,sizeof(tri));
  while(i=getchar()!='\n');
  while((i=getchar())!=EOF)
  {
    fill(head,i); 
  }
  printf("%u\n",max);
  return 0;
}
