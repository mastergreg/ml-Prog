#include <stdio.h>
#include <stdlib.h>

struct trie
{
  unsigned int level;
  unsigned int counter;
  unsigned int children;
  char character;
  struct trie * child[53];
};
typedef struct trie tri;
unsigned int max;

void fill(tri * head,char i)
{
  tri *current=head;
  tri *newchild;
  unsigned int children,buf;
  while(i!='\n')
  {
    children=current->children;
    for(;children>0;children--)
    {
      if (current->child[children-1]->character==i)
      {
        current->child[children-1]->counter++;
        buf=current->level*current->counter;
        max = (max>buf)?max:buf;
        current=current->child[children-1];
        break;
      }
    }
    if (children==0)
    {
      newchild=(tri *)calloc(1,sizeof(tri));
      current->child[current->children]=newchild;
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
