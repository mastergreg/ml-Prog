#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ANSWER_DEPTH 4200
#define ANSWER_COUNT 420
#define BASE_TOTAL 42
#define MAX_STR 100
#define TOTAL 4200

int main(int argc, char **argv)
{
	int answer_depth = ANSWER_DEPTH;
	FILE *f = fopen(argv[1], "w");
	fprintf(f,"%d\n", TOTAL);
	char *base = (char *)malloc((answer_depth+1)*sizeof(char));
	int i,j,k;
	srand(time(NULL));
	for(k=0; k< BASE_TOTAL; k++){
		base[0]='A'+rand()%26;
		for(i=1; i<answer_depth;i++){
			base[i]='a'+(rand()%26);
		}
		base[answer_depth]='\0';
		for(i=0; i<ANSWER_COUNT; i++){
			fprintf(f,"%s",	base);
			for(j=0;j<(rand()%(MAX_STR-ANSWER_DEPTH)); j++){
				fprintf(f, "%c", 'a'+rand()%26);
			}
			fprintf(f, "\n");
		}
		answer_depth-=(ANSWER_DEPTH/BASE_TOTAL);
	}
	free(base);
	for(i=ANSWER_COUNT*BASE_TOTAL; i<TOTAL; i++){
		fprintf(f, "%c", 'A'+rand()%26);
		for(j=1; j<(rand()%MAX_STR); j++){
			fprintf(f,"%c", 'a'+rand()%26);
		}
		fprintf(f, "\n");
	}
	fclose(f);
	return 0;
}
		
