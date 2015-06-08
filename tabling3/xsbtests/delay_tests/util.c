#include <stdio.h>

void main(int argc, char *argv[])
{
	fprintf(stdout, "%s\n", argv[1]);
	fprintf(stderr, "----------------------------\n");
	fprintf(stderr, "--> %s...\n", argv[1]);
	fprintf(stderr, "----------------------------\n");
}

