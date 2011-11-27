#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

const char *program_name;

void error(const char *msg)
{
	fprintf(stderr, "%s: %s\n", program_name, msg);
}

void die(const char *msg, int status)
{
	error(msg);
	exit(status);
}

int main(int argc, char *argv[])
{
	program_name = argv[0];
	if (argc > 1)
		die("fuck you!", EXIT_FAILURE);
	setlocale(LC_ALL, "");

	return EXIT_SUCCESS;
}
