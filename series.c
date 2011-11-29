#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <wchar.h>

const char   *program_name;
const wchar_t bar_characters[] = L" ▁▂▃▄▅▆▇";

void error(const char *msg)
{
	fprintf(stderr, "%s: %s\n", program_name, msg);
}

void die(const char *msg, int status)
{
	error(msg);
	exit(status);
}

/*******************************************************************/
void test(void)
{
	printf("wcslen(bar_characters) = %zi\n", wcslen(bar_characters));
	printf("bar_characters = %ls\n", bar_characters);
}
/*******************************************************************/

int main(int argc, char *argv[])
{
	program_name = argv[0];
	if (argc > 1)
		die("fuck you!", EXIT_FAILURE);
	setlocale(LC_ALL, "");

	test();

	return EXIT_SUCCESS;
}
