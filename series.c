#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <wchar.h>

const char   *program_name;
const wchar_t bar_chars[]  = L" ▁▂▃▄▅▆▇";
const char    bar_chars2[] =  " ▁▂▃▄▅▆▇";

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
	printf("wcslen(bar_chars) = %zi\n", wcslen(bar_chars));
	printf("bar_chars = %ls\n", bar_chars);
	printf("\n");
	printf("mbstowcs(bar_chars2) = %zi\n", mbstowcs(NULL, bar_chars2, 0));
	printf("bar_chars2 = %s\n", bar_chars2);
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
