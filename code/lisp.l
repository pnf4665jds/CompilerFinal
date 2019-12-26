%{
#include <string.h>
#include "lisp.tab.h"

extern int yyleng;
%}

separator [\t\n\r\s]
letter [a-z]
digit [0-9]

number 0|[1-9]{digit}*|-[1-9]{digit}*
ID {letter}({letter}|{digit}|-)*
bool_val #[tf]

%%

"+"|"-"|"*"|"/"|">"|"<"|"="|"("|")"	{return(yytext[0]);}
mod		{return(MOD);}
and		{return(AND);}
or		{return(OR);}
not		{return(NOT);}
print-num	{return(PRINT_NUM);}
print-bool  {return(PRINT_BOOL);}
if		{return(IF);}
define	{return(DEFINE);}
fun		{return(FUN);}

{number}	{yylval.ival = atoi(yytext); return(NUMBER);}
{ID}	{yylval.str = strdup(yytext); return(ID);}
{bool_val}	{
				yylval.ival = strcmp(yytext, "#t") == 0 ? 1 : 0;
				return(BOOL_VAL);
			}
[\t ]+
.	
\n
%%
