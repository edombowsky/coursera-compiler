/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

%{
 int escaped = 0;
 int string_buf_size = 0;
%}

%x STRING
%x COMMENT
%option stack 

/*
 * Define names for regular expressions here.
 */
DARROW          =>
WHITESPACE      [\ \f\r\t\v]
SINGLECHAR      [.@~*/+-<={}();:,]
A     [aA]
B   [bB]
C   [cC]
D   [dD]
E   [eE]
F   [fF]
G   [gG]
H   [hH]
I   [iI]
J   [jJ]
K   [kK]
L   [lL]
M   [mM]
N   [nN]
O   [oO]
P   [pP]
Q   [qQ]
R   [rR]
S   [sS]
T   [tT]
U   [uU]
V   [vV]
W   [wW]
X   [xX]
Y   [yY]
Z   [zZ]
%%

 /*
  *  Nested comments
  */

\n               { curr_lineno++; }
{WHITESPACE}+              {}
"--".*                     {}
<COMMENT,INITIAL>"(*"      { yy_push_state(COMMENT); }
<COMMENT><<EOF>>           { BEGIN(INITIAL);
                             cool_yylval.error_msg = "EOF in string constantcomment";
           return (ERROR); }
<COMMENT>"*)"        { yy_pop_state(); }
<COMMENT>\n      { curr_lineno++; }
<COMMENT>.       {}
"*)"               { cool_yylval.error_msg = "Unmatched *)"; return (ERROR); } 


 /*
  *  The multiple-character operators.
  */
{DARROW}       { return (DARROW); }
"<-"         { return (ASSIGN); }
"<="                       { return (LE); }
[0-9]+                     { cool_yylval.symbol = inttable.add_string(yytext); return (INT_CONST);}

{SINGLECHAR}       { return (int)(yytext[0]); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{C}{L}{A}{S}{S}            { return (CLASS); }
{E}{L}{S}{E}       { return (ELSE); }
f{A}{L}{S}{E}      { cool_yylval.boolean = false;  return (BOOL_CONST); }
{F}{I}         { return (FI); }
{I}{F}         { return (IF); }
{I}{N}         { return (IN); }
{I}{N}{H}{E}{R}{I}{T}{S}   { return (INHERITS); }
{I}{S}{V}{O}{I}{D}     { return (ISVOID); }
{L}{E}{T}      { return (LET); }
{L}{O}{O}{P}       { return (LOOP); }
{P}{O}{O}{L}       { return (POOL); }
{T}{H}{E}{N}       { return (THEN); }
{W}{H}{I}{L}{E}      { return (WHILE); }
{C}{A}{S}{E}       { return (CASE); }
{E}{S}{A}{C}       { return (ESAC); }
{N}{E}{W}      { return (NEW); }
{O}{F}         { return (OF); }
{N}{O}{T}      { return (NOT); }
t{R}{U}{E}       { cool_yylval.boolean = true; return (BOOL_CONST); } 


[a-z][a-zA-Z0-9_]*         { cool_yylval.symbol = idtable.add_string(yytext); return (OBJECTID);}
[A-Z][a-zA-Z0-9_]*         { cool_yylval.symbol = idtable.add_string(yytext); return (TYPEID);}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>\"      { escaped = 0;
           string_buf_size = 0;
                             string_buf_ptr = string_buf;  
                             BEGIN(STRING); }
<STRING><<EOF>>            { BEGIN(INITIAL);
                             cool_yylval.error_msg = "EOF in string constant";
           return (ERROR); }  
<STRING>\"                 { if(escaped){
                               *string_buf_ptr++ = '\"';
                               string_buf_size++;
             escaped = 0;
           }else{
                               *string_buf_ptr++ = '\0';
             string_buf_size++;
                               BEGIN(INITIAL);
                               if(string_buf_size > 1025)
                               { 
              cool_yylval.error_msg = "String constant too long"; 
                                return (ERROR);
                               }else{
              cool_yylval.symbol = stringtable.add_string(string_buf);
                                return (STR_CONST);
                               } } }
<STRING>\0.*\"             { BEGIN(INITIAL);
                             cool_yylval.error_msg = "String contains null character";
                             return (ERROR); }
<STRING>.|\n               { char c = yytext[0];
                             if(c == '\n')
             curr_lineno++;
                             if(escaped) 
                             { 
                               switch(c)
             {
        case 'n':
          *string_buf_ptr++ = '\n';
                string_buf_size++;
          break;
              case 't':
          *string_buf_ptr++ = '\t';
                string_buf_size++;
          break;
                                case 'b':
          *string_buf_ptr++ = '\b';
                string_buf_size++;
          break;
                                case 'f':
          *string_buf_ptr++ = '\f';
                string_buf_size++;
          break;
                                default:
                                  *string_buf_ptr++ = c;
                string_buf_size++;
                               }
                               escaped = 0;
                             }else{
                            switch(c) 
                              {
        case '\\':
                                  escaped = 1;
          break;
                                case '\n':
                                  BEGIN(INITIAL);
                                  cool_yylval.error_msg = "Unterminated string constant"; 
                                  return (ERROR);
                                default:
                                 *string_buf_ptr++ = c;
               string_buf_size++;
                              }
           } } 

.        { cool_yylval.error_msg = yytext; return (ERROR); }
%%
