/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_BISON_TAB_H_INCLUDED
# define YY_YY_BISON_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    NUMBER = 258,                  /* NUMBER  */
    FLOAT = 259,                   /* FLOAT  */
    STRING = 260,                  /* STRING  */
    WORD = 261,                    /* WORD  */
    CHAR = 262,                    /* CHAR  */
    HEX = 263,                     /* HEX  */
    OR = 264,                      /* OR  */
    AND = 265,                     /* AND  */
    LSHIFT = 266,                  /* LSHIFT  */
    RSHIFT = 267,                  /* RSHIFT  */
    IS_EQUAL = 268,                /* IS_EQUAL  */
    LESS_EQ = 269,                 /* LESS_EQ  */
    GREATER_EQ = 270,              /* GREATER_EQ  */
    NOT_EQUAL = 271,               /* NOT_EQUAL  */
    INCREMENT = 272,               /* INCREMENT  */
    DECREMENT = 273,               /* DECREMENT  */
    FORWARD_ARROW = 274,           /* FORWARD_ARROW  */
    OR_ASSIGN = 275,               /* OR_ASSIGN  */
    MUL_ASSIGN = 276,              /* MUL_ASSIGN  */
    DIV_ASSIGN = 277,              /* DIV_ASSIGN  */
    MOD_ASSIGN = 278,              /* MOD_ASSIGN  */
    ADD_ASSIGN = 279,              /* ADD_ASSIGN  */
    SUB_ASSIGN = 280,              /* SUB_ASSIGN  */
    SHL_ASSIGN = 281,              /* SHL_ASSIGN  */
    SHR_ASSIGN = 282,              /* SHR_ASSIGN  */
    AND_ASSIGN = 283,              /* AND_ASSIGN  */
    XOR_ASSIGN = 284,              /* XOR_ASSIGN  */
    BACKWARD_ARROW = 285,          /* BACKWARD_ARROW  */
    FUN_ARROW = 286,               /* FUN_ARROW  */
    SCOPE = 287,                   /* SCOPE  */
    LET = 288,                     /* LET  */
    MUT = 289,                     /* MUT  */
    FUN = 290,                     /* FUN  */
    RETURN = 291,                  /* RETURN  */
    IF = 292,                      /* IF  */
    ELSE = 293,                    /* ELSE  */
    BREAK = 294,                   /* BREAK  */
    CONTINUE = 295,                /* CONTINUE  */
    DO = 296,                      /* DO  */
    FOR = 297,                     /* FOR  */
    WHILE = 298,                   /* WHILE  */
    SWITCH = 299,                  /* SWITCH  */
    CASE = 300,                    /* CASE  */
    DEFAULT = 301,                 /* DEFAULT  */
    TYPE = 302,                    /* TYPE  */
    CONST = 303,                   /* CONST  */
    STRUCT = 304,                  /* STRUCT  */
    UNION = 305,                   /* UNION  */
    TRUE_VAL = 306,                /* TRUE_VAL  */
    ENUM = 307,                    /* ENUM  */
    METHODS = 308,                 /* METHODS  */
    MATCH = 309,                   /* MATCH  */
    FALSE_VAL = 310,               /* FALSE_VAL  */
    CLASS = 311,                   /* CLASS  */
    EXPORT = 312,                  /* EXPORT  */
    MODULE = 313,                  /* MODULE  */
    IMPORT = 314,                  /* IMPORT  */
    S8 = 315,                      /* S8  */
    S16 = 316,                     /* S16  */
    S32 = 317,                     /* S32  */
    S64 = 318,                     /* S64  */
    U8 = 319,                      /* U8  */
    U16 = 320,                     /* U16  */
    U32 = 321,                     /* U32  */
    U64 = 322,                     /* U64  */
    F32 = 323,                     /* F32  */
    F64 = 324,                     /* F64  */
    T_STRING = 325,                /* T_STRING  */
    T_BOOLEAN = 326,               /* T_BOOLEAN  */
    T_VOID = 327,                  /* T_VOID  */
    T_CHAR = 328,                  /* T_CHAR  */
    LOWER_THAN_ELSE = 329          /* LOWER_THAN_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 19 "./src/parser/gens/bison.y"

    int num;
    char ch;
    long ln;
    char *str;
    double db;
    struct ptree *ptree;

#line 147 "bison.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse (void);


#endif /* !YY_YY_BISON_TAB_H_INCLUDED  */
