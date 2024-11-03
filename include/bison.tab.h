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
    LET = 287,                     /* LET  */
    MUT = 288,                     /* MUT  */
    FUN = 289,                     /* FUN  */
    RETURN = 290,                  /* RETURN  */
    IF = 291,                      /* IF  */
    ELSE = 292,                    /* ELSE  */
    BREAK = 293,                   /* BREAK  */
    CONTINUE = 294,                /* CONTINUE  */
    DO = 295,                      /* DO  */
    FOR = 296,                     /* FOR  */
    WHILE = 297,                   /* WHILE  */
    SWITCH = 298,                  /* SWITCH  */
    CASE = 299,                    /* CASE  */
    DEFAULT = 300,                 /* DEFAULT  */
    TYPE = 301,                    /* TYPE  */
    CONST = 302,                   /* CONST  */
    STRUCT = 303,                  /* STRUCT  */
    UNION = 304,                   /* UNION  */
    TRUE_VAL = 305,                /* TRUE_VAL  */
    ENUM = 306,                    /* ENUM  */
    METHODS = 307,                 /* METHODS  */
    MATCH = 308,                   /* MATCH  */
    FALSE_VAL = 309,               /* FALSE_VAL  */
    CLASS = 310,                   /* CLASS  */
    S8 = 311,                      /* S8  */
    S16 = 312,                     /* S16  */
    S32 = 313,                     /* S32  */
    S64 = 314,                     /* S64  */
    U8 = 315,                      /* U8  */
    U16 = 316,                     /* U16  */
    U32 = 317,                     /* U32  */
    U64 = 318,                     /* U64  */
    F32 = 319,                     /* F32  */
    F64 = 320,                     /* F64  */
    T_STRING = 321,                /* T_STRING  */
    T_BOOLEAN = 322,               /* T_BOOLEAN  */
    T_VOID = 323,                  /* T_VOID  */
    T_CHAR = 324,                  /* T_CHAR  */
    LOWER_THAN_ELSE = 325          /* LOWER_THAN_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 18 "./src/parser/gens/bison.y"

    int num;
    char ch;
    long ln;
    char *str;
    double db;
    struct ptree *ptree;

#line 143 "bison.tab.h"

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
