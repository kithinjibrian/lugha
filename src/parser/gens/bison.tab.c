/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "./src/parser/gens/bison.y"

#include <stdio.h>
#include <stdarg.h>

#include "file/path.h"
#include "parser/ptree.h"
#include "parser/error.h"
#include "parser/module.h"

int yylex();
void yyerror(const char *msg);

ptree_t *ptree_g = NULL;

#line 86 "bison.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "bison.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_NUMBER = 3,                     /* NUMBER  */
  YYSYMBOL_FLOAT = 4,                      /* FLOAT  */
  YYSYMBOL_STRING = 5,                     /* STRING  */
  YYSYMBOL_WORD = 6,                       /* WORD  */
  YYSYMBOL_CHAR = 7,                       /* CHAR  */
  YYSYMBOL_HEX = 8,                        /* HEX  */
  YYSYMBOL_OR = 9,                         /* OR  */
  YYSYMBOL_AND = 10,                       /* AND  */
  YYSYMBOL_LSHIFT = 11,                    /* LSHIFT  */
  YYSYMBOL_RSHIFT = 12,                    /* RSHIFT  */
  YYSYMBOL_IS_EQUAL = 13,                  /* IS_EQUAL  */
  YYSYMBOL_LESS_EQ = 14,                   /* LESS_EQ  */
  YYSYMBOL_GREATER_EQ = 15,                /* GREATER_EQ  */
  YYSYMBOL_NOT_EQUAL = 16,                 /* NOT_EQUAL  */
  YYSYMBOL_INCREMENT = 17,                 /* INCREMENT  */
  YYSYMBOL_DECREMENT = 18,                 /* DECREMENT  */
  YYSYMBOL_FORWARD_ARROW = 19,             /* FORWARD_ARROW  */
  YYSYMBOL_OR_ASSIGN = 20,                 /* OR_ASSIGN  */
  YYSYMBOL_MUL_ASSIGN = 21,                /* MUL_ASSIGN  */
  YYSYMBOL_DIV_ASSIGN = 22,                /* DIV_ASSIGN  */
  YYSYMBOL_MOD_ASSIGN = 23,                /* MOD_ASSIGN  */
  YYSYMBOL_ADD_ASSIGN = 24,                /* ADD_ASSIGN  */
  YYSYMBOL_SUB_ASSIGN = 25,                /* SUB_ASSIGN  */
  YYSYMBOL_SHL_ASSIGN = 26,                /* SHL_ASSIGN  */
  YYSYMBOL_SHR_ASSIGN = 27,                /* SHR_ASSIGN  */
  YYSYMBOL_AND_ASSIGN = 28,                /* AND_ASSIGN  */
  YYSYMBOL_XOR_ASSIGN = 29,                /* XOR_ASSIGN  */
  YYSYMBOL_BACKWARD_ARROW = 30,            /* BACKWARD_ARROW  */
  YYSYMBOL_FUN_ARROW = 31,                 /* FUN_ARROW  */
  YYSYMBOL_SCOPE = 32,                     /* SCOPE  */
  YYSYMBOL_LET = 33,                       /* LET  */
  YYSYMBOL_MUT = 34,                       /* MUT  */
  YYSYMBOL_FUN = 35,                       /* FUN  */
  YYSYMBOL_RETURN = 36,                    /* RETURN  */
  YYSYMBOL_IF = 37,                        /* IF  */
  YYSYMBOL_ELSE = 38,                      /* ELSE  */
  YYSYMBOL_BREAK = 39,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 40,                  /* CONTINUE  */
  YYSYMBOL_DO = 41,                        /* DO  */
  YYSYMBOL_FOR = 42,                       /* FOR  */
  YYSYMBOL_WHILE = 43,                     /* WHILE  */
  YYSYMBOL_SWITCH = 44,                    /* SWITCH  */
  YYSYMBOL_CASE = 45,                      /* CASE  */
  YYSYMBOL_DEFAULT = 46,                   /* DEFAULT  */
  YYSYMBOL_TYPE = 47,                      /* TYPE  */
  YYSYMBOL_CONST = 48,                     /* CONST  */
  YYSYMBOL_STRUCT = 49,                    /* STRUCT  */
  YYSYMBOL_UNION = 50,                     /* UNION  */
  YYSYMBOL_TRUE_VAL = 51,                  /* TRUE_VAL  */
  YYSYMBOL_ENUM = 52,                      /* ENUM  */
  YYSYMBOL_METHODS = 53,                   /* METHODS  */
  YYSYMBOL_MATCH = 54,                     /* MATCH  */
  YYSYMBOL_FALSE_VAL = 55,                 /* FALSE_VAL  */
  YYSYMBOL_CLASS = 56,                     /* CLASS  */
  YYSYMBOL_EXPORT = 57,                    /* EXPORT  */
  YYSYMBOL_MODULE = 58,                    /* MODULE  */
  YYSYMBOL_IMPORT = 59,                    /* IMPORT  */
  YYSYMBOL_S8 = 60,                        /* S8  */
  YYSYMBOL_S16 = 61,                       /* S16  */
  YYSYMBOL_S32 = 62,                       /* S32  */
  YYSYMBOL_S64 = 63,                       /* S64  */
  YYSYMBOL_U8 = 64,                        /* U8  */
  YYSYMBOL_U16 = 65,                       /* U16  */
  YYSYMBOL_U32 = 66,                       /* U32  */
  YYSYMBOL_U64 = 67,                       /* U64  */
  YYSYMBOL_F32 = 68,                       /* F32  */
  YYSYMBOL_F64 = 69,                       /* F64  */
  YYSYMBOL_T_STRING = 70,                  /* T_STRING  */
  YYSYMBOL_T_BOOLEAN = 71,                 /* T_BOOLEAN  */
  YYSYMBOL_T_VOID = 72,                    /* T_VOID  */
  YYSYMBOL_T_CHAR = 73,                    /* T_CHAR  */
  YYSYMBOL_74_ = 74,                       /* '('  */
  YYSYMBOL_75_ = 75,                       /* ')'  */
  YYSYMBOL_LOWER_THAN_ELSE = 76,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_77_ = 77,                       /* ','  */
  YYSYMBOL_78_ = 78,                       /* '='  */
  YYSYMBOL_79_ = 79,                       /* ';'  */
  YYSYMBOL_80_ = 80,                       /* '?'  */
  YYSYMBOL_81_ = 81,                       /* ':'  */
  YYSYMBOL_82_ = 82,                       /* '|'  */
  YYSYMBOL_83_ = 83,                       /* '^'  */
  YYSYMBOL_84_ = 84,                       /* '&'  */
  YYSYMBOL_85_ = 85,                       /* '<'  */
  YYSYMBOL_86_ = 86,                       /* '>'  */
  YYSYMBOL_87_ = 87,                       /* '+'  */
  YYSYMBOL_88_ = 88,                       /* '-'  */
  YYSYMBOL_89_ = 89,                       /* '*'  */
  YYSYMBOL_90_ = 90,                       /* '/'  */
  YYSYMBOL_91_ = 91,                       /* '%'  */
  YYSYMBOL_92_ = 92,                       /* '!'  */
  YYSYMBOL_93_ = 93,                       /* '~'  */
  YYSYMBOL_94_ = 94,                       /* '.'  */
  YYSYMBOL_95_ = 95,                       /* '['  */
  YYSYMBOL_96_ = 96,                       /* ']'  */
  YYSYMBOL_97_ = 97,                       /* '{'  */
  YYSYMBOL_98_ = 98,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 99,                  /* $accept  */
  YYSYMBOL_program = 100,                  /* program  */
  YYSYMBOL_source_elements = 101,          /* source_elements  */
  YYSYMBOL_source_element = 102,           /* source_element  */
  YYSYMBOL_function_declaration = 103,     /* function_declaration  */
  YYSYMBOL_parameters_list = 104,          /* parameters_list  */
  YYSYMBOL_parameter_is_mut = 105,         /* parameter_is_mut  */
  YYSYMBOL_parameter = 106,                /* parameter  */
  YYSYMBOL_statement = 107,                /* statement  */
  YYSYMBOL_expression_statement = 108,     /* expression_statement  */
  YYSYMBOL_expression = 109,               /* expression  */
  YYSYMBOL_optional_expression = 110,      /* optional_expression  */
  YYSYMBOL_assignment_expression = 111,    /* assignment_expression  */
  YYSYMBOL_assignment_operator = 112,      /* assignment_operator  */
  YYSYMBOL_function_expression = 113,      /* function_expression  */
  YYSYMBOL_function_expression_body = 114, /* function_expression_body  */
  YYSYMBOL_constant_expression = 115,      /* constant_expression  */
  YYSYMBOL_conditional_expression = 116,   /* conditional_expression  */
  YYSYMBOL_logical_or_expression = 117,    /* logical_or_expression  */
  YYSYMBOL_logical_and_expression = 118,   /* logical_and_expression  */
  YYSYMBOL_bitwise_or_expression = 119,    /* bitwise_or_expression  */
  YYSYMBOL_bitwise_xor_expression = 120,   /* bitwise_xor_expression  */
  YYSYMBOL_bitwise_and_expression = 121,   /* bitwise_and_expression  */
  YYSYMBOL_equality_expression = 122,      /* equality_expression  */
  YYSYMBOL_equality_operator = 123,        /* equality_operator  */
  YYSYMBOL_relational_expression = 124,    /* relational_expression  */
  YYSYMBOL_relational_operator = 125,      /* relational_operator  */
  YYSYMBOL_shift_expression = 126,         /* shift_expression  */
  YYSYMBOL_shift_operator = 127,           /* shift_operator  */
  YYSYMBOL_additive_expression = 128,      /* additive_expression  */
  YYSYMBOL_additive_operator = 129,        /* additive_operator  */
  YYSYMBOL_multiplicative_expression = 130, /* multiplicative_expression  */
  YYSYMBOL_multiplicative_operator = 131,  /* multiplicative_operator  */
  YYSYMBOL_unary_expression = 132,         /* unary_expression  */
  YYSYMBOL_unary_operator = 133,           /* unary_operator  */
  YYSYMBOL_postfix_expression = 134,       /* postfix_expression  */
  YYSYMBOL_arguments = 135,                /* arguments  */
  YYSYMBOL_postfix_operator = 136,         /* postfix_operator  */
  YYSYMBOL_primary_expression = 137,       /* primary_expression  */
  YYSYMBOL_qualified_identifier = 138,     /* qualified_identifier  */
  YYSYMBOL_literal = 139,                  /* literal  */
  YYSYMBOL_array_literal = 140,            /* array_literal  */
  YYSYMBOL_element_list = 141,             /* element_list  */
  YYSYMBOL_object_literal = 142,           /* object_literal  */
  YYSYMBOL_keyvalue_list = 143,            /* keyvalue_list  */
  YYSYMBOL_keyvalue = 144,                 /* keyvalue  */
  YYSYMBOL_property_name = 145,            /* property_name  */
  YYSYMBOL_block_statement = 146,          /* block_statement  */
  YYSYMBOL_jump_statement = 147,           /* jump_statement  */
  YYSYMBOL_return_statement = 148,         /* return_statement  */
  YYSYMBOL_break_statement = 149,          /* break_statement  */
  YYSYMBOL_continue_statement = 150,       /* continue_statement  */
  YYSYMBOL_labeled_statement = 151,        /* labeled_statement  */
  YYSYMBOL_selection_statement = 152,      /* selection_statement  */
  YYSYMBOL_iteration_statement = 153,      /* iteration_statement  */
  YYSYMBOL_for_initialization = 154,       /* for_initialization  */
  YYSYMBOL_variable_statement = 155,       /* variable_statement  */
  YYSYMBOL_variable_list = 156,            /* variable_list  */
  YYSYMBOL_variable = 157,                 /* variable  */
  YYSYMBOL_identifier = 158,               /* identifier  */
  YYSYMBOL_type_annotation = 159,          /* type_annotation  */
  YYSYMBOL_type = 160,                     /* type  */
  YYSYMBOL_primitive_type = 161,           /* primitive_type  */
  YYSYMBOL_array_type = 162,               /* array_type  */
  YYSYMBOL_tuple_type = 163,               /* tuple_type  */
  YYSYMBOL_function_type = 164,            /* function_type  */
  YYSYMBOL_types_list = 165,               /* types_list  */
  YYSYMBOL_type_reference = 166,           /* type_reference  */
  YYSYMBOL_typed_arguments = 167,          /* typed_arguments  */
  YYSYMBOL_typed_parameters = 168,         /* typed_parameters  */
  YYSYMBOL_type_params = 169,              /* type_params  */
  YYSYMBOL_type_statement = 170,           /* type_statement  */
  YYSYMBOL_struct_statement = 171,         /* struct_statement  */
  YYSYMBOL_union_statement = 172,          /* union_statement  */
  YYSYMBOL_field_list = 173,               /* field_list  */
  YYSYMBOL_enum_statement = 174,           /* enum_statement  */
  YYSYMBOL_enum_members = 175,             /* enum_members  */
  YYSYMBOL_enum_member = 176,              /* enum_member  */
  YYSYMBOL_methods_statement = 177,        /* methods_statement  */
  YYSYMBOL_method_list = 178,              /* method_list  */
  YYSYMBOL_class_statement = 179,          /* class_statement  */
  YYSYMBOL_export_statement = 180,         /* export_statement  */
  YYSYMBOL_import_statement = 181,         /* import_statement  */
  YYSYMBOL_module_statement = 182          /* module_statement  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  128
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   858

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  99
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  84
/* YYNRULES -- Number of rules.  */
#define YYNRULES  210
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  353

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   329


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    92,     2,     2,     2,    91,    84,     2,
      74,    75,    89,    87,    77,    88,    94,    90,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    81,    79,
      85,    78,    86,    80,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    95,     2,    96,    83,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    97,    82,    98,    93,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    76
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    66,    66,    70,    76,    79,    85,    88,    94,   103,
     106,   109,   115,   118,   124,   127,   134,   137,   140,   143,
     146,   149,   152,   155,   158,   161,   164,   167,   170,   173,
     176,   179,   186,   193,   196,   202,   205,   211,   214,   217,
     223,   226,   229,   232,   235,   238,   241,   244,   247,   250,
     253,   259,   268,   271,   277,   283,   286,   294,   297,   304,
     307,   314,   317,   324,   327,   334,   337,   344,   347,   353,
     356,   362,   365,   371,   374,   377,   380,   386,   389,   395,
     398,   404,   407,   413,   416,   422,   425,   431,   434,   437,
     443,   446,   452,   455,   458,   461,   464,   467,   470,   476,
     479,   482,   487,   492,   500,   503,   506,   512,   515,   521,
     524,   527,   530,   533,   541,   545,   549,   557,   560,   563,
     566,   572,   580,   583,   586,   589,   595,   603,   606,   609,
     612,   618,   625,   628,   631,   637,   643,   652,   655,   658,
     663,   667,   673,   679,   685,   690,   694,   701,   708,   715,
     724,   729,   737,   747,   750,   756,   760,   767,   770,   776,
     779,   786,   793,   797,   803,   806,   809,   812,   815,   821,
     824,   827,   830,   836,   844,   853,   862,   865,   868,   874,
     878,   885,   893,   898,   904,   908,   915,   924,   928,   937,
     946,   949,   952,   955,   961,   970,   973,   976,   982,   985,
     991,   997,  1006,  1014,  1017,  1023,  1032,  1035,  1038,  1044,
    1051
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "NUMBER", "FLOAT",
  "STRING", "WORD", "CHAR", "HEX", "OR", "AND", "LSHIFT", "RSHIFT",
  "IS_EQUAL", "LESS_EQ", "GREATER_EQ", "NOT_EQUAL", "INCREMENT",
  "DECREMENT", "FORWARD_ARROW", "OR_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN",
  "MOD_ASSIGN", "ADD_ASSIGN", "SUB_ASSIGN", "SHL_ASSIGN", "SHR_ASSIGN",
  "AND_ASSIGN", "XOR_ASSIGN", "BACKWARD_ARROW", "FUN_ARROW", "SCOPE",
  "LET", "MUT", "FUN", "RETURN", "IF", "ELSE", "BREAK", "CONTINUE", "DO",
  "FOR", "WHILE", "SWITCH", "CASE", "DEFAULT", "TYPE", "CONST", "STRUCT",
  "UNION", "TRUE_VAL", "ENUM", "METHODS", "MATCH", "FALSE_VAL", "CLASS",
  "EXPORT", "MODULE", "IMPORT", "S8", "S16", "S32", "S64", "U8", "U16",
  "U32", "U64", "F32", "F64", "T_STRING", "T_BOOLEAN", "T_VOID", "T_CHAR",
  "'('", "')'", "LOWER_THAN_ELSE", "','", "'='", "';'", "'?'", "':'",
  "'|'", "'^'", "'&'", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'!'", "'~'", "'.'", "'['", "']'", "'{'", "'}'", "$accept", "program",
  "source_elements", "source_element", "function_declaration",
  "parameters_list", "parameter_is_mut", "parameter", "statement",
  "expression_statement", "expression", "optional_expression",
  "assignment_expression", "assignment_operator", "function_expression",
  "function_expression_body", "constant_expression",
  "conditional_expression", "logical_or_expression",
  "logical_and_expression", "bitwise_or_expression",
  "bitwise_xor_expression", "bitwise_and_expression",
  "equality_expression", "equality_operator", "relational_expression",
  "relational_operator", "shift_expression", "shift_operator",
  "additive_expression", "additive_operator", "multiplicative_expression",
  "multiplicative_operator", "unary_expression", "unary_operator",
  "postfix_expression", "arguments", "postfix_operator",
  "primary_expression", "qualified_identifier", "literal", "array_literal",
  "element_list", "object_literal", "keyvalue_list", "keyvalue",
  "property_name", "block_statement", "jump_statement", "return_statement",
  "break_statement", "continue_statement", "labeled_statement",
  "selection_statement", "iteration_statement", "for_initialization",
  "variable_statement", "variable_list", "variable", "identifier",
  "type_annotation", "type", "primitive_type", "array_type", "tuple_type",
  "function_type", "types_list", "type_reference", "typed_arguments",
  "typed_parameters", "type_params", "type_statement", "struct_statement",
  "union_statement", "field_list", "enum_statement", "enum_members",
  "enum_member", "methods_statement", "method_list", "class_statement",
  "export_statement", "import_statement", "module_statement", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-267)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-37)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     521,  -267,  -267,   -48,  -267,  -267,    50,    56,     9,   185,
      -5,    -4,     6,   593,     3,    72,    87,   763,    34,    86,
      56,   117,   129,  -267,   158,    19,  -267,   167,    12,   170,
     174,   724,  -267,  -267,  -267,  -267,  -267,   724,   305,   189,
     521,  -267,  -267,  -267,  -267,   120,   125,  -267,  -267,  -267,
       0,   195,   124,   126,   123,   118,    -3,    16,   111,    60,
     201,   763,    13,  -267,   -11,  -267,  -267,  -267,  -267,  -267,
    -267,  -267,  -267,  -267,  -267,  -267,   131,  -267,  -267,  -267,
    -267,  -267,  -267,  -267,  -267,  -267,   593,  -267,   132,   135,
    -267,   136,   133,   210,   159,  -267,   133,  -267,    63,   724,
    -267,  -267,   196,   160,   724,   724,   151,  -267,  -267,   593,
     133,   135,   -53,   133,   133,    -8,   133,   232,  -267,  -267,
    -267,   152,   171,    77,  -267,   -33,  -267,   377,  -267,  -267,
     724,  -267,   763,   724,   763,   763,   763,   763,  -267,  -267,
     763,  -267,  -267,  -267,  -267,   763,  -267,  -267,   763,  -267,
    -267,   763,  -267,  -267,  -267,   763,  -267,  -267,  -267,  -267,
    -267,  -267,  -267,  -267,  -267,  -267,  -267,   724,  -267,  -267,
    -267,   724,   252,   724,  -267,   254,    61,  -267,  -267,    49,
    -267,    56,   724,   188,  -267,     7,    11,  -267,    78,   191,
    -267,   184,  -267,    82,    85,   593,  -267,   190,  -267,   169,
     173,   177,   236,   178,   665,  -267,  -267,   724,  -267,  -267,
    -267,   195,   186,   124,   126,   123,   118,    -3,    16,   111,
      60,  -267,  -267,  -267,    92,  -267,   -18,  -267,  -267,  -267,
    -267,   -27,  -267,   200,   197,  -267,  -267,  -267,  -267,    18,
    -267,  -267,  -267,  -267,  -267,  -267,  -267,    11,   270,  -267,
      56,    93,  -267,  -267,   205,   593,   724,   724,   593,   593,
    -267,    49,    56,    56,   278,  -267,     1,    56,   449,  -267,
     724,  -267,   724,  -267,    61,  -267,   724,    49,  -267,   211,
      10,   106,  -267,  -267,   132,    11,   724,   248,   109,   208,
    -267,  -267,   -14,  -267,   -19,    39,   -21,    41,  -267,  -267,
    -267,    45,  -267,  -267,  -267,  -267,  -267,    18,    40,    49,
     192,  -267,   132,   271,  -267,  -267,   593,   212,   724,  -267,
      56,  -267,  -267,    49,   286,    56,   278,  -267,  -267,    49,
    -267,   110,  -267,   198,    17,  -267,  -267,   217,  -267,   119,
    -267,    47,  -267,    18,  -267,  -267,  -267,  -267,  -267,   593,
    -267,  -267,  -267
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,   117,   120,   114,    92,    93,     0,     0,   183,     0,
       0,     0,     0,    36,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,   119,     0,     0,     0,
       0,     0,    98,    96,    97,    94,    95,   125,    36,     0,
       2,     5,     6,     7,    16,    35,     0,    33,    38,    39,
      55,    57,    59,    61,    63,    65,    67,    71,    77,    81,
      85,     0,    90,    99,   109,   110,   111,   112,    17,    18,
     137,   138,   139,    19,    20,    21,     0,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    36,   116,   163,   155,
     157,   159,   183,     0,     0,   114,   183,   140,     0,     0,
     142,   143,     0,    36,     0,     0,     0,    54,    85,    36,
     183,   156,   183,   183,   183,     0,   183,     0,   206,   207,
     208,     0,     0,     0,   122,     0,   136,    36,     1,     4,
       0,    32,     0,     0,     0,     0,     0,     0,    69,    70,
       0,    75,    76,    73,    74,     0,    79,    80,     0,    83,
      84,     0,    87,    88,    89,     0,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    40,     0,    91,   107,
     108,   106,     0,     0,   100,     0,   130,    22,   144,     0,
     161,     0,     0,     0,   184,     0,    11,   141,     0,     0,
     153,     0,   154,     0,     0,    36,   146,     0,   187,     0,
       0,     0,     0,     0,    36,   209,   113,   124,   121,   135,
      34,    58,     0,    60,    62,    64,    66,    68,    72,    78,
      82,    86,    37,   104,     0,   101,     0,   115,   134,   133,
     132,     0,   127,     0,   179,   170,   171,   172,   169,   162,
     164,   165,   166,   167,   168,   158,   160,    11,     0,   182,
       0,     0,     9,    12,    14,    36,     0,    36,    36,    36,
     145,     0,   193,   193,     0,   203,     0,   193,    36,   123,
       0,   103,     0,   102,   129,   126,     0,   178,   180,     0,
       0,     0,   185,    13,   163,     0,     0,   147,     0,     0,
     150,   149,     0,   190,     0,     0,   198,     0,   195,   202,
     204,     0,   210,    56,   105,   128,   131,   176,     0,   178,
       0,   173,   163,     0,    10,    15,    36,     0,    36,   186,
     192,   188,   189,   178,     0,   193,   197,   194,   205,     0,
     181,     0,   174,     0,     0,   148,   151,     0,   191,     0,
     201,     0,   196,   177,   175,     8,    52,    51,    53,    36,
     199,   200,   152
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -267,  -267,   -30,   -37,   -23,    46,    15,    44,   -13,  -267,
      -2,   -99,   -35,  -267,  -267,  -267,  -267,   279,  -267,   165,
     164,   166,   168,   172,  -267,   162,  -267,   161,  -267,   157,
    -267,   156,  -267,    -7,  -267,  -267,  -267,  -267,  -267,   287,
    -267,  -267,  -267,  -267,  -267,    42,  -267,  -133,  -267,  -267,
    -267,  -267,  -267,  -267,  -267,  -267,   214,   293,   134,    -6,
    -266,  -173,  -267,  -267,  -267,  -267,  -249,  -267,  -267,   -73,
    -267,  -267,   290,  -267,  -225,  -267,  -267,    -1,  -267,  -267,
    -267,  -267,  -267,   292
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    39,    40,    41,    42,   251,   252,   253,    43,    44,
      45,    46,    47,   167,    48,   347,   106,    49,    50,    51,
      52,    53,    54,    55,   140,    56,   145,    57,   148,    58,
     151,    59,   155,    60,    61,    62,   224,   174,    63,    64,
      65,    66,   125,    67,   231,   232,   233,    68,    69,    70,
      71,    72,    73,    74,    75,   191,    76,    89,    90,   254,
     180,   307,   240,   241,   242,   243,   308,   244,   278,    94,
     185,    77,    78,    79,   294,    80,   297,   298,    81,   266,
      82,    83,    84,    85
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     102,    91,   124,   129,   190,   118,   239,    98,   127,   132,
     108,   141,   142,   310,    91,    92,   279,    88,   313,   183,
       1,   175,     2,    95,   175,    95,   198,   146,   147,   123,
     169,   170,    93,    86,     4,     5,   117,   197,   295,   199,
     200,   201,   301,   203,   207,   250,   333,   117,   279,     6,
     274,     6,    96,   323,   168,   234,    87,   324,   320,   130,
     331,    21,    88,   208,   228,   319,   229,   230,    23,    99,
      29,   275,    26,   178,   339,   100,   325,   103,   273,   321,
     133,   280,   143,   144,   248,   101,   176,   171,   292,   202,
     129,    31,   110,   249,    93,   210,   196,   188,   212,   299,
     341,    32,   193,   194,    33,    34,   311,   172,   173,    35,
      36,   235,    37,   280,    38,   109,   320,   329,   326,   236,
     237,   238,   320,   112,   320,   108,   330,   108,   108,   108,
     108,   138,   222,   108,   139,   113,   223,   322,   108,   327,
     130,   108,   187,   328,   108,   351,   104,   246,   221,   152,
     153,   154,   206,   255,   130,   130,   343,   258,   289,   130,
     259,   105,   130,     1,   114,     2,    95,   271,   284,   272,
     285,   226,   269,   116,   268,    91,   121,     4,     5,   265,
     122,   312,   260,   285,   317,   344,   130,   329,     1,   128,
       2,    95,     6,     7,   350,    96,   329,   130,   149,   150,
     345,   348,     4,     5,   131,   134,   135,   137,    20,   136,
     177,    23,   181,   179,   182,    26,   184,     6,    93,   337,
      96,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   129,   195,   186,    31,   303,    23,   304,    92,   189,
      26,   306,   287,   300,    32,   290,   291,    33,    34,   204,
     205,   315,    35,    36,   288,    37,   293,   293,   225,    31,
     227,   293,   247,   257,    97,   256,   262,   270,   261,    32,
     263,   117,    33,    34,   264,   267,   282,    35,    36,   166,
      37,   276,   277,   286,   296,   309,   316,   318,   332,   340,
     334,   336,   349,   281,   283,    38,   107,   211,   213,   346,
     314,   214,   217,   335,   215,   219,   218,   220,     1,   216,
       2,     3,   115,   111,   338,   245,   305,   192,   119,   293,
     120,     0,     4,     5,     0,   342,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   352,     6,     7,     0,
       8,     9,    10,     0,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,    27,    28,    29,    30,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
       1,     0,     2,     3,     0,     0,     0,     0,     0,    32,
       0,     0,    33,    34,     4,     5,     0,    35,    36,     0,
      37,     0,    38,   126,     0,     0,     0,     0,     0,     6,
       7,     0,     8,     9,    10,     0,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,    27,    28,    29,    30,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    31,     1,     0,     2,     3,     0,     0,     0,     0,
       0,    32,     0,     0,    33,    34,     4,     5,     0,    35,
      36,     0,    37,     0,    38,   209,     0,     0,     0,     0,
       0,     6,     7,     0,     8,     9,    10,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,    27,    28,    29,    30,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    31,     1,     0,     2,     3,     0,     0,
       0,     0,     0,    32,     0,     0,    33,    34,     4,     5,
       0,    35,    36,     0,    37,     0,    38,   302,     0,     0,
       0,     0,     0,     6,     7,     0,     8,     9,    10,     0,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,    27,    28,    29,
      30,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    31,     1,     0,     2,     3,
     -36,     0,     0,     0,     0,    32,     0,     0,    33,    34,
       4,     5,     0,    35,    36,     0,    37,     0,    38,     0,
       0,     0,     0,     0,     0,     6,     7,     0,    96,     9,
      10,     0,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,    27,
      28,    29,    30,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,     1,     0,
       2,     3,     0,     0,     0,     0,     0,    32,     0,     0,
      33,    34,     4,     5,     0,    35,    36,     0,    37,     0,
      38,     0,     0,     0,     0,     0,     0,     6,     7,     0,
       8,     9,    10,     0,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,    27,    28,    29,    30,     0,     0,     1,     0,     2,
      95,     0,     0,     0,     0,     0,     0,     0,     0,    31,
       0,     4,     5,     0,     0,     0,     0,     0,     0,    32,
       0,     0,    33,    34,     0,     0,     6,    35,    36,    96,
      37,     0,    38,     0,     0,     0,     1,     0,     2,    95,
       0,     0,     0,     0,     0,    23,     0,     0,     0,    26,
       4,     5,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     6,     0,     0,    31,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    32,     0,
       0,    33,    34,     0,    23,     0,    35,    36,    26,    37,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,     0,     0,
      33,    34,     0,     0,     0,    35,    36,     0,    37
};

static const yytype_int16 yycheck[] =
{
      13,     7,    37,    40,   103,    28,   179,     9,    38,     9,
      17,    14,    15,     3,    20,     6,    30,     6,   284,    92,
       3,    32,     5,     6,    32,     6,    79,    11,    12,    31,
      17,    18,    85,    81,    17,    18,    35,   110,   263,   112,
     113,   114,   267,   116,    77,    34,   312,    35,    30,    32,
      77,    32,    35,    74,    61,     6,     6,    78,    77,    77,
     309,    49,     6,    96,     3,    79,     5,     6,    51,    74,
      58,    98,    55,    86,   323,    79,    97,    74,    96,    98,
      80,    95,    85,    86,    77,    79,    97,    74,   261,    97,
     127,    74,     6,    86,    85,   130,   109,    99,   133,    98,
     325,    84,   104,   105,    87,    88,    96,    94,    95,    92,
      93,    62,    95,    95,    97,    81,    77,    77,    77,    70,
      71,    72,    77,     6,    77,   132,    86,   134,   135,   136,
     137,    13,   167,   140,    16,     6,   171,    98,   145,    98,
      77,   148,    79,    98,   151,    98,    74,   182,   155,    89,
      90,    91,    75,    75,    77,    77,   329,    75,   257,    77,
      75,    74,    77,     3,     6,     5,     6,    75,    75,    77,
      77,   173,   207,     6,   204,   181,     6,    17,    18,   202,
       6,    75,   195,    77,    75,    75,    77,    77,     3,     0,
       5,     6,    32,    33,    75,    35,    77,    77,    87,    88,
     333,   334,    17,    18,    79,    10,    82,    84,    48,    83,
      79,    51,    77,    81,    78,    55,     6,    32,    85,   318,
      35,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,   268,    81,    74,    74,   270,    51,   272,     6,    43,
      55,   276,   255,   266,    84,   258,   259,    87,    88,    97,
      79,   286,    92,    93,   256,    95,   262,   263,     6,    74,
       6,   267,    74,    79,    79,    74,    97,    81,    78,    84,
      97,    35,    87,    88,    97,    97,     6,    92,    93,    78,
      95,    81,    85,    78,     6,    74,    38,    79,    96,     3,
      19,    79,    75,   247,   250,    97,    17,   132,   134,   334,
     285,   135,   140,   316,   136,   148,   145,   151,     3,   137,
       5,     6,    25,    20,   320,   181,   274,   103,    28,   325,
      28,    -1,    17,    18,    -1,   326,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   349,    32,    33,    -1,
      35,    36,    37,    -1,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    -1,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
       3,    -1,     5,     6,    -1,    -1,    -1,    -1,    -1,    84,
      -1,    -1,    87,    88,    17,    18,    -1,    92,    93,    -1,
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,    32,
      33,    -1,    35,    36,    37,    -1,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    -1,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,     3,    -1,     5,     6,    -1,    -1,    -1,    -1,
      -1,    84,    -1,    -1,    87,    88,    17,    18,    -1,    92,
      93,    -1,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,    32,    33,    -1,    35,    36,    37,    -1,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    -1,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,     3,    -1,     5,     6,    -1,    -1,
      -1,    -1,    -1,    84,    -1,    -1,    87,    88,    17,    18,
      -1,    92,    93,    -1,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,    32,    33,    -1,    35,    36,    37,    -1,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    -1,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,     3,    -1,     5,     6,
      79,    -1,    -1,    -1,    -1,    84,    -1,    -1,    87,    88,
      17,    18,    -1,    92,    93,    -1,    95,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    32,    33,    -1,    35,    36,
      37,    -1,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,     3,    -1,
       5,     6,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
      87,    88,    17,    18,    -1,    92,    93,    -1,    95,    -1,
      97,    -1,    -1,    -1,    -1,    -1,    -1,    32,    33,    -1,
      35,    36,    37,    -1,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    -1,
      55,    56,    57,    58,    59,    -1,    -1,     3,    -1,     5,
       6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      -1,    -1,    87,    88,    -1,    -1,    32,    92,    93,    35,
      95,    -1,    97,    -1,    -1,    -1,     3,    -1,     5,     6,
      -1,    -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,    55,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    32,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,
      -1,    87,    88,    -1,    51,    -1,    92,    93,    55,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    92,    93,    -1,    95
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,     6,    17,    18,    32,    33,    35,    36,
      37,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    55,    56,    57,    58,
      59,    74,    84,    87,    88,    92,    93,    95,    97,   100,
     101,   102,   103,   107,   108,   109,   110,   111,   113,   116,
     117,   118,   119,   120,   121,   122,   124,   126,   128,   130,
     132,   133,   134,   137,   138,   139,   140,   142,   146,   147,
     148,   149,   150,   151,   152,   153,   155,   170,   171,   172,
     174,   177,   179,   180,   181,   182,    81,     6,     6,   156,
     157,   158,     6,    85,   168,     6,    35,    79,   109,    74,
      79,    79,   107,    74,    74,    74,   115,   116,   132,    81,
       6,   156,     6,     6,     6,   138,     6,    35,   103,   171,
     182,     6,     6,   109,   111,   141,    98,   101,     0,   102,
      77,    79,     9,    80,    10,    82,    83,    84,    13,    16,
     123,    14,    15,    85,    86,   125,    11,    12,   127,    87,
      88,   129,    89,    90,    91,   131,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    78,   112,   132,    17,
      18,    74,    94,    95,   136,    32,    97,    79,   107,    81,
     159,    77,    78,   168,     6,   169,    74,    79,   109,    43,
     110,   154,   155,   109,   109,    81,   107,   168,    79,   168,
     168,   168,    97,   168,    97,    79,    75,    77,    96,    98,
     111,   118,   111,   119,   120,   121,   122,   124,   126,   128,
     130,   132,   111,   111,   135,     6,   109,     6,     3,     5,
       6,   143,   144,   145,     6,    62,    70,    71,    72,   160,
     161,   162,   163,   164,   166,   157,   111,    74,    77,    86,
      34,   104,   105,   106,   158,    75,    74,    79,    75,    75,
     107,    78,    97,    97,    97,   103,   178,    97,   101,   111,
      81,    75,    77,    96,    77,    98,    81,    85,   167,    30,
      95,   104,     6,   106,    75,    77,    78,   107,   109,   110,
     107,   107,   160,   158,   173,   173,     6,   175,   176,    98,
     103,   173,    98,   111,   111,   144,   111,   160,   165,    74,
       3,    96,    75,   159,   105,   111,    38,    75,    79,    79,
      77,    98,    98,    74,    78,    97,    77,    98,    98,    77,
      86,   165,    96,   159,    19,   107,    79,   110,   158,   165,
       3,   173,   176,   160,    75,   146,   111,   114,   146,    75,
      75,    98,   107
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    99,   100,   100,   101,   101,   102,   102,   103,   104,
     104,   104,   105,   105,   106,   106,   107,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
     107,   107,   108,   109,   109,   110,   110,   111,   111,   111,
     112,   112,   112,   112,   112,   112,   112,   112,   112,   112,
     112,   113,   114,   114,   115,   116,   116,   117,   117,   118,
     118,   119,   119,   120,   120,   121,   121,   122,   122,   123,
     123,   124,   124,   125,   125,   125,   125,   126,   126,   127,
     127,   128,   128,   129,   129,   130,   130,   131,   131,   131,
     132,   132,   133,   133,   133,   133,   133,   133,   133,   134,
     134,   134,   134,   134,   135,   135,   135,   136,   136,   137,
     137,   137,   137,   137,   138,   138,   138,   139,   139,   139,
     139,   140,   141,   141,   141,   141,   142,   143,   143,   143,
     143,   144,   145,   145,   145,   146,   146,   147,   147,   147,
     148,   148,   149,   150,   151,   151,   151,   152,   152,   152,
     153,   153,   153,   154,   154,   155,   155,   156,   156,   157,
     157,   158,   159,   159,   160,   160,   160,   160,   160,   161,
     161,   161,   161,   162,   163,   164,   165,   165,   165,   166,
     166,   167,   168,   168,   169,   169,   170,   171,   171,   172,
     173,   173,   173,   173,   174,   175,   175,   175,   176,   176,
     176,   176,   177,   178,   178,   179,   180,   180,   180,   181,
     182
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     2,     1,     1,     1,     8,     1,
       3,     0,     1,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     3,     1,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     8,     1,     1,     1,     1,     5,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     3,     1,     1,     1,     3,     1,     1,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     3,     4,     4,     1,     3,     0,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     2,     1,     1,     1,
       1,     3,     1,     3,     2,     0,     4,     1,     3,     2,
       0,     3,     1,     1,     1,     3,     2,     1,     1,     1,
       2,     3,     2,     2,     3,     4,     3,     5,     7,     5,
       5,     7,     9,     1,     1,     2,     2,     1,     3,     1,
       3,     2,     2,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     5,     1,     3,     0,     1,
       2,     3,     3,     0,     1,     3,     6,     3,     6,     6,
       1,     3,     2,     0,     6,     1,     3,     2,     1,     4,
       4,     3,     5,     1,     2,     6,     2,     2,     2,     3,
       5
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: source_elements  */
#line 66 "./src/parser/gens/bison.y"
                    {
        ptree_t *f = ptree_create(PTREE_EOF, 0);
        ptree_g = ptree_add((yyvsp[0].ptree), 1, f);
    }
#line 2029 "bison.tab.c"
    break;

  case 3: /* program: %empty  */
#line 70 "./src/parser/gens/bison.y"
                  {
        ptree_g = ptree_create(PTREE_EOF, 0);
    }
#line 2037 "bison.tab.c"
    break;

  case 4: /* source_elements: source_elements source_element  */
#line 76 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 2045 "bison.tab.c"
    break;

  case 5: /* source_elements: source_element  */
#line 79 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_SOURCE_ELEMENTS, 1, (yyvsp[0].ptree));
    }
#line 2053 "bison.tab.c"
    break;

  case 6: /* source_element: function_declaration  */
#line 85 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2061 "bison.tab.c"
    break;

  case 7: /* source_element: statement  */
#line 88 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2069 "bison.tab.c"
    break;

  case 8: /* function_declaration: FUN WORD typed_parameters '(' parameters_list ')' type_annotation block_statement  */
#line 94 "./src/parser/gens/bison.y"
                                                                                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-6].str));
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_DECLARATION, 7, w, (yyvsp[-5].ptree), lp, (yyvsp[-3].ptree), rp, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2080 "bison.tab.c"
    break;

  case 9: /* parameters_list: parameter_is_mut  */
#line 103 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_LIST, 1, (yyvsp[0].ptree));
    }
#line 2088 "bison.tab.c"
    break;

  case 10: /* parameters_list: parameters_list ',' parameter_is_mut  */
#line 106 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2096 "bison.tab.c"
    break;

  case 11: /* parameters_list: %empty  */
#line 109 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 2104 "bison.tab.c"
    break;

  case 12: /* parameter_is_mut: parameter  */
#line 115 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_CONST, 1, (yyvsp[0].ptree));
    }
#line 2112 "bison.tab.c"
    break;

  case 13: /* parameter_is_mut: MUT parameter  */
#line 118 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_MUT, 1, (yyvsp[0].ptree));
    }
#line 2120 "bison.tab.c"
    break;

  case 14: /* parameter: identifier  */
#line 124 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 2128 "bison.tab.c"
    break;

  case 15: /* parameter: identifier '=' assignment_expression  */
#line 127 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 2137 "bison.tab.c"
    break;

  case 16: /* statement: expression_statement  */
#line 134 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2145 "bison.tab.c"
    break;

  case 17: /* statement: block_statement  */
#line 137 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2153 "bison.tab.c"
    break;

  case 18: /* statement: jump_statement  */
#line 140 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2161 "bison.tab.c"
    break;

  case 19: /* statement: labeled_statement  */
#line 143 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2169 "bison.tab.c"
    break;

  case 20: /* statement: selection_statement  */
#line 146 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2177 "bison.tab.c"
    break;

  case 21: /* statement: iteration_statement  */
#line 149 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2185 "bison.tab.c"
    break;

  case 22: /* statement: variable_statement ';'  */
#line 152 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2193 "bison.tab.c"
    break;

  case 23: /* statement: type_statement  */
#line 155 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2201 "bison.tab.c"
    break;

  case 24: /* statement: struct_statement  */
#line 158 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2209 "bison.tab.c"
    break;

  case 25: /* statement: union_statement  */
#line 161 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2217 "bison.tab.c"
    break;

  case 26: /* statement: enum_statement  */
#line 164 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2225 "bison.tab.c"
    break;

  case 27: /* statement: methods_statement  */
#line 167 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2233 "bison.tab.c"
    break;

  case 28: /* statement: class_statement  */
#line 170 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2241 "bison.tab.c"
    break;

  case 29: /* statement: export_statement  */
#line 173 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2249 "bison.tab.c"
    break;

  case 30: /* statement: import_statement  */
#line 176 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2257 "bison.tab.c"
    break;

  case 31: /* statement: module_statement  */
#line 179 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2265 "bison.tab.c"
    break;

  case 32: /* expression_statement: optional_expression ';'  */
#line 186 "./src/parser/gens/bison.y"
                            {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION_STATEMENT, 2, (yyvsp[-1].ptree), sc);
    }
#line 2274 "bison.tab.c"
    break;

  case 33: /* expression: assignment_expression  */
#line 193 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2282 "bison.tab.c"
    break;

  case 34: /* expression: expression ',' assignment_expression  */
#line 196 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 2, (yyvsp[0].ptree));
    }
#line 2290 "bison.tab.c"
    break;

  case 35: /* optional_expression: expression  */
#line 202 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2298 "bison.tab.c"
    break;

  case 36: /* optional_expression: %empty  */
#line 205 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2306 "bison.tab.c"
    break;

  case 37: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 211 "./src/parser/gens/bison.y"
                                                               {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2314 "bison.tab.c"
    break;

  case 38: /* assignment_expression: function_expression  */
#line 214 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2322 "bison.tab.c"
    break;

  case 39: /* assignment_expression: conditional_expression  */
#line 217 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2330 "bison.tab.c"
    break;

  case 40: /* assignment_operator: '='  */
#line 223 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_ASSIGN, 0);
    }
#line 2338 "bison.tab.c"
    break;

  case 41: /* assignment_operator: OR_ASSIGN  */
#line 226 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_OR_ASSIGN, 0);
    }
#line 2346 "bison.tab.c"
    break;

  case 42: /* assignment_operator: MUL_ASSIGN  */
#line 229 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MUL_ASSIGN, 0);
    }
#line 2354 "bison.tab.c"
    break;

  case 43: /* assignment_operator: DIV_ASSIGN  */
#line 232 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_DIV_ASSIGN, 0);
    }
#line 2362 "bison.tab.c"
    break;

  case 44: /* assignment_operator: MOD_ASSIGN  */
#line 235 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MOD_ASSIGN, 0);
    }
#line 2370 "bison.tab.c"
    break;

  case 45: /* assignment_operator: ADD_ASSIGN  */
#line 238 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_ADD_ASSIGN, 0);
    }
#line 2378 "bison.tab.c"
    break;

  case 46: /* assignment_operator: SUB_ASSIGN  */
#line 241 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SUB_ASSIGN, 0);
    }
#line 2386 "bison.tab.c"
    break;

  case 47: /* assignment_operator: SHL_ASSIGN  */
#line 244 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHL_ASSIGN, 0);
    }
#line 2394 "bison.tab.c"
    break;

  case 48: /* assignment_operator: SHR_ASSIGN  */
#line 247 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHR_ASSIGN, 0);
    }
#line 2402 "bison.tab.c"
    break;

  case 49: /* assignment_operator: AND_ASSIGN  */
#line 250 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_AND_ASSIGN, 0);
    }
#line 2410 "bison.tab.c"
    break;

  case 50: /* assignment_operator: XOR_ASSIGN  */
#line 253 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_XOR_ASSIGN, 0);
    }
#line 2418 "bison.tab.c"
    break;

  case 51: /* function_expression: FUN typed_parameters '(' parameters_list ')' type_annotation FORWARD_ARROW function_expression_body  */
#line 259 "./src/parser/gens/bison.y"
                                                                                                        {
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        ptree_t *fa = ptree_create(PTREE_FORWARD_ARROW, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_EXPRESSION, 7, (yyvsp[-6].ptree), lp, (yyvsp[-4].ptree), rp, (yyvsp[-2].ptree), fa, (yyvsp[0].ptree));
    }
#line 2429 "bison.tab.c"
    break;

  case 52: /* function_expression_body: assignment_expression  */
#line 268 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2437 "bison.tab.c"
    break;

  case 53: /* function_expression_body: block_statement  */
#line 271 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2445 "bison.tab.c"
    break;

  case 54: /* constant_expression: conditional_expression  */
#line 277 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2453 "bison.tab.c"
    break;

  case 55: /* conditional_expression: logical_or_expression  */
#line 283 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2461 "bison.tab.c"
    break;

  case 56: /* conditional_expression: logical_or_expression '?' assignment_expression ':' assignment_expression  */
#line 286 "./src/parser/gens/bison.y"
                                                                                {
        ptree_t *q = ptree_create(PTREE_QUESTION, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TERNARY_EXPRESSION, 5, (yyvsp[-4].ptree), q, (yyvsp[-2].ptree), sc, (yyvsp[0].ptree));
    }
#line 2471 "bison.tab.c"
    break;

  case 57: /* logical_or_expression: logical_and_expression  */
#line 294 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2479 "bison.tab.c"
    break;

  case 58: /* logical_or_expression: logical_or_expression OR logical_and_expression  */
#line 297 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *op = ptree_create(PTREE_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2488 "bison.tab.c"
    break;

  case 59: /* logical_and_expression: bitwise_or_expression  */
#line 304 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2496 "bison.tab.c"
    break;

  case 60: /* logical_and_expression: logical_and_expression AND bitwise_or_expression  */
#line 307 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2505 "bison.tab.c"
    break;

  case 61: /* bitwise_or_expression: bitwise_xor_expression  */
#line 314 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2513 "bison.tab.c"
    break;

  case 62: /* bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression  */
#line 317 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_BIT_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2522 "bison.tab.c"
    break;

  case 63: /* bitwise_xor_expression: bitwise_and_expression  */
#line 324 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2530 "bison.tab.c"
    break;

  case 64: /* bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression  */
#line 327 "./src/parser/gens/bison.y"
                                                        {
        ptree_t *op = ptree_create(PTREE_BIT_XOR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2539 "bison.tab.c"
    break;

  case 65: /* bitwise_and_expression: equality_expression  */
#line 334 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2547 "bison.tab.c"
    break;

  case 66: /* bitwise_and_expression: bitwise_and_expression '&' equality_expression  */
#line 337 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *op = ptree_create(PTREE_BIT_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2556 "bison.tab.c"
    break;

  case 67: /* equality_expression: relational_expression  */
#line 344 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2564 "bison.tab.c"
    break;

  case 68: /* equality_expression: equality_expression equality_operator relational_expression  */
#line 347 "./src/parser/gens/bison.y"
                                                                  {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2572 "bison.tab.c"
    break;

  case 69: /* equality_operator: IS_EQUAL  */
#line 353 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_IS_EQUAL, 0);
    }
#line 2580 "bison.tab.c"
    break;

  case 70: /* equality_operator: NOT_EQUAL  */
#line 356 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_NOT_EQUAL, 0);
    }
#line 2588 "bison.tab.c"
    break;

  case 71: /* relational_expression: shift_expression  */
#line 362 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2596 "bison.tab.c"
    break;

  case 72: /* relational_expression: relational_expression relational_operator shift_expression  */
#line 365 "./src/parser/gens/bison.y"
                                                                 {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2604 "bison.tab.c"
    break;

  case 73: /* relational_operator: '<'  */
#line 371 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_LESS, 0);
    }
#line 2612 "bison.tab.c"
    break;

  case 74: /* relational_operator: '>'  */
#line 374 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_GREATER, 0);
    }
#line 2620 "bison.tab.c"
    break;

  case 75: /* relational_operator: LESS_EQ  */
#line 377 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_LESS_EQ, 0);
    }
#line 2628 "bison.tab.c"
    break;

  case 76: /* relational_operator: GREATER_EQ  */
#line 380 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_GREATER_EQ, 0);
    }
#line 2636 "bison.tab.c"
    break;

  case 77: /* shift_expression: additive_expression  */
#line 386 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2644 "bison.tab.c"
    break;

  case 78: /* shift_expression: shift_expression shift_operator additive_expression  */
#line 389 "./src/parser/gens/bison.y"
                                                          {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2652 "bison.tab.c"
    break;

  case 79: /* shift_operator: LSHIFT  */
#line 395 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create(PTREE_LSHIFT, 0);
    }
#line 2660 "bison.tab.c"
    break;

  case 80: /* shift_operator: RSHIFT  */
#line 398 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_RSHIFT, 0);
    }
#line 2668 "bison.tab.c"
    break;

  case 81: /* additive_expression: multiplicative_expression  */
#line 404 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2676 "bison.tab.c"
    break;

  case 82: /* additive_expression: additive_expression additive_operator multiplicative_expression  */
#line 407 "./src/parser/gens/bison.y"
                                                                      {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2684 "bison.tab.c"
    break;

  case 83: /* additive_operator: '+'  */
#line 413 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2692 "bison.tab.c"
    break;

  case 84: /* additive_operator: '-'  */
#line 416 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2700 "bison.tab.c"
    break;

  case 85: /* multiplicative_expression: unary_expression  */
#line 422 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2708 "bison.tab.c"
    break;

  case 86: /* multiplicative_expression: multiplicative_expression multiplicative_operator unary_expression  */
#line 425 "./src/parser/gens/bison.y"
                                                                         {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2716 "bison.tab.c"
    break;

  case 87: /* multiplicative_operator: '*'  */
#line 431 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_MULTIPLY, 0);
    }
#line 2724 "bison.tab.c"
    break;

  case 88: /* multiplicative_operator: '/'  */
#line 434 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_DIVIDE, 0);
    }
#line 2732 "bison.tab.c"
    break;

  case 89: /* multiplicative_operator: '%'  */
#line 437 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MODULO, 0);
    }
#line 2740 "bison.tab.c"
    break;

  case 90: /* unary_expression: postfix_expression  */
#line 443 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2748 "bison.tab.c"
    break;

  case 91: /* unary_expression: unary_operator unary_expression  */
#line 446 "./src/parser/gens/bison.y"
                                      {
        (yyval.ptree) = ptree_create(PTREE_UNARY, 2, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2756 "bison.tab.c"
    break;

  case 92: /* unary_operator: INCREMENT  */
#line 452 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2764 "bison.tab.c"
    break;

  case 93: /* unary_operator: DECREMENT  */
#line 455 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2772 "bison.tab.c"
    break;

  case 94: /* unary_operator: '!'  */
#line 458 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_NOT, 0);
    }
#line 2780 "bison.tab.c"
    break;

  case 95: /* unary_operator: '~'  */
#line 461 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_TILDE, 0);
    }
#line 2788 "bison.tab.c"
    break;

  case 96: /* unary_operator: '+'  */
#line 464 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2796 "bison.tab.c"
    break;

  case 97: /* unary_operator: '-'  */
#line 467 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2804 "bison.tab.c"
    break;

  case 98: /* unary_operator: '&'  */
#line 470 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_BIT_AND, 0);
    }
#line 2812 "bison.tab.c"
    break;

  case 99: /* postfix_expression: primary_expression  */
#line 476 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2820 "bison.tab.c"
    break;

  case 100: /* postfix_expression: postfix_expression postfix_operator  */
#line 479 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_create(PTREE_POSTFIX, 2, (yyvsp[0].ptree), (yyvsp[-1].ptree));
    }
#line 2828 "bison.tab.c"
    break;

  case 101: /* postfix_expression: postfix_expression '.' WORD  */
#line 482 "./src/parser/gens/bison.y"
                                  {
        ptree_t *w = ptree_create_word((yyvsp[0].str));
        ptree_t *dot = ptree_create(PTREE_DOT, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_ACCESS, 3, (yyvsp[-2].ptree), dot, w);
    }
#line 2838 "bison.tab.c"
    break;

  case 102: /* postfix_expression: postfix_expression '[' expression ']'  */
#line 487 "./src/parser/gens/bison.y"
                                            {
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_ACCESS, 3, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2848 "bison.tab.c"
    break;

  case 103: /* postfix_expression: postfix_expression '(' arguments ')'  */
#line 492 "./src/parser/gens/bison.y"
                                           {
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_CALL, 4, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2858 "bison.tab.c"
    break;

  case 104: /* arguments: assignment_expression  */
#line 500 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ARGUMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2866 "bison.tab.c"
    break;

  case 105: /* arguments: arguments ',' assignment_expression  */
#line 503 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2874 "bison.tab.c"
    break;

  case 106: /* arguments: %empty  */
#line 506 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2882 "bison.tab.c"
    break;

  case 107: /* postfix_operator: INCREMENT  */
#line 512 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2890 "bison.tab.c"
    break;

  case 108: /* postfix_operator: DECREMENT  */
#line 515 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2898 "bison.tab.c"
    break;

  case 109: /* primary_expression: qualified_identifier  */
#line 521 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2906 "bison.tab.c"
    break;

  case 110: /* primary_expression: literal  */
#line 524 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2914 "bison.tab.c"
    break;

  case 111: /* primary_expression: array_literal  */
#line 527 "./src/parser/gens/bison.y"
                    {
       (yyval.ptree) = (yyvsp[0].ptree); 
    }
#line 2922 "bison.tab.c"
    break;

  case 112: /* primary_expression: object_literal  */
#line 530 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2930 "bison.tab.c"
    break;

  case 113: /* primary_expression: '(' expression ')'  */
#line 533 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2940 "bison.tab.c"
    break;

  case 114: /* qualified_identifier: WORD  */
#line 541 "./src/parser/gens/bison.y"
         {
        ptree_t *symbol =  ptree_create_word((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_SYMBOL, 1, symbol);
    }
#line 2949 "bison.tab.c"
    break;

  case 115: /* qualified_identifier: qualified_identifier SCOPE WORD  */
#line 545 "./src/parser/gens/bison.y"
                                      {
        ptree_t *symbol =  ptree_create_word((yyvsp[0].str));
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, symbol);
    }
#line 2958 "bison.tab.c"
    break;

  case 116: /* qualified_identifier: SCOPE WORD  */
#line 549 "./src/parser/gens/bison.y"
                 {
        ptree_t *root =  ptree_create_word("root");
        ptree_t *symbol =  ptree_create_word((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_SYMBOL, 2, root, symbol);
    }
#line 2968 "bison.tab.c"
    break;

  case 117: /* literal: NUMBER  */
#line 557 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2976 "bison.tab.c"
    break;

  case 118: /* literal: TRUE_VAL  */
#line 560 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create_bool(1);
    }
#line 2984 "bison.tab.c"
    break;

  case 119: /* literal: FALSE_VAL  */
#line 563 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create_bool(0);
    }
#line 2992 "bison.tab.c"
    break;

  case 120: /* literal: STRING  */
#line 566 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 3000 "bison.tab.c"
    break;

  case 121: /* array_literal: '[' element_list ']'  */
#line 572 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *right = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_LITERAL, 3, left, (yyvsp[-1].ptree), right);
    }
#line 3010 "bison.tab.c"
    break;

  case 122: /* element_list: assignment_expression  */
#line 580 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ELEMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 3018 "bison.tab.c"
    break;

  case 123: /* element_list: element_list ',' assignment_expression  */
#line 583 "./src/parser/gens/bison.y"
                                             {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3026 "bison.tab.c"
    break;

  case 124: /* element_list: element_list ','  */
#line 586 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3034 "bison.tab.c"
    break;

  case 125: /* element_list: %empty  */
#line 589 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 3042 "bison.tab.c"
    break;

  case 126: /* object_literal: qualified_identifier '{' keyvalue_list '}'  */
#line 595 "./src/parser/gens/bison.y"
                                               {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_LITERAL, 4, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3052 "bison.tab.c"
    break;

  case 127: /* keyvalue_list: keyvalue  */
#line 603 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3060 "bison.tab.c"
    break;

  case 128: /* keyvalue_list: keyvalue_list ',' keyvalue  */
#line 606 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3068 "bison.tab.c"
    break;

  case 129: /* keyvalue_list: keyvalue_list ','  */
#line 609 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3076 "bison.tab.c"
    break;

  case 130: /* keyvalue_list: %empty  */
#line 612 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 3084 "bison.tab.c"
    break;

  case 131: /* keyvalue: property_name ':' assignment_expression  */
#line 618 "./src/parser/gens/bison.y"
                                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 3093 "bison.tab.c"
    break;

  case 132: /* property_name: WORD  */
#line 625 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_word((yyvsp[0].str));
    }
#line 3101 "bison.tab.c"
    break;

  case 133: /* property_name: STRING  */
#line 628 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 3109 "bison.tab.c"
    break;

  case 134: /* property_name: NUMBER  */
#line 631 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 3117 "bison.tab.c"
    break;

  case 135: /* block_statement: '{' source_elements '}'  */
#line 637 "./src/parser/gens/bison.y"
                            {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, (yyvsp[-1].ptree), right);
    }
#line 3128 "bison.tab.c"
    break;

  case 136: /* block_statement: '{' '}'  */
#line 643 "./src/parser/gens/bison.y"
              {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, empty, right);
    }
#line 3139 "bison.tab.c"
    break;

  case 137: /* jump_statement: return_statement  */
#line 652 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3147 "bison.tab.c"
    break;

  case 138: /* jump_statement: break_statement  */
#line 655 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3155 "bison.tab.c"
    break;

  case 139: /* jump_statement: continue_statement  */
#line 658 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3163 "bison.tab.c"
    break;

  case 140: /* return_statement: RETURN ';'  */
#line 663 "./src/parser/gens/bison.y"
               {
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, empty);
    }
#line 3172 "bison.tab.c"
    break;

  case 141: /* return_statement: RETURN expression ';'  */
#line 667 "./src/parser/gens/bison.y"
                            {
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, (yyvsp[-1].ptree));
    }
#line 3180 "bison.tab.c"
    break;

  case 142: /* break_statement: BREAK ';'  */
#line 673 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_BREAK, 0);
    }
#line 3188 "bison.tab.c"
    break;

  case 143: /* continue_statement: CONTINUE ';'  */
#line 679 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_CONTINUE, 0);
    }
#line 3196 "bison.tab.c"
    break;

  case 144: /* labeled_statement: WORD ':' statement  */
#line 685 "./src/parser/gens/bison.y"
                       {
        ptree_t *w = ptree_create_word((yyvsp[-2].str));
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LABEL, 3, w, c, (yyvsp[0].ptree));
    }
#line 3206 "bison.tab.c"
    break;

  case 145: /* labeled_statement: CASE constant_expression ':' statement  */
#line 690 "./src/parser/gens/bison.y"
                                             {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CASE_LABEL, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 3215 "bison.tab.c"
    break;

  case 146: /* labeled_statement: DEFAULT ':' statement  */
#line 694 "./src/parser/gens/bison.y"
                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_DEFAULT_LABEL, 2, c, (yyvsp[0].ptree));
    }
#line 3224 "bison.tab.c"
    break;

  case 147: /* selection_statement: IF '(' expression ')' statement  */
#line 702 "./src/parser/gens/bison.y"
    {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3235 "bison.tab.c"
    break;

  case 148: /* selection_statement: IF '(' expression ')' statement ELSE statement  */
#line 708 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *e = ptree_create(PTREE_ELSE, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF_ELSE, 6, left, (yyvsp[-4].ptree), right, (yyvsp[-2].ptree), e, (yyvsp[0].ptree));
    }
#line 3247 "bison.tab.c"
    break;

  case 149: /* selection_statement: SWITCH '(' expression ')' statement  */
#line 715 "./src/parser/gens/bison.y"
                                          {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_SWITCH, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3258 "bison.tab.c"
    break;

  case 150: /* iteration_statement: WHILE '(' expression ')' statement  */
#line 724 "./src/parser/gens/bison.y"
                                       {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_WHILE, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3268 "bison.tab.c"
    break;

  case 151: /* iteration_statement: DO statement WHILE '(' expression ')' ';'  */
#line 729 "./src/parser/gens/bison.y"
                                                {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *wh = ptree_create(PTREE_WHILE, 0);

        (yyval.ptree) = ptree_create(PTREE_DO, 6, (yyvsp[-5].ptree), wh, left, (yyvsp[-2].ptree), right, sc);
    }
#line 3281 "bison.tab.c"
    break;

  case 152: /* iteration_statement: FOR '(' for_initialization ';' optional_expression ';' optional_expression ')' statement  */
#line 737 "./src/parser/gens/bison.y"
                                                                                               {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *sc2 = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_FOR, 8, left, (yyvsp[-6].ptree), sc, (yyvsp[-4].ptree), sc2, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3293 "bison.tab.c"
    break;

  case 153: /* for_initialization: optional_expression  */
#line 747 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3301 "bison.tab.c"
    break;

  case 154: /* for_initialization: variable_statement  */
#line 750 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3309 "bison.tab.c"
    break;

  case 155: /* variable_statement: LET variable_list  */
#line 756 "./src/parser/gens/bison.y"
                      {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LET_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3318 "bison.tab.c"
    break;

  case 156: /* variable_statement: CONST variable_list  */
#line 760 "./src/parser/gens/bison.y"
                          {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CONST_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3327 "bison.tab.c"
    break;

  case 157: /* variable_list: variable  */
#line 767 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3335 "bison.tab.c"
    break;

  case 158: /* variable_list: variable_list ',' variable  */
#line 770 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3343 "bison.tab.c"
    break;

  case 159: /* variable: identifier  */
#line 776 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 3351 "bison.tab.c"
    break;

  case 160: /* variable: identifier '=' assignment_expression  */
#line 779 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 3360 "bison.tab.c"
    break;

  case 161: /* identifier: WORD type_annotation  */
#line 786 "./src/parser/gens/bison.y"
                         {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_IDENTIFIER, 2, w, (yyvsp[0].ptree));
    }
#line 3369 "bison.tab.c"
    break;

  case 162: /* type_annotation: ':' type  */
#line 793 "./src/parser/gens/bison.y"
             {
        ptree_t *sc = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ANNOTATION, 2, sc, (yyvsp[0].ptree)); 
    }
#line 3378 "bison.tab.c"
    break;

  case 163: /* type_annotation: %empty  */
#line 797 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3386 "bison.tab.c"
    break;

  case 164: /* type: primitive_type  */
#line 803 "./src/parser/gens/bison.y"
                   {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3394 "bison.tab.c"
    break;

  case 165: /* type: array_type  */
#line 806 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3402 "bison.tab.c"
    break;

  case 166: /* type: tuple_type  */
#line 809 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3410 "bison.tab.c"
    break;

  case 167: /* type: function_type  */
#line 812 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = (yyvsp[0].ptree);        
    }
#line 3418 "bison.tab.c"
    break;

  case 168: /* type: type_reference  */
#line 815 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3426 "bison.tab.c"
    break;

  case 169: /* primitive_type: T_VOID  */
#line 821 "./src/parser/gens/bison.y"
           { 
        (yyval.ptree) = ptree_create(PTREE_VOID_TYPE, 0);
    }
#line 3434 "bison.tab.c"
    break;

  case 170: /* primitive_type: S32  */
#line 824 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_INTEGER_TYPE, 0);
    }
#line 3442 "bison.tab.c"
    break;

  case 171: /* primitive_type: T_STRING  */
#line 827 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_STRING_TYPE, 0);
    }
#line 3450 "bison.tab.c"
    break;

  case 172: /* primitive_type: T_BOOLEAN  */
#line 830 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_BOOLEAN_TYPE, 0);
    }
#line 3458 "bison.tab.c"
    break;

  case 173: /* array_type: type '[' ']'  */
#line 836 "./src/parser/gens/bison.y"
                 {
        ptree_t *lb = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_TYPE, 3, (yyvsp[-2].ptree), lb, rb);
    }
#line 3468 "bison.tab.c"
    break;

  case 174: /* tuple_type: type '[' NUMBER ']'  */
#line 844 "./src/parser/gens/bison.y"
                        {
        ptree_t *num = ptree_create_num((yyvsp[-1].num));
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_TUPLE_TYPE, 4, (yyvsp[-3].ptree), lb, num, rb);
    }
#line 3479 "bison.tab.c"
    break;

  case 175: /* function_type: type BACKWARD_ARROW '(' types_list ')'  */
#line 853 "./src/parser/gens/bison.y"
                                           {
        ptree_t *a = ptree_create(PTREE_BACKWARD_ARROW, 0);
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_TYPE, 5, (yyvsp[-4].ptree), a, lb, (yyvsp[-1].ptree), rb);
    }
#line 3490 "bison.tab.c"
    break;

  case 176: /* types_list: type  */
#line 862 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create(PTREE_TYPE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3498 "bison.tab.c"
    break;

  case 177: /* types_list: types_list ',' type  */
#line 865 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3506 "bison.tab.c"
    break;

  case 178: /* types_list: %empty  */
#line 868 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0); 
    }
#line 3514 "bison.tab.c"
    break;

  case 179: /* type_reference: WORD  */
#line 874 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 1, w);
    }
#line 3523 "bison.tab.c"
    break;

  case 180: /* type_reference: WORD typed_arguments  */
#line 878 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 2, w, (yyvsp[0].ptree));
    }
#line 3532 "bison.tab.c"
    break;

  case 181: /* typed_arguments: '<' types_list '>'  */
#line 885 "./src/parser/gens/bison.y"
                       {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_ARGUMENTS, 3, l, (yyvsp[-1].ptree), g);
    }
#line 3542 "bison.tab.c"
    break;

  case 182: /* typed_parameters: '<' type_params '>'  */
#line 893 "./src/parser/gens/bison.y"
                        {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_PARAMETERS, 3, l, (yyvsp[-1].ptree), g); 
    }
#line 3552 "bison.tab.c"
    break;

  case 183: /* typed_parameters: %empty  */
#line 898 "./src/parser/gens/bison.y"
                  {
         (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3560 "bison.tab.c"
    break;

  case 184: /* type_params: WORD  */
#line 904 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_PARAMS, 1, w); 
    }
#line 3569 "bison.tab.c"
    break;

  case 185: /* type_params: type_params ',' WORD  */
#line 908 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, w);
    }
#line 3578 "bison.tab.c"
    break;

  case 186: /* type_statement: TYPE WORD typed_parameters '=' type ';'  */
#line 915 "./src/parser/gens/bison.y"
                                            {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ALIAS, 5, w, (yyvsp[-3].ptree), eq, (yyvsp[-1].ptree), sc); 
    }
#line 3589 "bison.tab.c"
    break;

  case 187: /* struct_statement: STRUCT WORD ';'  */
#line 924 "./src/parser/gens/bison.y"
                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_UNIT_STRUCT, 1, w);
    }
#line 3598 "bison.tab.c"
    break;

  case 188: /* struct_statement: STRUCT WORD typed_parameters '{' field_list '}'  */
#line 928 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_STRUCT, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3609 "bison.tab.c"
    break;

  case 189: /* union_statement: UNION WORD typed_parameters '{' field_list '}'  */
#line 937 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_UNION, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3620 "bison.tab.c"
    break;

  case 190: /* field_list: identifier  */
#line 946 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_FIELDS, 1, (yyvsp[0].ptree));
    }
#line 3628 "bison.tab.c"
    break;

  case 191: /* field_list: field_list ',' identifier  */
#line 949 "./src/parser/gens/bison.y"
                                {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3636 "bison.tab.c"
    break;

  case 192: /* field_list: field_list ','  */
#line 952 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3644 "bison.tab.c"
    break;

  case 193: /* field_list: %empty  */
#line 955 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3652 "bison.tab.c"
    break;

  case 194: /* enum_statement: ENUM WORD typed_parameters '{' enum_members '}'  */
#line 961 "./src/parser/gens/bison.y"
                                                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3663 "bison.tab.c"
    break;

  case 195: /* enum_members: enum_member  */
#line 970 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_ENUM_MEMBERS, 1, (yyvsp[0].ptree));
    }
#line 3671 "bison.tab.c"
    break;

  case 196: /* enum_members: enum_members ',' enum_member  */
#line 973 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3679 "bison.tab.c"
    break;

  case 197: /* enum_members: enum_members ','  */
#line 976 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3687 "bison.tab.c"
    break;

  case 198: /* enum_member: WORD  */
#line 982 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_word((yyvsp[0].str));
    }
#line 3695 "bison.tab.c"
    break;

  case 199: /* enum_member: WORD '(' types_list ')'  */
#line 985 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_word((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LPRN, 0);
        ptree_t *right = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3706 "bison.tab.c"
    break;

  case 200: /* enum_member: WORD '{' field_list '}'  */
#line 991 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_word((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_NAMED_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3717 "bison.tab.c"
    break;

  case 201: /* enum_member: WORD '=' NUMBER  */
#line 997 "./src/parser/gens/bison.y"
                      {
        ptree_t *w = ptree_create_word((yyvsp[-2].str));
        ptree_t *num = ptree_create_num((yyvsp[0].num));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_DEFAULT, 3, w, eq, num);
    }
#line 3728 "bison.tab.c"
    break;

  case 202: /* methods_statement: METHODS qualified_identifier '{' method_list '}'  */
#line 1006 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_METHODS, 4, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3738 "bison.tab.c"
    break;

  case 203: /* method_list: function_declaration  */
#line 1014 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = ptree_create(PTREE_METHODS_LIST, 1, (yyvsp[0].ptree));
    }
#line 3746 "bison.tab.c"
    break;

  case 204: /* method_list: method_list function_declaration  */
#line 1017 "./src/parser/gens/bison.y"
                                       {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 3754 "bison.tab.c"
    break;

  case 205: /* class_statement: CLASS WORD typed_parameters '{' field_list '}'  */
#line 1023 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_CLASS, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3765 "bison.tab.c"
    break;

  case 206: /* export_statement: EXPORT function_declaration  */
#line 1032 "./src/parser/gens/bison.y"
                                {
        (yyval.ptree) = ptree_create(PTREE_EXPORT, 1, (yyvsp[0].ptree));
    }
#line 3773 "bison.tab.c"
    break;

  case 207: /* export_statement: EXPORT struct_statement  */
#line 1035 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = ptree_create(PTREE_EXPORT, 1, (yyvsp[0].ptree));
    }
#line 3781 "bison.tab.c"
    break;

  case 208: /* export_statement: EXPORT module_statement  */
#line 1038 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = ptree_create(PTREE_EXPORT, 1, (yyvsp[0].ptree));
    }
#line 3789 "bison.tab.c"
    break;

  case 209: /* import_statement: IMPORT WORD ';'  */
#line 1044 "./src/parser/gens/bison.y"
                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_IMPORT, 1, w);
    }
#line 3798 "bison.tab.c"
    break;

  case 210: /* module_statement: MODULE WORD '{' source_elements '}'  */
#line 1051 "./src/parser/gens/bison.y"
                                        {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        (yyval.ptree) = ptree_create(PTREE_MODULE, 2, w, (yyvsp[-1].ptree));
    }
#line 3807 "bison.tab.c"
    break;


#line 3811 "bison.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 1057 "./src/parser/gens/bison.y"
