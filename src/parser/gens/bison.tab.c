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

#include "parser/ptree.h"
#include "parser/error.h"
#include "parser/symtab.h"

int yylex();
void yyerror(const char *msg);

extern ptree_t *ptree_g;

#line 85 "bison.tab.c"

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
  YYSYMBOL_LET = 32,                       /* LET  */
  YYSYMBOL_MUT = 33,                       /* MUT  */
  YYSYMBOL_FUN = 34,                       /* FUN  */
  YYSYMBOL_RETURN = 35,                    /* RETURN  */
  YYSYMBOL_IF = 36,                        /* IF  */
  YYSYMBOL_ELSE = 37,                      /* ELSE  */
  YYSYMBOL_BREAK = 38,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 39,                  /* CONTINUE  */
  YYSYMBOL_DO = 40,                        /* DO  */
  YYSYMBOL_FOR = 41,                       /* FOR  */
  YYSYMBOL_WHILE = 42,                     /* WHILE  */
  YYSYMBOL_SWITCH = 43,                    /* SWITCH  */
  YYSYMBOL_CASE = 44,                      /* CASE  */
  YYSYMBOL_DEFAULT = 45,                   /* DEFAULT  */
  YYSYMBOL_TYPE = 46,                      /* TYPE  */
  YYSYMBOL_CONST = 47,                     /* CONST  */
  YYSYMBOL_STRUCT = 48,                    /* STRUCT  */
  YYSYMBOL_UNION = 49,                     /* UNION  */
  YYSYMBOL_TRUE_VAL = 50,                  /* TRUE_VAL  */
  YYSYMBOL_ENUM = 51,                      /* ENUM  */
  YYSYMBOL_METHODS = 52,                   /* METHODS  */
  YYSYMBOL_MATCH = 53,                     /* MATCH  */
  YYSYMBOL_FALSE_VAL = 54,                 /* FALSE_VAL  */
  YYSYMBOL_S8 = 55,                        /* S8  */
  YYSYMBOL_S16 = 56,                       /* S16  */
  YYSYMBOL_S32 = 57,                       /* S32  */
  YYSYMBOL_S64 = 58,                       /* S64  */
  YYSYMBOL_U8 = 59,                        /* U8  */
  YYSYMBOL_U16 = 60,                       /* U16  */
  YYSYMBOL_U32 = 61,                       /* U32  */
  YYSYMBOL_U64 = 62,                       /* U64  */
  YYSYMBOL_F32 = 63,                       /* F32  */
  YYSYMBOL_F64 = 64,                       /* F64  */
  YYSYMBOL_T_STRING = 65,                  /* T_STRING  */
  YYSYMBOL_T_BOOLEAN = 66,                 /* T_BOOLEAN  */
  YYSYMBOL_T_VOID = 67,                    /* T_VOID  */
  YYSYMBOL_T_CHAR = 68,                    /* T_CHAR  */
  YYSYMBOL_69_ = 69,                       /* '('  */
  YYSYMBOL_70_ = 70,                       /* ')'  */
  YYSYMBOL_LOWER_THAN_ELSE = 71,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_72_ = 72,                       /* ','  */
  YYSYMBOL_73_ = 73,                       /* '='  */
  YYSYMBOL_74_ = 74,                       /* ';'  */
  YYSYMBOL_75_ = 75,                       /* '?'  */
  YYSYMBOL_76_ = 76,                       /* ':'  */
  YYSYMBOL_77_ = 77,                       /* '|'  */
  YYSYMBOL_78_ = 78,                       /* '^'  */
  YYSYMBOL_79_ = 79,                       /* '&'  */
  YYSYMBOL_80_ = 80,                       /* '<'  */
  YYSYMBOL_81_ = 81,                       /* '>'  */
  YYSYMBOL_82_ = 82,                       /* '+'  */
  YYSYMBOL_83_ = 83,                       /* '-'  */
  YYSYMBOL_84_ = 84,                       /* '*'  */
  YYSYMBOL_85_ = 85,                       /* '/'  */
  YYSYMBOL_86_ = 86,                       /* '%'  */
  YYSYMBOL_87_ = 87,                       /* '!'  */
  YYSYMBOL_88_ = 88,                       /* '~'  */
  YYSYMBOL_89_ = 89,                       /* '.'  */
  YYSYMBOL_90_ = 90,                       /* '['  */
  YYSYMBOL_91_ = 91,                       /* ']'  */
  YYSYMBOL_92_ = 92,                       /* '{'  */
  YYSYMBOL_93_ = 93,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 94,                  /* $accept  */
  YYSYMBOL_program = 95,                   /* program  */
  YYSYMBOL_source_elements = 96,           /* source_elements  */
  YYSYMBOL_source_element = 97,            /* source_element  */
  YYSYMBOL_function_declaration = 98,      /* function_declaration  */
  YYSYMBOL_parameters_list = 99,           /* parameters_list  */
  YYSYMBOL_parameter_is_mut = 100,         /* parameter_is_mut  */
  YYSYMBOL_parameter = 101,                /* parameter  */
  YYSYMBOL_statement = 102,                /* statement  */
  YYSYMBOL_expression_statement = 103,     /* expression_statement  */
  YYSYMBOL_expression = 104,               /* expression  */
  YYSYMBOL_optional_expression = 105,      /* optional_expression  */
  YYSYMBOL_assignment_expression = 106,    /* assignment_expression  */
  YYSYMBOL_assignment_operator = 107,      /* assignment_operator  */
  YYSYMBOL_function_expression = 108,      /* function_expression  */
  YYSYMBOL_function_expression_body = 109, /* function_expression_body  */
  YYSYMBOL_constant_expression = 110,      /* constant_expression  */
  YYSYMBOL_conditional_expression = 111,   /* conditional_expression  */
  YYSYMBOL_logical_or_expression = 112,    /* logical_or_expression  */
  YYSYMBOL_logical_and_expression = 113,   /* logical_and_expression  */
  YYSYMBOL_bitwise_or_expression = 114,    /* bitwise_or_expression  */
  YYSYMBOL_bitwise_xor_expression = 115,   /* bitwise_xor_expression  */
  YYSYMBOL_bitwise_and_expression = 116,   /* bitwise_and_expression  */
  YYSYMBOL_equality_expression = 117,      /* equality_expression  */
  YYSYMBOL_equality_operator = 118,        /* equality_operator  */
  YYSYMBOL_relational_expression = 119,    /* relational_expression  */
  YYSYMBOL_relational_operator = 120,      /* relational_operator  */
  YYSYMBOL_shift_expression = 121,         /* shift_expression  */
  YYSYMBOL_shift_operator = 122,           /* shift_operator  */
  YYSYMBOL_additive_expression = 123,      /* additive_expression  */
  YYSYMBOL_additive_operator = 124,        /* additive_operator  */
  YYSYMBOL_multiplicative_expression = 125, /* multiplicative_expression  */
  YYSYMBOL_multiplicative_operator = 126,  /* multiplicative_operator  */
  YYSYMBOL_unary_expression = 127,         /* unary_expression  */
  YYSYMBOL_unary_operator = 128,           /* unary_operator  */
  YYSYMBOL_postfix_expression = 129,       /* postfix_expression  */
  YYSYMBOL_arguments = 130,                /* arguments  */
  YYSYMBOL_postfix_operator = 131,         /* postfix_operator  */
  YYSYMBOL_primary_expression = 132,       /* primary_expression  */
  YYSYMBOL_literal = 133,                  /* literal  */
  YYSYMBOL_array_literal = 134,            /* array_literal  */
  YYSYMBOL_element_list = 135,             /* element_list  */
  YYSYMBOL_object_literal = 136,           /* object_literal  */
  YYSYMBOL_keyvalue_list = 137,            /* keyvalue_list  */
  YYSYMBOL_keyvalue = 138,                 /* keyvalue  */
  YYSYMBOL_property_name = 139,            /* property_name  */
  YYSYMBOL_block_statement = 140,          /* block_statement  */
  YYSYMBOL_jump_statement = 141,           /* jump_statement  */
  YYSYMBOL_return_statement = 142,         /* return_statement  */
  YYSYMBOL_break_statement = 143,          /* break_statement  */
  YYSYMBOL_continue_statement = 144,       /* continue_statement  */
  YYSYMBOL_labeled_statement = 145,        /* labeled_statement  */
  YYSYMBOL_selection_statement = 146,      /* selection_statement  */
  YYSYMBOL_iteration_statement = 147,      /* iteration_statement  */
  YYSYMBOL_for_initialization = 148,       /* for_initialization  */
  YYSYMBOL_variable_statement = 149,       /* variable_statement  */
  YYSYMBOL_variable_list = 150,            /* variable_list  */
  YYSYMBOL_variable = 151,                 /* variable  */
  YYSYMBOL_identifier = 152,               /* identifier  */
  YYSYMBOL_type_annotation = 153,          /* type_annotation  */
  YYSYMBOL_type = 154,                     /* type  */
  YYSYMBOL_primitive_type = 155,           /* primitive_type  */
  YYSYMBOL_array_type = 156,               /* array_type  */
  YYSYMBOL_tuple_type = 157,               /* tuple_type  */
  YYSYMBOL_function_type = 158,            /* function_type  */
  YYSYMBOL_types_list = 159,               /* types_list  */
  YYSYMBOL_type_reference = 160,           /* type_reference  */
  YYSYMBOL_typed_arguments = 161,          /* typed_arguments  */
  YYSYMBOL_typed_parameters = 162,         /* typed_parameters  */
  YYSYMBOL_type_params = 163,              /* type_params  */
  YYSYMBOL_type_statement = 164,           /* type_statement  */
  YYSYMBOL_struct_statement = 165,         /* struct_statement  */
  YYSYMBOL_union_statement = 166,          /* union_statement  */
  YYSYMBOL_field_list = 167,               /* field_list  */
  YYSYMBOL_enum_statement = 168,           /* enum_statement  */
  YYSYMBOL_enum_members = 169,             /* enum_members  */
  YYSYMBOL_enum_member = 170,              /* enum_member  */
  YYSYMBOL_methods_statement = 171,        /* methods_statement  */
  YYSYMBOL_method_list = 172               /* method_list  */
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
#define YYFINAL  111
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   653

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  94
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  79
/* YYNRULES -- Number of rules.  */
#define YYNRULES  197
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  326

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   324


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
       2,     2,     2,    87,     2,     2,     2,    86,    79,     2,
      69,    70,    84,    82,    72,    83,    89,    85,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    76,    74,
      80,    73,    81,    75,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    90,     2,    91,    78,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    92,    77,    93,    88,     2,     2,     2,
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
      65,    66,    67,    68,    71
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    65,    65,    69,    75,    78,    84,    87,    93,   102,
     105,   108,   114,   117,   123,   126,   133,   136,   139,   142,
     145,   148,   151,   154,   157,   160,   163,   166,   173,   180,
     183,   189,   192,   198,   201,   204,   210,   213,   216,   219,
     222,   225,   228,   231,   234,   237,   240,   246,   255,   258,
     264,   270,   273,   281,   284,   291,   294,   301,   304,   311,
     314,   321,   324,   331,   334,   340,   343,   349,   352,   358,
     361,   364,   367,   373,   376,   382,   385,   391,   394,   400,
     403,   409,   412,   418,   421,   424,   430,   433,   439,   442,
     445,   448,   451,   454,   457,   463,   466,   469,   474,   479,
     487,   490,   493,   499,   502,   508,   511,   514,   517,   520,
     528,   531,   534,   537,   543,   551,   554,   557,   560,   566,
     575,   578,   581,   584,   590,   597,   600,   603,   609,   615,
     624,   627,   630,   635,   639,   645,   651,   657,   662,   666,
     673,   680,   687,   696,   701,   709,   719,   722,   728,   732,
     739,   742,   748,   751,   758,   765,   769,   775,   778,   781,
     784,   787,   793,   796,   799,   802,   808,   816,   825,   834,
     837,   840,   846,   850,   857,   865,   870,   876,   880,   887,
     896,   900,   909,   918,   921,   924,   927,   933,   942,   945,
     948,   954,   957,   963,   969,   978,   987,   990
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
  "AND_ASSIGN", "XOR_ASSIGN", "BACKWARD_ARROW", "FUN_ARROW", "LET", "MUT",
  "FUN", "RETURN", "IF", "ELSE", "BREAK", "CONTINUE", "DO", "FOR", "WHILE",
  "SWITCH", "CASE", "DEFAULT", "TYPE", "CONST", "STRUCT", "UNION",
  "TRUE_VAL", "ENUM", "METHODS", "MATCH", "FALSE_VAL", "S8", "S16", "S32",
  "S64", "U8", "U16", "U32", "U64", "F32", "F64", "T_STRING", "T_BOOLEAN",
  "T_VOID", "T_CHAR", "'('", "')'", "LOWER_THAN_ELSE", "','", "'='", "';'",
  "'?'", "':'", "'|'", "'^'", "'&'", "'<'", "'>'", "'+'", "'-'", "'*'",
  "'/'", "'%'", "'!'", "'~'", "'.'", "'['", "']'", "'{'", "'}'", "$accept",
  "program", "source_elements", "source_element", "function_declaration",
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
  "primary_expression", "literal", "array_literal", "element_list",
  "object_literal", "keyvalue_list", "keyvalue", "property_name",
  "block_statement", "jump_statement", "return_statement",
  "break_statement", "continue_statement", "labeled_statement",
  "selection_statement", "iteration_statement", "for_initialization",
  "variable_statement", "variable_list", "variable", "identifier",
  "type_annotation", "type", "primitive_type", "array_type", "tuple_type",
  "function_type", "types_list", "type_reference", "typed_arguments",
  "typed_parameters", "type_params", "type_statement", "struct_statement",
  "union_statement", "field_list", "enum_statement", "enum_members",
  "enum_member", "methods_statement", "method_list", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-244)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-33)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     413,  -244,  -244,   -50,  -244,  -244,    33,     9,   170,   -23,
     -24,   -13,   472,    19,    24,    32,   563,    41,   120,    33,
     129,   140,  -244,   149,   156,  -244,   522,  -244,  -244,  -244,
    -244,  -244,   522,   279,   130,   413,  -244,  -244,  -244,  -244,
     105,   107,  -244,  -244,  -244,     0,   180,   114,   117,   122,
      50,    -3,    56,    76,    64,   186,   563,    10,  -244,  -244,
    -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,
     125,  -244,  -244,  -244,  -244,  -244,   472,   131,   127,   133,
    -244,   143,   137,   212,   153,   136,   137,  -244,    71,   522,
    -244,  -244,   183,   110,   522,   522,   154,  -244,  -244,   472,
     137,   133,   -40,   137,   137,   139,    68,  -244,   -31,  -244,
     346,  -244,  -244,   522,  -244,   563,   522,   563,   563,   563,
     563,  -244,  -244,   563,  -244,  -244,  -244,  -244,   563,  -244,
    -244,   563,  -244,  -244,   563,  -244,  -244,  -244,   563,  -244,
    -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,  -244,
     522,  -244,  -244,  -244,   522,   223,   522,  -244,  -244,  -244,
    -244,  -244,  -244,   -48,  -244,   157,    66,  -244,    33,   522,
     166,  -244,   -10,    11,  -244,    81,   167,  -244,   168,  -244,
      84,    95,   472,  -244,   172,  -244,   148,   151,   155,   207,
    -244,   522,  -244,  -244,  -244,   180,   174,   114,   117,   122,
      50,    -3,    56,    76,    64,  -244,  -244,  -244,    96,  -244,
       4,   131,  -244,   522,   171,  -244,  -244,  -244,  -244,   -17,
    -244,  -244,  -244,  -244,  -244,  -244,  -244,    11,   240,  -244,
      33,    99,  -244,  -244,   175,   472,   522,   522,   472,   472,
    -244,    66,    33,    33,   248,   249,  -244,   -11,  -244,   522,
    -244,   522,  -244,  -244,  -244,    66,  -244,   187,     5,   102,
    -244,  -244,   127,    11,   522,   224,   108,   188,  -244,  -244,
     -20,  -244,   -42,   -41,   -44,   -35,  -244,  -244,  -244,  -244,
    -244,   -17,    40,    66,   173,  -244,   127,   244,  -244,  -244,
     472,   191,   522,  -244,    33,  -244,  -244,    66,   263,    33,
     248,  -244,    66,  -244,   112,  -244,   176,    15,  -244,  -244,
     197,  -244,   124,  -244,   -34,  -244,   -17,  -244,  -244,  -244,
    -244,  -244,   472,  -244,  -244,  -244
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,   110,   113,   105,    88,    89,     0,   176,     0,     0,
       0,     0,    32,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,   112,     0,    94,    92,    93,
      90,    91,   118,    32,     0,     2,     5,     6,     7,    16,
      31,     0,    29,    34,    35,    51,    53,    55,    57,    59,
      61,    63,    67,    73,    77,    81,     0,    86,    95,   106,
     107,   108,    17,    18,   130,   131,   132,    19,    20,    21,
       0,    23,    24,    25,    26,    27,    32,   123,   156,   148,
     150,   152,   176,     0,     0,   105,   176,   133,     0,     0,
     135,   136,     0,    32,     0,     0,     0,    50,    81,    32,
     176,   149,   176,   176,   176,     0,     0,   115,     0,   129,
      32,     1,     4,     0,    28,     0,     0,     0,     0,     0,
       0,    65,    66,     0,    71,    72,    69,    70,     0,    75,
      76,     0,    79,    80,     0,    83,    84,    85,     0,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    36,
       0,    87,   103,   104,   102,     0,     0,    96,    22,   137,
     127,   126,   125,     0,   120,     0,     0,   154,     0,     0,
       0,   177,     0,    11,   134,     0,     0,   146,     0,   147,
       0,     0,    32,   139,     0,   180,     0,     0,     0,     0,
     109,   117,   114,   128,    30,    54,     0,    56,    58,    60,
      62,    64,    68,    74,    78,    82,    33,   100,     0,    97,
       0,   122,   119,     0,   172,   163,   164,   165,   162,   155,
     157,   158,   159,   160,   161,   151,   153,    11,     0,   175,
       0,     0,     9,    12,    14,    32,     0,    32,    32,    32,
     138,     0,   186,   186,     0,     0,   196,     0,   116,     0,
      99,     0,    98,   121,   124,   171,   173,     0,     0,     0,
     178,    13,   156,     0,     0,   140,     0,     0,   143,   142,
       0,   183,     0,     0,   191,     0,   188,   195,   197,    52,
     101,   169,     0,   171,     0,   166,   156,     0,    10,    15,
      32,     0,    32,   179,   185,   181,   182,   171,     0,   186,
     190,   187,     0,   174,     0,   167,     0,     0,   141,   144,
       0,   184,     0,   194,     0,   189,   170,   168,     8,    48,
      47,    49,    32,   192,   193,   145
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -244,  -244,   236,   -29,  -173,    43,     8,    42,   -12,  -244,
      -4,   -90,   -30,  -244,  -244,  -244,  -244,   257,  -244,   159,
     158,   161,   162,   160,  -244,   163,  -244,   164,  -244,   145,
    -244,   165,  -244,    -9,  -244,  -244,  -244,  -244,  -244,  -244,
    -244,  -244,  -244,  -244,    72,  -244,  -121,  -244,  -244,  -244,
    -244,  -244,  -244,  -244,  -244,   194,   269,   123,    -5,  -243,
    -161,  -244,  -244,  -244,  -244,  -179,  -244,  -244,   -47,  -244,
    -244,  -244,  -244,  -207,  -244,  -244,    -7,  -244,  -244
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    34,    35,    36,    37,   231,   232,   233,    38,    39,
      40,    41,    42,   150,    43,   320,    96,    44,    45,    46,
      47,    48,    49,    50,   123,    51,   128,    52,   131,    53,
     134,    54,   138,    55,    56,    57,   208,   157,    58,    59,
      60,   108,    61,   163,   164,   165,    62,    63,    64,    65,
      66,    67,    68,    69,   178,    70,    79,    80,   234,   167,
     281,   220,   221,   222,   223,   282,   224,   256,    84,   172,
      71,    72,    73,   272,    74,   275,   276,    75,   247
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      92,    81,   107,   177,    88,   219,   112,    98,   284,   115,
     257,   124,   125,   257,    81,    82,   246,    78,     1,   287,
       2,    85,   106,   245,   211,   297,    76,   152,   153,   298,
     294,   294,     4,     5,   185,   170,   273,   300,   294,    78,
      83,   191,    77,   306,   230,   212,    89,   151,   299,    86,
      90,   295,   296,   184,   293,   186,   187,   188,   301,   324,
     192,    91,   228,   121,   159,    22,   122,   129,   130,    25,
     258,   229,   214,   258,   278,   116,   113,   126,   127,   154,
     270,   112,   277,   194,    26,   175,   196,   183,    93,    83,
     180,   181,   314,    94,    27,   252,   285,    28,    29,   155,
     156,    95,    30,    31,   304,    32,    98,    33,    98,    98,
      98,    98,   302,     1,    98,     2,    85,    99,   312,    98,
     206,   303,    98,   215,   207,    98,   100,     4,     5,   205,
     111,   216,   217,   218,   160,   102,   161,   162,   190,   226,
     113,   316,     6,   113,    86,   174,   103,   267,   135,   136,
     137,   235,   210,   113,   238,   104,   113,    19,   132,   133,
      22,   248,   105,    81,    25,   239,   250,   113,   251,   262,
     240,   263,   286,     1,   263,     2,    85,   113,   291,    26,
     113,   114,   317,   254,   302,   318,   321,     4,     5,    27,
     117,   118,    28,    29,   323,   119,   302,    30,    31,   158,
      32,   120,   310,   166,    86,   168,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   169,    83,   171,   279,
      22,   280,   173,   265,    25,   176,   268,   269,    77,   209,
     182,   189,   266,   213,   289,   227,   236,   271,   271,    26,
     242,   245,   237,   243,    87,   241,   260,   244,   264,    27,
     249,   255,    28,    29,   274,    82,   283,    30,    31,   149,
      32,   290,   292,   307,   305,   309,   313,   322,    33,   110,
     259,   288,   261,    97,   195,   197,   203,   319,   308,   198,
     200,   199,     1,   253,     2,     3,   201,   179,   101,   311,
       0,   225,   202,   315,   271,     0,     4,     5,     0,   204,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     325,     6,     0,     7,     8,     9,     0,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,     0,    25,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    26,     1,
       0,     2,     3,     0,     0,     0,     0,     0,    27,     0,
       0,    28,    29,     4,     5,     0,    30,    31,     0,    32,
       0,    33,   109,     0,     0,     0,     0,     0,     6,     0,
       7,     8,     9,     0,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,     0,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    26,     1,     0,     2,     3,
       0,     0,     0,     0,     0,    27,     0,     0,    28,    29,
       4,     5,     0,    30,    31,     0,    32,     0,    33,   193,
       0,     0,     0,     0,     0,     6,     0,     7,     8,     9,
       0,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,     0,    25,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     2,     3,     0,
       0,     0,    26,     0,     0,     0,     0,   -32,     0,     4,
       5,     0,    27,     0,     0,    28,    29,     0,     0,     0,
      30,    31,     0,    32,     6,    33,    86,     8,     9,     0,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,     1,    25,     2,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
       5,    26,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    27,     0,     0,    28,    29,    86,     0,     0,    30,
      31,     0,    32,     0,    33,     0,     1,     0,     2,    85,
       0,     0,    22,     0,     0,     0,    25,     0,     0,     0,
       4,     5,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    26,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    27,     0,     0,    28,    29,     0,     0,     0,    30,
      31,     0,    32,    22,     0,     0,     0,    25,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    27,     0,     0,    28,    29,     0,     0,     0,
      30,    31,     0,    32
};

static const yytype_int16 yycheck[] =
{
      12,     6,    32,    93,     8,   166,    35,    16,     3,     9,
      30,    14,    15,    30,    19,     6,   189,     6,     3,   262,
       5,     6,    26,    34,    72,    69,    76,    17,    18,    73,
      72,    72,    17,    18,    74,    82,   243,    72,    72,     6,
      80,    72,    92,   286,    33,    93,    69,    56,    92,    34,
      74,    93,    93,   100,    74,   102,   103,   104,    93,    93,
      91,    74,    72,    13,    76,    50,    16,    11,    12,    54,
      90,    81,     6,    90,   247,    75,    72,    80,    81,    69,
     241,   110,    93,   113,    69,    89,   116,    99,    69,    80,
      94,    95,   299,    69,    79,    91,    91,    82,    83,    89,
      90,    69,    87,    88,   283,    90,   115,    92,   117,   118,
     119,   120,    72,     3,   123,     5,     6,    76,   297,   128,
     150,    81,   131,    57,   154,   134,     6,    17,    18,   138,
       0,    65,    66,    67,     3,     6,     5,     6,    70,   169,
      72,   302,    32,    72,    34,    74,     6,   237,    84,    85,
      86,    70,   156,    72,    70,     6,    72,    47,    82,    83,
      50,   191,     6,   168,    54,    70,    70,    72,    72,    70,
     182,    72,    70,     3,    72,     5,     6,    72,    70,    69,
      72,    74,    70,   213,    72,   306,   307,    17,    18,    79,
      10,    77,    82,    83,    70,    78,    72,    87,    88,    74,
      90,    79,   292,    76,    34,    72,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    73,    80,     6,   249,
      50,   251,    69,   235,    54,    42,   238,   239,    92,     6,
      76,    92,   236,    76,   264,    69,    69,   242,   243,    69,
      92,    34,    74,    92,    74,    73,     6,    92,    73,    79,
      76,    80,    82,    83,     6,     6,    69,    87,    88,    73,
      90,    37,    74,    19,    91,    74,     3,    70,    92,    33,
     227,   263,   230,    16,   115,   117,   131,   307,   290,   118,
     120,   119,     3,   211,     5,     6,   123,    93,    19,   294,
      -1,   168,   128,   300,   299,    -1,    17,    18,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    32,    -1,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,     3,
      -1,     5,     6,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    82,    83,    17,    18,    -1,    87,    88,    -1,    90,
      -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    32,    -1,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,     3,    -1,     5,     6,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    83,
      17,    18,    -1,    87,    88,    -1,    90,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    32,    -1,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,     5,     6,    -1,
      -1,    -1,    69,    -1,    -1,    -1,    -1,    74,    -1,    17,
      18,    -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,
      87,    88,    -1,    90,    32,    92,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,     3,    54,     5,     6,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,
      18,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    83,    34,    -1,    -1,    87,
      88,    -1,    90,    -1,    92,    -1,     3,    -1,     5,     6,
      -1,    -1,    50,    -1,    -1,    -1,    54,    -1,    -1,    -1,
      17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,    87,
      88,    -1,    90,    50,    -1,    -1,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,
      87,    88,    -1,    90
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,     6,    17,    18,    32,    34,    35,    36,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    54,    69,    79,    82,    83,
      87,    88,    90,    92,    95,    96,    97,    98,   102,   103,
     104,   105,   106,   108,   111,   112,   113,   114,   115,   116,
     117,   119,   121,   123,   125,   127,   128,   129,   132,   133,
     134,   136,   140,   141,   142,   143,   144,   145,   146,   147,
     149,   164,   165,   166,   168,   171,    76,    92,     6,   150,
     151,   152,     6,    80,   162,     6,    34,    74,   104,    69,
      74,    74,   102,    69,    69,    69,   110,   111,   127,    76,
       6,   150,     6,     6,     6,     6,   104,   106,   135,    93,
      96,     0,    97,    72,    74,     9,    75,    10,    77,    78,
      79,    13,    16,   118,    14,    15,    80,    81,   120,    11,
      12,   122,    82,    83,   124,    84,    85,    86,   126,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    73,
     107,   127,    17,    18,    69,    89,    90,   131,    74,   102,
       3,     5,     6,   137,   138,   139,    76,   153,    72,    73,
     162,     6,   163,    69,    74,   104,    42,   105,   148,   149,
     104,   104,    76,   102,   162,    74,   162,   162,   162,    92,
      70,    72,    91,    93,   106,   113,   106,   114,   115,   116,
     117,   119,   121,   123,   125,   127,   106,   106,   130,     6,
     104,    72,    93,    76,     6,    57,    65,    66,    67,   154,
     155,   156,   157,   158,   160,   151,   106,    69,    72,    81,
      33,    99,   100,   101,   152,    70,    69,    74,    70,    70,
     102,    73,    92,    92,    92,    34,    98,   172,   106,    76,
      70,    72,    91,   138,   106,    80,   161,    30,    90,    99,
       6,   101,    70,    72,    73,   102,   104,   105,   102,   102,
     154,   152,   167,   167,     6,   169,   170,    93,    98,   106,
     106,   154,   159,    69,     3,    91,    70,   153,   100,   106,
      37,    70,    74,    74,    72,    93,    93,    69,    73,    92,
      72,    93,    72,    81,   159,    91,   153,    19,   102,    74,
     105,   152,   159,     3,   167,   170,   154,    70,   140,   106,
     109,   140,    70,    70,    93,   102
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    94,    95,    95,    96,    96,    97,    97,    98,    99,
      99,    99,   100,   100,   101,   101,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   103,   104,
     104,   105,   105,   106,   106,   106,   107,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   107,   108,   109,   109,
     110,   111,   111,   112,   112,   113,   113,   114,   114,   115,
     115,   116,   116,   117,   117,   118,   118,   119,   119,   120,
     120,   120,   120,   121,   121,   122,   122,   123,   123,   124,
     124,   125,   125,   126,   126,   126,   127,   127,   128,   128,
     128,   128,   128,   128,   128,   129,   129,   129,   129,   129,
     130,   130,   130,   131,   131,   132,   132,   132,   132,   132,
     133,   133,   133,   133,   134,   135,   135,   135,   135,   136,
     137,   137,   137,   137,   138,   139,   139,   139,   140,   140,
     141,   141,   141,   142,   142,   143,   144,   145,   145,   145,
     146,   146,   146,   147,   147,   147,   148,   148,   149,   149,
     150,   150,   151,   151,   152,   153,   153,   154,   154,   154,
     154,   154,   155,   155,   155,   155,   156,   157,   158,   159,
     159,   159,   160,   160,   161,   162,   162,   163,   163,   164,
     165,   165,   166,   167,   167,   167,   167,   168,   169,   169,
     169,   170,   170,   170,   170,   171,   172,   172
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     2,     1,     1,     1,     8,     1,
       3,     0,     1,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     2,     1,
       3,     1,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     8,     1,     1,
       1,     1,     5,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     3,     1,
       1,     1,     3,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     4,     4,
       1,     3,     0,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     3,     1,     3,     2,     0,     4,
       1,     3,     2,     0,     3,     1,     1,     1,     3,     2,
       1,     1,     1,     2,     3,     2,     2,     3,     4,     3,
       5,     7,     5,     5,     7,     9,     1,     1,     2,     2,
       1,     3,     1,     3,     2,     2,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     5,     1,
       3,     0,     1,     2,     3,     3,     0,     1,     3,     6,
       3,     6,     6,     1,     3,     2,     0,     6,     1,     3,
       2,     1,     4,     4,     3,     5,     1,     2
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
#line 65 "./src/parser/gens/bison.y"
                    {
        ptree_t *f = ptree_create(PTREE_EOF, 0);
        ptree_g = ptree_add((yyvsp[0].ptree), 1, f);
    }
#line 1959 "bison.tab.c"
    break;

  case 3: /* program: %empty  */
#line 69 "./src/parser/gens/bison.y"
                  {
        ptree_g = ptree_create(PTREE_EOF, 0);
    }
#line 1967 "bison.tab.c"
    break;

  case 4: /* source_elements: source_elements source_element  */
#line 75 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 1975 "bison.tab.c"
    break;

  case 5: /* source_elements: source_element  */
#line 78 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_SOURCE_ELEMENTS, 1, (yyvsp[0].ptree));
    }
#line 1983 "bison.tab.c"
    break;

  case 6: /* source_element: function_declaration  */
#line 84 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 1991 "bison.tab.c"
    break;

  case 7: /* source_element: statement  */
#line 87 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 1999 "bison.tab.c"
    break;

  case 8: /* function_declaration: FUN WORD typed_parameters '(' parameters_list ')' type_annotation block_statement  */
#line 93 "./src/parser/gens/bison.y"
                                                                                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-6].str));
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_DECLARATION, 7, w, (yyvsp[-5].ptree), lp, (yyvsp[-3].ptree), rp, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2010 "bison.tab.c"
    break;

  case 9: /* parameters_list: parameter_is_mut  */
#line 102 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_LIST, 1, (yyvsp[0].ptree));
    }
#line 2018 "bison.tab.c"
    break;

  case 10: /* parameters_list: parameters_list ',' parameter_is_mut  */
#line 105 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2026 "bison.tab.c"
    break;

  case 11: /* parameters_list: %empty  */
#line 108 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 2034 "bison.tab.c"
    break;

  case 12: /* parameter_is_mut: parameter  */
#line 114 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_CONST, 1, (yyvsp[0].ptree));
    }
#line 2042 "bison.tab.c"
    break;

  case 13: /* parameter_is_mut: MUT parameter  */
#line 117 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_MUT, 1, (yyvsp[0].ptree));
    }
#line 2050 "bison.tab.c"
    break;

  case 14: /* parameter: identifier  */
#line 123 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 2058 "bison.tab.c"
    break;

  case 15: /* parameter: identifier '=' assignment_expression  */
#line 126 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 2067 "bison.tab.c"
    break;

  case 16: /* statement: expression_statement  */
#line 133 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2075 "bison.tab.c"
    break;

  case 17: /* statement: block_statement  */
#line 136 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2083 "bison.tab.c"
    break;

  case 18: /* statement: jump_statement  */
#line 139 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2091 "bison.tab.c"
    break;

  case 19: /* statement: labeled_statement  */
#line 142 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2099 "bison.tab.c"
    break;

  case 20: /* statement: selection_statement  */
#line 145 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2107 "bison.tab.c"
    break;

  case 21: /* statement: iteration_statement  */
#line 148 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2115 "bison.tab.c"
    break;

  case 22: /* statement: variable_statement ';'  */
#line 151 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2123 "bison.tab.c"
    break;

  case 23: /* statement: type_statement  */
#line 154 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2131 "bison.tab.c"
    break;

  case 24: /* statement: struct_statement  */
#line 157 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2139 "bison.tab.c"
    break;

  case 25: /* statement: union_statement  */
#line 160 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2147 "bison.tab.c"
    break;

  case 26: /* statement: enum_statement  */
#line 163 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2155 "bison.tab.c"
    break;

  case 27: /* statement: methods_statement  */
#line 166 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2163 "bison.tab.c"
    break;

  case 28: /* expression_statement: optional_expression ';'  */
#line 173 "./src/parser/gens/bison.y"
                            {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION_STATEMENT, 2, (yyvsp[-1].ptree), sc);
    }
#line 2172 "bison.tab.c"
    break;

  case 29: /* expression: assignment_expression  */
#line 180 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2180 "bison.tab.c"
    break;

  case 30: /* expression: expression ',' assignment_expression  */
#line 183 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 2, (yyvsp[0].ptree));
    }
#line 2188 "bison.tab.c"
    break;

  case 31: /* optional_expression: expression  */
#line 189 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2196 "bison.tab.c"
    break;

  case 32: /* optional_expression: %empty  */
#line 192 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2204 "bison.tab.c"
    break;

  case 33: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 198 "./src/parser/gens/bison.y"
                                                               {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2212 "bison.tab.c"
    break;

  case 34: /* assignment_expression: function_expression  */
#line 201 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2220 "bison.tab.c"
    break;

  case 35: /* assignment_expression: conditional_expression  */
#line 204 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2228 "bison.tab.c"
    break;

  case 36: /* assignment_operator: '='  */
#line 210 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_ASSIGN, 0);
    }
#line 2236 "bison.tab.c"
    break;

  case 37: /* assignment_operator: OR_ASSIGN  */
#line 213 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_OR_ASSIGN, 0);
    }
#line 2244 "bison.tab.c"
    break;

  case 38: /* assignment_operator: MUL_ASSIGN  */
#line 216 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MUL_ASSIGN, 0);
    }
#line 2252 "bison.tab.c"
    break;

  case 39: /* assignment_operator: DIV_ASSIGN  */
#line 219 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_DIV_ASSIGN, 0);
    }
#line 2260 "bison.tab.c"
    break;

  case 40: /* assignment_operator: MOD_ASSIGN  */
#line 222 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MOD_ASSIGN, 0);
    }
#line 2268 "bison.tab.c"
    break;

  case 41: /* assignment_operator: ADD_ASSIGN  */
#line 225 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_ADD_ASSIGN, 0);
    }
#line 2276 "bison.tab.c"
    break;

  case 42: /* assignment_operator: SUB_ASSIGN  */
#line 228 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SUB_ASSIGN, 0);
    }
#line 2284 "bison.tab.c"
    break;

  case 43: /* assignment_operator: SHL_ASSIGN  */
#line 231 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHL_ASSIGN, 0);
    }
#line 2292 "bison.tab.c"
    break;

  case 44: /* assignment_operator: SHR_ASSIGN  */
#line 234 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHR_ASSIGN, 0);
    }
#line 2300 "bison.tab.c"
    break;

  case 45: /* assignment_operator: AND_ASSIGN  */
#line 237 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_AND_ASSIGN, 0);
    }
#line 2308 "bison.tab.c"
    break;

  case 46: /* assignment_operator: XOR_ASSIGN  */
#line 240 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_XOR_ASSIGN, 0);
    }
#line 2316 "bison.tab.c"
    break;

  case 47: /* function_expression: FUN typed_parameters '(' parameters_list ')' type_annotation FORWARD_ARROW function_expression_body  */
#line 246 "./src/parser/gens/bison.y"
                                                                                                        {
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        ptree_t *fa = ptree_create(PTREE_FORWARD_ARROW, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_EXPRESSION, 7, (yyvsp[-6].ptree), lp, (yyvsp[-4].ptree), rp, (yyvsp[-2].ptree), fa, (yyvsp[0].ptree));
    }
#line 2327 "bison.tab.c"
    break;

  case 48: /* function_expression_body: assignment_expression  */
#line 255 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2335 "bison.tab.c"
    break;

  case 49: /* function_expression_body: block_statement  */
#line 258 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2343 "bison.tab.c"
    break;

  case 50: /* constant_expression: conditional_expression  */
#line 264 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2351 "bison.tab.c"
    break;

  case 51: /* conditional_expression: logical_or_expression  */
#line 270 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2359 "bison.tab.c"
    break;

  case 52: /* conditional_expression: logical_or_expression '?' assignment_expression ':' assignment_expression  */
#line 273 "./src/parser/gens/bison.y"
                                                                                {
        ptree_t *q = ptree_create(PTREE_QUESTION, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TERNARY_EXPRESSION, 5, (yyvsp[-4].ptree), q, (yyvsp[-2].ptree), sc, (yyvsp[0].ptree));
    }
#line 2369 "bison.tab.c"
    break;

  case 53: /* logical_or_expression: logical_and_expression  */
#line 281 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2377 "bison.tab.c"
    break;

  case 54: /* logical_or_expression: logical_or_expression OR logical_and_expression  */
#line 284 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *op = ptree_create(PTREE_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2386 "bison.tab.c"
    break;

  case 55: /* logical_and_expression: bitwise_or_expression  */
#line 291 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2394 "bison.tab.c"
    break;

  case 56: /* logical_and_expression: logical_and_expression AND bitwise_or_expression  */
#line 294 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2403 "bison.tab.c"
    break;

  case 57: /* bitwise_or_expression: bitwise_xor_expression  */
#line 301 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2411 "bison.tab.c"
    break;

  case 58: /* bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression  */
#line 304 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_BIT_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2420 "bison.tab.c"
    break;

  case 59: /* bitwise_xor_expression: bitwise_and_expression  */
#line 311 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2428 "bison.tab.c"
    break;

  case 60: /* bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression  */
#line 314 "./src/parser/gens/bison.y"
                                                        {
        ptree_t *op = ptree_create(PTREE_BIT_XOR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2437 "bison.tab.c"
    break;

  case 61: /* bitwise_and_expression: equality_expression  */
#line 321 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2445 "bison.tab.c"
    break;

  case 62: /* bitwise_and_expression: bitwise_and_expression '&' equality_expression  */
#line 324 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *op = ptree_create(PTREE_BIT_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2454 "bison.tab.c"
    break;

  case 63: /* equality_expression: relational_expression  */
#line 331 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2462 "bison.tab.c"
    break;

  case 64: /* equality_expression: equality_expression equality_operator relational_expression  */
#line 334 "./src/parser/gens/bison.y"
                                                                  {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2470 "bison.tab.c"
    break;

  case 65: /* equality_operator: IS_EQUAL  */
#line 340 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_IS_EQUAL, 0);
    }
#line 2478 "bison.tab.c"
    break;

  case 66: /* equality_operator: NOT_EQUAL  */
#line 343 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_NOT_EQUAL, 0);
    }
#line 2486 "bison.tab.c"
    break;

  case 67: /* relational_expression: shift_expression  */
#line 349 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2494 "bison.tab.c"
    break;

  case 68: /* relational_expression: relational_expression relational_operator shift_expression  */
#line 352 "./src/parser/gens/bison.y"
                                                                 {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2502 "bison.tab.c"
    break;

  case 69: /* relational_operator: '<'  */
#line 358 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_LESS, 0);
    }
#line 2510 "bison.tab.c"
    break;

  case 70: /* relational_operator: '>'  */
#line 361 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_GREATER, 0);
    }
#line 2518 "bison.tab.c"
    break;

  case 71: /* relational_operator: LESS_EQ  */
#line 364 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_LESS_EQ, 0);
    }
#line 2526 "bison.tab.c"
    break;

  case 72: /* relational_operator: GREATER_EQ  */
#line 367 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_GREATER_EQ, 0);
    }
#line 2534 "bison.tab.c"
    break;

  case 73: /* shift_expression: additive_expression  */
#line 373 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2542 "bison.tab.c"
    break;

  case 74: /* shift_expression: shift_expression shift_operator additive_expression  */
#line 376 "./src/parser/gens/bison.y"
                                                          {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2550 "bison.tab.c"
    break;

  case 75: /* shift_operator: LSHIFT  */
#line 382 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create(PTREE_LSHIFT, 0);
    }
#line 2558 "bison.tab.c"
    break;

  case 76: /* shift_operator: RSHIFT  */
#line 385 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_RSHIFT, 0);
    }
#line 2566 "bison.tab.c"
    break;

  case 77: /* additive_expression: multiplicative_expression  */
#line 391 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2574 "bison.tab.c"
    break;

  case 78: /* additive_expression: additive_expression additive_operator multiplicative_expression  */
#line 394 "./src/parser/gens/bison.y"
                                                                      {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2582 "bison.tab.c"
    break;

  case 79: /* additive_operator: '+'  */
#line 400 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2590 "bison.tab.c"
    break;

  case 80: /* additive_operator: '-'  */
#line 403 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2598 "bison.tab.c"
    break;

  case 81: /* multiplicative_expression: unary_expression  */
#line 409 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2606 "bison.tab.c"
    break;

  case 82: /* multiplicative_expression: multiplicative_expression multiplicative_operator unary_expression  */
#line 412 "./src/parser/gens/bison.y"
                                                                         {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2614 "bison.tab.c"
    break;

  case 83: /* multiplicative_operator: '*'  */
#line 418 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_MULTIPLY, 0);
    }
#line 2622 "bison.tab.c"
    break;

  case 84: /* multiplicative_operator: '/'  */
#line 421 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_DIVIDE, 0);
    }
#line 2630 "bison.tab.c"
    break;

  case 85: /* multiplicative_operator: '%'  */
#line 424 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MODULO, 0);
    }
#line 2638 "bison.tab.c"
    break;

  case 86: /* unary_expression: postfix_expression  */
#line 430 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2646 "bison.tab.c"
    break;

  case 87: /* unary_expression: unary_operator unary_expression  */
#line 433 "./src/parser/gens/bison.y"
                                      {
        (yyval.ptree) = ptree_create(PTREE_UNARY, 2, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2654 "bison.tab.c"
    break;

  case 88: /* unary_operator: INCREMENT  */
#line 439 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2662 "bison.tab.c"
    break;

  case 89: /* unary_operator: DECREMENT  */
#line 442 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2670 "bison.tab.c"
    break;

  case 90: /* unary_operator: '!'  */
#line 445 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_NOT, 0);
    }
#line 2678 "bison.tab.c"
    break;

  case 91: /* unary_operator: '~'  */
#line 448 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_TILDE, 0);
    }
#line 2686 "bison.tab.c"
    break;

  case 92: /* unary_operator: '+'  */
#line 451 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2694 "bison.tab.c"
    break;

  case 93: /* unary_operator: '-'  */
#line 454 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2702 "bison.tab.c"
    break;

  case 94: /* unary_operator: '&'  */
#line 457 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_BIT_AND, 0);
    }
#line 2710 "bison.tab.c"
    break;

  case 95: /* postfix_expression: primary_expression  */
#line 463 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2718 "bison.tab.c"
    break;

  case 96: /* postfix_expression: postfix_expression postfix_operator  */
#line 466 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_create(PTREE_POSTFIX, 2, (yyvsp[0].ptree), (yyvsp[-1].ptree));
    }
#line 2726 "bison.tab.c"
    break;

  case 97: /* postfix_expression: postfix_expression '.' WORD  */
#line 469 "./src/parser/gens/bison.y"
                                  {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        ptree_t *dot =ptree_create(PTREE_DOT, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_ACCESS, 3, (yyvsp[-2].ptree), dot, w);
    }
#line 2736 "bison.tab.c"
    break;

  case 98: /* postfix_expression: postfix_expression '[' expression ']'  */
#line 474 "./src/parser/gens/bison.y"
                                            {
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_ACCESS, 3, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2746 "bison.tab.c"
    break;

  case 99: /* postfix_expression: postfix_expression '(' arguments ')'  */
#line 479 "./src/parser/gens/bison.y"
                                           {
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_CALL, 4, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2756 "bison.tab.c"
    break;

  case 100: /* arguments: assignment_expression  */
#line 487 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ARGUMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2764 "bison.tab.c"
    break;

  case 101: /* arguments: arguments ',' assignment_expression  */
#line 490 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2772 "bison.tab.c"
    break;

  case 102: /* arguments: %empty  */
#line 493 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2780 "bison.tab.c"
    break;

  case 103: /* postfix_operator: INCREMENT  */
#line 499 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2788 "bison.tab.c"
    break;

  case 104: /* postfix_operator: DECREMENT  */
#line 502 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2796 "bison.tab.c"
    break;

  case 105: /* primary_expression: WORD  */
#line 508 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 2804 "bison.tab.c"
    break;

  case 106: /* primary_expression: literal  */
#line 511 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2812 "bison.tab.c"
    break;

  case 107: /* primary_expression: array_literal  */
#line 514 "./src/parser/gens/bison.y"
                    {
       (yyval.ptree) = (yyvsp[0].ptree); 
    }
#line 2820 "bison.tab.c"
    break;

  case 108: /* primary_expression: object_literal  */
#line 517 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2828 "bison.tab.c"
    break;

  case 109: /* primary_expression: '(' expression ')'  */
#line 520 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2838 "bison.tab.c"
    break;

  case 110: /* literal: NUMBER  */
#line 528 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2846 "bison.tab.c"
    break;

  case 111: /* literal: TRUE_VAL  */
#line 531 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create_bool(1);
    }
#line 2854 "bison.tab.c"
    break;

  case 112: /* literal: FALSE_VAL  */
#line 534 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create_bool(0);
    }
#line 2862 "bison.tab.c"
    break;

  case 113: /* literal: STRING  */
#line 537 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2870 "bison.tab.c"
    break;

  case 114: /* array_literal: '[' element_list ']'  */
#line 543 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *right = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_LITERAL, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2880 "bison.tab.c"
    break;

  case 115: /* element_list: assignment_expression  */
#line 551 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ELEMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2888 "bison.tab.c"
    break;

  case 116: /* element_list: element_list ',' assignment_expression  */
#line 554 "./src/parser/gens/bison.y"
                                             {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2896 "bison.tab.c"
    break;

  case 117: /* element_list: element_list ','  */
#line 557 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2904 "bison.tab.c"
    break;

  case 118: /* element_list: %empty  */
#line 560 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2912 "bison.tab.c"
    break;

  case 119: /* object_literal: WORD '{' keyvalue_list '}'  */
#line 566 "./src/parser/gens/bison.y"
                               {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_LITERAL, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 2923 "bison.tab.c"
    break;

  case 120: /* keyvalue_list: keyvalue  */
#line 575 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2931 "bison.tab.c"
    break;

  case 121: /* keyvalue_list: keyvalue_list ',' keyvalue  */
#line 578 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2939 "bison.tab.c"
    break;

  case 122: /* keyvalue_list: keyvalue_list ','  */
#line 581 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2947 "bison.tab.c"
    break;

  case 123: /* keyvalue_list: %empty  */
#line 584 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2955 "bison.tab.c"
    break;

  case 124: /* keyvalue: property_name ':' assignment_expression  */
#line 590 "./src/parser/gens/bison.y"
                                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 2964 "bison.tab.c"
    break;

  case 125: /* property_name: WORD  */
#line 597 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 2972 "bison.tab.c"
    break;

  case 126: /* property_name: STRING  */
#line 600 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2980 "bison.tab.c"
    break;

  case 127: /* property_name: NUMBER  */
#line 603 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2988 "bison.tab.c"
    break;

  case 128: /* block_statement: '{' source_elements '}'  */
#line 609 "./src/parser/gens/bison.y"
                            {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2999 "bison.tab.c"
    break;

  case 129: /* block_statement: '{' '}'  */
#line 615 "./src/parser/gens/bison.y"
              {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, empty, right);
    }
#line 3010 "bison.tab.c"
    break;

  case 130: /* jump_statement: return_statement  */
#line 624 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3018 "bison.tab.c"
    break;

  case 131: /* jump_statement: break_statement  */
#line 627 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3026 "bison.tab.c"
    break;

  case 132: /* jump_statement: continue_statement  */
#line 630 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3034 "bison.tab.c"
    break;

  case 133: /* return_statement: RETURN ';'  */
#line 635 "./src/parser/gens/bison.y"
               {
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, empty);
    }
#line 3043 "bison.tab.c"
    break;

  case 134: /* return_statement: RETURN expression ';'  */
#line 639 "./src/parser/gens/bison.y"
                            {
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, (yyvsp[-1].ptree));
    }
#line 3051 "bison.tab.c"
    break;

  case 135: /* break_statement: BREAK ';'  */
#line 645 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_BREAK, 0);
    }
#line 3059 "bison.tab.c"
    break;

  case 136: /* continue_statement: CONTINUE ';'  */
#line 651 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_CONTINUE, 0);
    }
#line 3067 "bison.tab.c"
    break;

  case 137: /* labeled_statement: WORD ':' statement  */
#line 657 "./src/parser/gens/bison.y"
                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LABEL, 3, w, c, (yyvsp[0].ptree));
    }
#line 3077 "bison.tab.c"
    break;

  case 138: /* labeled_statement: CASE constant_expression ':' statement  */
#line 662 "./src/parser/gens/bison.y"
                                             {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CASE_LABEL, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 3086 "bison.tab.c"
    break;

  case 139: /* labeled_statement: DEFAULT ':' statement  */
#line 666 "./src/parser/gens/bison.y"
                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_DEFAULT_LABEL, 2, c, (yyvsp[0].ptree));
    }
#line 3095 "bison.tab.c"
    break;

  case 140: /* selection_statement: IF '(' expression ')' statement  */
#line 674 "./src/parser/gens/bison.y"
    {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3106 "bison.tab.c"
    break;

  case 141: /* selection_statement: IF '(' expression ')' statement ELSE statement  */
#line 680 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *e = ptree_create(PTREE_ELSE, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF_ELSE, 6, left, (yyvsp[-4].ptree), right, (yyvsp[-2].ptree), e, (yyvsp[0].ptree));
    }
#line 3118 "bison.tab.c"
    break;

  case 142: /* selection_statement: SWITCH '(' expression ')' statement  */
#line 687 "./src/parser/gens/bison.y"
                                          {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_SWITCH, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3129 "bison.tab.c"
    break;

  case 143: /* iteration_statement: WHILE '(' expression ')' statement  */
#line 696 "./src/parser/gens/bison.y"
                                       {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_WHILE, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3139 "bison.tab.c"
    break;

  case 144: /* iteration_statement: DO statement WHILE '(' expression ')' ';'  */
#line 701 "./src/parser/gens/bison.y"
                                                {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *wh = ptree_create(PTREE_WHILE, 0);

        (yyval.ptree) = ptree_create(PTREE_DO, 6, (yyvsp[-5].ptree), wh, left, (yyvsp[-2].ptree), right, sc);
    }
#line 3152 "bison.tab.c"
    break;

  case 145: /* iteration_statement: FOR '(' for_initialization ';' optional_expression ';' optional_expression ')' statement  */
#line 709 "./src/parser/gens/bison.y"
                                                                                               {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *sc2 = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_FOR, 8, left, (yyvsp[-6].ptree), sc, (yyvsp[-4].ptree), sc2, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3164 "bison.tab.c"
    break;

  case 146: /* for_initialization: optional_expression  */
#line 719 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3172 "bison.tab.c"
    break;

  case 147: /* for_initialization: variable_statement  */
#line 722 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3180 "bison.tab.c"
    break;

  case 148: /* variable_statement: LET variable_list  */
#line 728 "./src/parser/gens/bison.y"
                      {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LET_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3189 "bison.tab.c"
    break;

  case 149: /* variable_statement: CONST variable_list  */
#line 732 "./src/parser/gens/bison.y"
                          {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CONST_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3198 "bison.tab.c"
    break;

  case 150: /* variable_list: variable  */
#line 739 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3206 "bison.tab.c"
    break;

  case 151: /* variable_list: variable_list ',' variable  */
#line 742 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3214 "bison.tab.c"
    break;

  case 152: /* variable: identifier  */
#line 748 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 3222 "bison.tab.c"
    break;

  case 153: /* variable: identifier '=' assignment_expression  */
#line 751 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 3231 "bison.tab.c"
    break;

  case 154: /* identifier: WORD type_annotation  */
#line 758 "./src/parser/gens/bison.y"
                         {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_IDENTIFIER, 2, w, (yyvsp[0].ptree));
    }
#line 3240 "bison.tab.c"
    break;

  case 155: /* type_annotation: ':' type  */
#line 765 "./src/parser/gens/bison.y"
             {
        ptree_t *sc = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ANNOTATION, 2, sc, (yyvsp[0].ptree)); 
    }
#line 3249 "bison.tab.c"
    break;

  case 156: /* type_annotation: %empty  */
#line 769 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3257 "bison.tab.c"
    break;

  case 157: /* type: primitive_type  */
#line 775 "./src/parser/gens/bison.y"
                   {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3265 "bison.tab.c"
    break;

  case 158: /* type: array_type  */
#line 778 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3273 "bison.tab.c"
    break;

  case 159: /* type: tuple_type  */
#line 781 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3281 "bison.tab.c"
    break;

  case 160: /* type: function_type  */
#line 784 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = (yyvsp[0].ptree);        
    }
#line 3289 "bison.tab.c"
    break;

  case 161: /* type: type_reference  */
#line 787 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3297 "bison.tab.c"
    break;

  case 162: /* primitive_type: T_VOID  */
#line 793 "./src/parser/gens/bison.y"
           { 
        (yyval.ptree) = ptree_create(PTREE_VOID_TYPE, 0);
    }
#line 3305 "bison.tab.c"
    break;

  case 163: /* primitive_type: S32  */
#line 796 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_INTEGER_TYPE, 0);
    }
#line 3313 "bison.tab.c"
    break;

  case 164: /* primitive_type: T_STRING  */
#line 799 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_STRING_TYPE, 0);
    }
#line 3321 "bison.tab.c"
    break;

  case 165: /* primitive_type: T_BOOLEAN  */
#line 802 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_BOOLEAN_TYPE, 0);
    }
#line 3329 "bison.tab.c"
    break;

  case 166: /* array_type: type '[' ']'  */
#line 808 "./src/parser/gens/bison.y"
                 {
        ptree_t *lb = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_TYPE, 3, (yyvsp[-2].ptree), lb, rb);
    }
#line 3339 "bison.tab.c"
    break;

  case 167: /* tuple_type: type '[' NUMBER ']'  */
#line 816 "./src/parser/gens/bison.y"
                        {
        ptree_t *num = ptree_create_num((yyvsp[-1].num));
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_TUPLE_TYPE, 4, (yyvsp[-3].ptree), lb, num, rb);
    }
#line 3350 "bison.tab.c"
    break;

  case 168: /* function_type: type BACKWARD_ARROW '(' types_list ')'  */
#line 825 "./src/parser/gens/bison.y"
                                           {
        ptree_t *a = ptree_create(PTREE_BACKWARD_ARROW, 0);
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_TYPE, 5, (yyvsp[-4].ptree), a, lb, (yyvsp[-1].ptree), rb);
    }
#line 3361 "bison.tab.c"
    break;

  case 169: /* types_list: type  */
#line 834 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create(PTREE_TYPE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3369 "bison.tab.c"
    break;

  case 170: /* types_list: types_list ',' type  */
#line 837 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3377 "bison.tab.c"
    break;

  case 171: /* types_list: %empty  */
#line 840 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0); 
    }
#line 3385 "bison.tab.c"
    break;

  case 172: /* type_reference: WORD  */
#line 846 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 1, w);
    }
#line 3394 "bison.tab.c"
    break;

  case 173: /* type_reference: WORD typed_arguments  */
#line 850 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 2, w, (yyvsp[0].ptree));
    }
#line 3403 "bison.tab.c"
    break;

  case 174: /* typed_arguments: '<' types_list '>'  */
#line 857 "./src/parser/gens/bison.y"
                       {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_ARGUMENTS, 3, l, (yyvsp[-1].ptree), g);
    }
#line 3413 "bison.tab.c"
    break;

  case 175: /* typed_parameters: '<' type_params '>'  */
#line 865 "./src/parser/gens/bison.y"
                        {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_PARAMETERS, 3, l, (yyvsp[-1].ptree), g); 
    }
#line 3423 "bison.tab.c"
    break;

  case 176: /* typed_parameters: %empty  */
#line 870 "./src/parser/gens/bison.y"
                  {
         (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3431 "bison.tab.c"
    break;

  case 177: /* type_params: WORD  */
#line 876 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_PARAMS, 1, w); 
    }
#line 3440 "bison.tab.c"
    break;

  case 178: /* type_params: type_params ',' WORD  */
#line 880 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, w);
    }
#line 3449 "bison.tab.c"
    break;

  case 179: /* type_statement: TYPE WORD typed_parameters '=' type ';'  */
#line 887 "./src/parser/gens/bison.y"
                                            {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ALIAS, 5, w, (yyvsp[-3].ptree), eq, (yyvsp[-1].ptree), sc); 
    }
#line 3460 "bison.tab.c"
    break;

  case 180: /* struct_statement: STRUCT WORD ';'  */
#line 896 "./src/parser/gens/bison.y"
                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_UNIT_STRUCT, 1, w);
    }
#line 3469 "bison.tab.c"
    break;

  case 181: /* struct_statement: STRUCT WORD typed_parameters '{' field_list '}'  */
#line 900 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_STRUCT, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3480 "bison.tab.c"
    break;

  case 182: /* union_statement: UNION WORD typed_parameters '{' field_list '}'  */
#line 909 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_UNION, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3491 "bison.tab.c"
    break;

  case 183: /* field_list: identifier  */
#line 918 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_FIELDS, 1, (yyvsp[0].ptree));
    }
#line 3499 "bison.tab.c"
    break;

  case 184: /* field_list: field_list ',' identifier  */
#line 921 "./src/parser/gens/bison.y"
                                {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3507 "bison.tab.c"
    break;

  case 185: /* field_list: field_list ','  */
#line 924 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3515 "bison.tab.c"
    break;

  case 186: /* field_list: %empty  */
#line 927 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3523 "bison.tab.c"
    break;

  case 187: /* enum_statement: ENUM WORD typed_parameters '{' enum_members '}'  */
#line 933 "./src/parser/gens/bison.y"
                                                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3534 "bison.tab.c"
    break;

  case 188: /* enum_members: enum_member  */
#line 942 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_ENUM_MEMBERS, 1, (yyvsp[0].ptree));
    }
#line 3542 "bison.tab.c"
    break;

  case 189: /* enum_members: enum_members ',' enum_member  */
#line 945 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3550 "bison.tab.c"
    break;

  case 190: /* enum_members: enum_members ','  */
#line 948 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3558 "bison.tab.c"
    break;

  case 191: /* enum_member: WORD  */
#line 954 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 3566 "bison.tab.c"
    break;

  case 192: /* enum_member: WORD '(' types_list ')'  */
#line 957 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LPRN, 0);
        ptree_t *right = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3577 "bison.tab.c"
    break;

  case 193: /* enum_member: WORD '{' field_list '}'  */
#line 963 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_NAMED_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3588 "bison.tab.c"
    break;

  case 194: /* enum_member: WORD '=' NUMBER  */
#line 969 "./src/parser/gens/bison.y"
                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *num = ptree_create_num((yyvsp[0].num));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_DEFAULT, 3, w, eq, num);
    }
#line 3599 "bison.tab.c"
    break;

  case 195: /* methods_statement: METHODS WORD '{' method_list '}'  */
#line 978 "./src/parser/gens/bison.y"
                                     {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_METHODS, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3610 "bison.tab.c"
    break;

  case 196: /* method_list: function_declaration  */
#line 987 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = ptree_create(PTREE_METHODS_LIST, 1, (yyvsp[0].ptree));
    }
#line 3618 "bison.tab.c"
    break;

  case 197: /* method_list: method_list function_declaration  */
#line 990 "./src/parser/gens/bison.y"
                                       {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 3626 "bison.tab.c"
    break;


#line 3630 "bison.tab.c"

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

#line 995 "./src/parser/gens/bison.y"
