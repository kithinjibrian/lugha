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
  YYSYMBOL_CLASS = 55,                     /* CLASS  */
  YYSYMBOL_S8 = 56,                        /* S8  */
  YYSYMBOL_S16 = 57,                       /* S16  */
  YYSYMBOL_S32 = 58,                       /* S32  */
  YYSYMBOL_S64 = 59,                       /* S64  */
  YYSYMBOL_U8 = 60,                        /* U8  */
  YYSYMBOL_U16 = 61,                       /* U16  */
  YYSYMBOL_U32 = 62,                       /* U32  */
  YYSYMBOL_U64 = 63,                       /* U64  */
  YYSYMBOL_F32 = 64,                       /* F32  */
  YYSYMBOL_F64 = 65,                       /* F64  */
  YYSYMBOL_T_STRING = 66,                  /* T_STRING  */
  YYSYMBOL_T_BOOLEAN = 67,                 /* T_BOOLEAN  */
  YYSYMBOL_T_VOID = 68,                    /* T_VOID  */
  YYSYMBOL_T_CHAR = 69,                    /* T_CHAR  */
  YYSYMBOL_70_ = 70,                       /* '('  */
  YYSYMBOL_71_ = 71,                       /* ')'  */
  YYSYMBOL_LOWER_THAN_ELSE = 72,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_73_ = 73,                       /* ','  */
  YYSYMBOL_74_ = 74,                       /* '='  */
  YYSYMBOL_75_ = 75,                       /* ';'  */
  YYSYMBOL_76_ = 76,                       /* '?'  */
  YYSYMBOL_77_ = 77,                       /* ':'  */
  YYSYMBOL_78_ = 78,                       /* '|'  */
  YYSYMBOL_79_ = 79,                       /* '^'  */
  YYSYMBOL_80_ = 80,                       /* '&'  */
  YYSYMBOL_81_ = 81,                       /* '<'  */
  YYSYMBOL_82_ = 82,                       /* '>'  */
  YYSYMBOL_83_ = 83,                       /* '+'  */
  YYSYMBOL_84_ = 84,                       /* '-'  */
  YYSYMBOL_85_ = 85,                       /* '*'  */
  YYSYMBOL_86_ = 86,                       /* '/'  */
  YYSYMBOL_87_ = 87,                       /* '%'  */
  YYSYMBOL_88_ = 88,                       /* '!'  */
  YYSYMBOL_89_ = 89,                       /* '~'  */
  YYSYMBOL_90_ = 90,                       /* '.'  */
  YYSYMBOL_91_ = 91,                       /* '['  */
  YYSYMBOL_92_ = 92,                       /* ']'  */
  YYSYMBOL_93_ = 93,                       /* '{'  */
  YYSYMBOL_94_ = 94,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 95,                  /* $accept  */
  YYSYMBOL_program = 96,                   /* program  */
  YYSYMBOL_source_elements = 97,           /* source_elements  */
  YYSYMBOL_source_element = 98,            /* source_element  */
  YYSYMBOL_function_declaration = 99,      /* function_declaration  */
  YYSYMBOL_parameters_list = 100,          /* parameters_list  */
  YYSYMBOL_parameter_is_mut = 101,         /* parameter_is_mut  */
  YYSYMBOL_parameter = 102,                /* parameter  */
  YYSYMBOL_statement = 103,                /* statement  */
  YYSYMBOL_expression_statement = 104,     /* expression_statement  */
  YYSYMBOL_expression = 105,               /* expression  */
  YYSYMBOL_optional_expression = 106,      /* optional_expression  */
  YYSYMBOL_assignment_expression = 107,    /* assignment_expression  */
  YYSYMBOL_assignment_operator = 108,      /* assignment_operator  */
  YYSYMBOL_function_expression = 109,      /* function_expression  */
  YYSYMBOL_function_expression_body = 110, /* function_expression_body  */
  YYSYMBOL_constant_expression = 111,      /* constant_expression  */
  YYSYMBOL_conditional_expression = 112,   /* conditional_expression  */
  YYSYMBOL_logical_or_expression = 113,    /* logical_or_expression  */
  YYSYMBOL_logical_and_expression = 114,   /* logical_and_expression  */
  YYSYMBOL_bitwise_or_expression = 115,    /* bitwise_or_expression  */
  YYSYMBOL_bitwise_xor_expression = 116,   /* bitwise_xor_expression  */
  YYSYMBOL_bitwise_and_expression = 117,   /* bitwise_and_expression  */
  YYSYMBOL_equality_expression = 118,      /* equality_expression  */
  YYSYMBOL_equality_operator = 119,        /* equality_operator  */
  YYSYMBOL_relational_expression = 120,    /* relational_expression  */
  YYSYMBOL_relational_operator = 121,      /* relational_operator  */
  YYSYMBOL_shift_expression = 122,         /* shift_expression  */
  YYSYMBOL_shift_operator = 123,           /* shift_operator  */
  YYSYMBOL_additive_expression = 124,      /* additive_expression  */
  YYSYMBOL_additive_operator = 125,        /* additive_operator  */
  YYSYMBOL_multiplicative_expression = 126, /* multiplicative_expression  */
  YYSYMBOL_multiplicative_operator = 127,  /* multiplicative_operator  */
  YYSYMBOL_unary_expression = 128,         /* unary_expression  */
  YYSYMBOL_unary_operator = 129,           /* unary_operator  */
  YYSYMBOL_postfix_expression = 130,       /* postfix_expression  */
  YYSYMBOL_arguments = 131,                /* arguments  */
  YYSYMBOL_postfix_operator = 132,         /* postfix_operator  */
  YYSYMBOL_primary_expression = 133,       /* primary_expression  */
  YYSYMBOL_literal = 134,                  /* literal  */
  YYSYMBOL_array_literal = 135,            /* array_literal  */
  YYSYMBOL_element_list = 136,             /* element_list  */
  YYSYMBOL_object_literal = 137,           /* object_literal  */
  YYSYMBOL_keyvalue_list = 138,            /* keyvalue_list  */
  YYSYMBOL_keyvalue = 139,                 /* keyvalue  */
  YYSYMBOL_property_name = 140,            /* property_name  */
  YYSYMBOL_block_statement = 141,          /* block_statement  */
  YYSYMBOL_jump_statement = 142,           /* jump_statement  */
  YYSYMBOL_return_statement = 143,         /* return_statement  */
  YYSYMBOL_break_statement = 144,          /* break_statement  */
  YYSYMBOL_continue_statement = 145,       /* continue_statement  */
  YYSYMBOL_labeled_statement = 146,        /* labeled_statement  */
  YYSYMBOL_selection_statement = 147,      /* selection_statement  */
  YYSYMBOL_iteration_statement = 148,      /* iteration_statement  */
  YYSYMBOL_for_initialization = 149,       /* for_initialization  */
  YYSYMBOL_variable_statement = 150,       /* variable_statement  */
  YYSYMBOL_variable_list = 151,            /* variable_list  */
  YYSYMBOL_variable = 152,                 /* variable  */
  YYSYMBOL_identifier = 153,               /* identifier  */
  YYSYMBOL_type_annotation = 154,          /* type_annotation  */
  YYSYMBOL_type = 155,                     /* type  */
  YYSYMBOL_primitive_type = 156,           /* primitive_type  */
  YYSYMBOL_array_type = 157,               /* array_type  */
  YYSYMBOL_tuple_type = 158,               /* tuple_type  */
  YYSYMBOL_function_type = 159,            /* function_type  */
  YYSYMBOL_types_list = 160,               /* types_list  */
  YYSYMBOL_type_reference = 161,           /* type_reference  */
  YYSYMBOL_typed_arguments = 162,          /* typed_arguments  */
  YYSYMBOL_typed_parameters = 163,         /* typed_parameters  */
  YYSYMBOL_type_params = 164,              /* type_params  */
  YYSYMBOL_type_statement = 165,           /* type_statement  */
  YYSYMBOL_struct_statement = 166,         /* struct_statement  */
  YYSYMBOL_union_statement = 167,          /* union_statement  */
  YYSYMBOL_field_list = 168,               /* field_list  */
  YYSYMBOL_enum_statement = 169,           /* enum_statement  */
  YYSYMBOL_enum_members = 170,             /* enum_members  */
  YYSYMBOL_enum_member = 171,              /* enum_member  */
  YYSYMBOL_methods_statement = 172,        /* methods_statement  */
  YYSYMBOL_method_list = 173,              /* method_list  */
  YYSYMBOL_class_statement = 174           /* class_statement  */
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
#define YYFINAL  114
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   667

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  80
/* YYNRULES -- Number of rules.  */
#define YYNRULES  199
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  333

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   325


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
       2,     2,     2,    88,     2,     2,     2,    87,    80,     2,
      70,    71,    85,    83,    73,    84,    90,    86,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    77,    75,
      81,    74,    82,    76,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    91,     2,    92,    79,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    93,    78,    94,    89,     2,     2,     2,
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
      65,    66,    67,    68,    69,    72
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    65,    65,    69,    75,    78,    84,    87,    93,   102,
     105,   108,   114,   117,   123,   126,   133,   136,   139,   142,
     145,   148,   151,   154,   157,   160,   163,   166,   169,   176,
     183,   186,   192,   195,   201,   204,   207,   213,   216,   219,
     222,   225,   228,   231,   234,   237,   240,   243,   249,   258,
     261,   267,   273,   276,   284,   287,   294,   297,   304,   307,
     314,   317,   324,   327,   334,   337,   343,   346,   352,   355,
     361,   364,   367,   370,   376,   379,   385,   388,   394,   397,
     403,   406,   412,   415,   421,   424,   427,   433,   436,   442,
     445,   448,   451,   454,   457,   460,   466,   469,   472,   477,
     482,   490,   493,   496,   502,   505,   511,   514,   517,   520,
     523,   531,   534,   537,   540,   546,   554,   557,   560,   563,
     569,   578,   581,   584,   587,   593,   600,   603,   606,   612,
     618,   627,   630,   633,   638,   642,   648,   654,   660,   665,
     669,   676,   683,   690,   699,   704,   712,   722,   725,   731,
     735,   742,   745,   751,   754,   761,   768,   772,   778,   781,
     784,   787,   790,   796,   799,   802,   805,   811,   819,   828,
     837,   840,   843,   849,   853,   860,   868,   873,   879,   883,
     890,   899,   903,   912,   921,   924,   927,   930,   936,   945,
     948,   951,   957,   960,   966,   972,   981,   990,   993,   999
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
  "TRUE_VAL", "ENUM", "METHODS", "MATCH", "FALSE_VAL", "CLASS", "S8",
  "S16", "S32", "S64", "U8", "U16", "U32", "U64", "F32", "F64", "T_STRING",
  "T_BOOLEAN", "T_VOID", "T_CHAR", "'('", "')'", "LOWER_THAN_ELSE", "','",
  "'='", "';'", "'?'", "':'", "'|'", "'^'", "'&'", "'<'", "'>'", "'+'",
  "'-'", "'*'", "'/'", "'%'", "'!'", "'~'", "'.'", "'['", "']'", "'{'",
  "'}'", "$accept", "program", "source_elements", "source_element",
  "function_declaration", "parameters_list", "parameter_is_mut",
  "parameter", "statement", "expression_statement", "expression",
  "optional_expression", "assignment_expression", "assignment_operator",
  "function_expression", "function_expression_body", "constant_expression",
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
  "enum_member", "methods_statement", "method_list", "class_statement", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-247)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-34)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     420,  -247,  -247,   -23,  -247,  -247,    24,     2,   185,   -48,
     -41,    34,   480,    60,    63,    68,   576,    29,   138,    24,
     175,   183,  -247,   188,   194,  -247,   195,   536,  -247,  -247,
    -247,  -247,  -247,   536,   284,   204,   420,  -247,  -247,  -247,
    -247,   132,   133,  -247,  -247,  -247,     1,   197,   136,   131,
     135,    49,    36,   181,   112,    67,    16,   576,     9,  -247,
    -247,  -247,  -247,  -247,  -247,  -247,  -247,  -247,  -247,  -247,
    -247,   141,  -247,  -247,  -247,  -247,  -247,  -247,   480,   145,
     134,   148,  -247,   150,   147,   216,   156,   139,   147,  -247,
      83,   536,  -247,  -247,   187,   129,   536,   536,   157,  -247,
    -247,   480,   147,   148,   -14,   147,   147,   140,   147,    86,
    -247,    47,  -247,   352,  -247,  -247,   536,  -247,   576,   536,
     576,   576,   576,   576,  -247,  -247,   576,  -247,  -247,  -247,
    -247,   576,  -247,  -247,   576,  -247,  -247,   576,  -247,  -247,
    -247,   576,  -247,  -247,  -247,  -247,  -247,  -247,  -247,  -247,
    -247,  -247,  -247,   536,  -247,  -247,  -247,   536,   231,   536,
    -247,  -247,  -247,  -247,  -247,  -247,   -61,  -247,   163,     5,
    -247,    24,   536,   173,  -247,   -13,    19,  -247,    89,   174,
    -247,   170,  -247,    94,    97,   480,  -247,   172,  -247,   155,
     158,   159,   215,   160,  -247,   536,  -247,  -247,  -247,   197,
     177,   136,   131,   135,    49,    36,   181,   112,    67,  -247,
    -247,  -247,    98,  -247,    50,   145,  -247,   536,   169,  -247,
    -247,  -247,  -247,   -15,  -247,  -247,  -247,  -247,  -247,  -247,
    -247,    19,   250,  -247,    24,   101,  -247,  -247,   184,   480,
     536,   536,   480,   480,  -247,     5,    24,    24,   251,   253,
    -247,   -16,    24,  -247,   536,  -247,   536,  -247,  -247,  -247,
       5,  -247,   191,     4,   104,  -247,  -247,   134,    19,   536,
     225,   107,   189,  -247,  -247,   -17,  -247,   -45,     7,    21,
      31,  -247,  -247,  -247,    42,  -247,  -247,   -15,    55,     5,
     171,  -247,   134,   247,  -247,  -247,   480,   192,   536,  -247,
      24,  -247,  -247,     5,   267,    24,   251,  -247,  -247,     5,
    -247,   111,  -247,   178,    14,  -247,  -247,   201,  -247,   114,
    -247,    46,  -247,   -15,  -247,  -247,  -247,  -247,  -247,   480,
    -247,  -247,  -247
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,   111,   114,   106,    89,    90,     0,   177,     0,     0,
       0,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,   113,     0,     0,    95,    93,
      94,    91,    92,   119,    33,     0,     2,     5,     6,     7,
      16,    32,     0,    30,    35,    36,    52,    54,    56,    58,
      60,    62,    64,    68,    74,    78,    82,     0,    87,    96,
     107,   108,   109,    17,    18,   131,   132,   133,    19,    20,
      21,     0,    23,    24,    25,    26,    27,    28,    33,   124,
     157,   149,   151,   153,   177,     0,     0,   106,   177,   134,
       0,     0,   136,   137,     0,    33,     0,     0,     0,    51,
      82,    33,   177,   150,   177,   177,   177,     0,   177,     0,
     116,     0,   130,    33,     1,     4,     0,    29,     0,     0,
       0,     0,     0,     0,    66,    67,     0,    72,    73,    70,
      71,     0,    76,    77,     0,    80,    81,     0,    84,    85,
      86,     0,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    37,     0,    88,   104,   105,   103,     0,     0,
      97,    22,   138,   128,   127,   126,     0,   121,     0,     0,
     155,     0,     0,     0,   178,     0,    11,   135,     0,     0,
     147,     0,   148,     0,     0,    33,   140,     0,   181,     0,
       0,     0,     0,     0,   110,   118,   115,   129,    31,    55,
       0,    57,    59,    61,    63,    65,    69,    75,    79,    83,
      34,   101,     0,    98,     0,   123,   120,     0,   173,   164,
     165,   166,   163,   156,   158,   159,   160,   161,   162,   152,
     154,    11,     0,   176,     0,     0,     9,    12,    14,    33,
       0,    33,    33,    33,   139,     0,   187,   187,     0,     0,
     197,     0,   187,   117,     0,   100,     0,    99,   122,   125,
     172,   174,     0,     0,     0,   179,    13,   157,     0,     0,
     141,     0,     0,   144,   143,     0,   184,     0,     0,   192,
       0,   189,   196,   198,     0,    53,   102,   170,     0,   172,
       0,   167,   157,     0,    10,    15,    33,     0,    33,   180,
     186,   182,   183,   172,     0,   187,   191,   188,   199,     0,
     175,     0,   168,     0,     0,   142,   145,     0,   185,     0,
     195,     0,   190,   171,   169,     8,    49,    48,    50,    33,
     193,   194,   146
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -247,  -247,   241,   -27,  -176,    48,    10,    43,   -12,  -247,
      -4,   -92,   -31,  -247,  -247,  -247,  -247,   264,  -247,   164,
     161,   165,   166,   162,  -247,   167,  -247,   168,  -247,   176,
    -247,   154,  -247,   -10,  -247,  -247,  -247,  -247,  -247,  -247,
    -247,  -247,  -247,  -247,    77,  -247,  -116,  -247,  -247,  -247,
    -247,  -247,  -247,  -247,  -247,   199,   277,   126,    -5,  -246,
    -164,  -247,  -247,  -247,  -247,  -160,  -247,  -247,   -49,  -247,
    -247,  -247,  -247,  -223,  -247,  -247,    -8,  -247,  -247,  -247
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    35,    36,    37,    38,   235,   236,   237,    39,    40,
      41,    42,    43,   153,    44,   327,    98,    45,    46,    47,
      48,    49,    50,    51,   126,    52,   131,    53,   134,    54,
     137,    55,   141,    56,    57,    58,   212,   160,    59,    60,
      61,   111,    62,   166,   167,   168,    63,    64,    65,    66,
      67,    68,    69,    70,   181,    71,    81,    82,   238,   170,
     287,   224,   225,   226,   227,   288,   228,   261,    86,   175,
      72,    73,    74,   277,    75,   280,   281,    76,   251,    77
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      94,    83,   110,   180,    90,   223,   100,   290,    84,   115,
     118,   218,   215,   262,    83,   262,   250,     1,   249,     2,
      87,   293,    91,   109,   278,    80,   155,   156,   300,   284,
      80,     4,     5,   216,    92,   173,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   313,   154,    88,   301,
     127,   128,   234,   187,    78,   189,   190,   191,   299,   193,
     232,   188,   124,   219,    22,   125,   162,    85,    25,   233,
      79,   220,   221,   222,   263,   283,   263,   119,   282,   157,
     300,   275,   321,    85,    27,   198,   115,   178,   200,   186,
     152,   303,   183,   184,    28,   304,   291,    29,    30,   158,
     159,   302,    31,    32,   306,    33,   101,    34,   100,    93,
     100,   100,   100,   100,   305,   300,   100,   129,   130,   300,
     195,   100,   210,   116,   100,   307,   211,   100,   309,   311,
      95,   209,     1,    96,     2,    87,   308,   310,    97,   196,
     331,   230,   257,   319,   102,   323,     4,     5,   163,   272,
     164,   165,   138,   139,   140,   214,   116,   194,   177,   116,
     239,     6,   116,    88,   253,   242,    83,   116,   243,   255,
     116,   256,   267,   244,   268,   292,    19,   268,   297,    22,
     116,   104,   324,    25,   309,   330,   259,   309,     1,   105,
       2,    87,   132,   133,   106,   135,   136,   325,   328,    27,
     107,   108,     4,     5,   114,   116,   317,   120,   117,    28,
     122,   169,    29,    30,   121,   123,   161,    31,    32,    88,
      33,   171,   174,   285,   172,   286,   176,   270,    85,   179,
     273,   274,    79,   192,   185,    22,   271,   213,   295,    25,
     217,   276,   276,   231,   240,   241,   245,   276,   246,   249,
     260,   247,   248,   252,   254,    27,   265,   279,   269,    84,
      89,   289,   296,   312,   298,    28,   314,   316,    29,    30,
     320,    34,   329,    31,    32,   113,    33,   266,   294,   264,
      99,   201,   199,   326,   315,   204,   202,     1,   203,     2,
       3,   208,   258,   205,   182,   318,   103,   229,   322,   206,
     276,     4,     5,     0,     0,     0,     0,     0,     0,     0,
     207,     0,     0,     0,     0,     0,     6,   332,     7,     8,
       9,     0,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,     0,    25,    26,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    27,     1,     0,     2,     3,     0,
       0,     0,     0,     0,    28,     0,     0,    29,    30,     4,
       5,     0,    31,    32,     0,    33,     0,    34,   112,     0,
       0,     0,     0,     0,     6,     0,     7,     8,     9,     0,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,     0,    25,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    27,     1,     0,     2,     3,     0,     0,     0,
       0,     0,    28,     0,     0,    29,    30,     4,     5,     0,
      31,    32,     0,    33,     0,    34,   197,     0,     0,     0,
       0,     0,     6,     0,     7,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,     0,    25,    26,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     2,     3,     0,     0,     0,
      27,     0,     0,     0,     0,   -33,     0,     4,     5,     0,
      28,     0,     0,    29,    30,     0,     0,     0,    31,    32,
       0,    33,     6,    34,    88,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,     0,    25,    26,     0,     0,     0,     1,
       0,     2,    87,     0,     0,     0,     0,     0,     0,     0,
      27,     0,     0,     4,     5,     0,     0,     0,     0,     0,
      28,     0,     0,    29,    30,     0,     0,     0,    31,    32,
      88,    33,     0,    34,     0,     0,     0,     0,     0,     1,
       0,     2,    87,     0,     0,     0,    22,     0,     0,     0,
      25,     0,     0,     4,     5,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    27,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,     0,     0,    29,
      30,     0,     0,     0,    31,    32,    22,    33,     0,     0,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    27,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,     0,     0,    29,
      30,     0,     0,     0,    31,    32,     0,    33
};

static const yytype_int16 yycheck[] =
{
      12,     6,    33,    95,     8,   169,    16,     3,     6,    36,
       9,     6,    73,    30,    19,    30,   192,     3,    34,     5,
       6,   267,    70,    27,   247,     6,    17,    18,    73,   252,
       6,    17,    18,    94,    75,    84,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,   292,    57,    34,    94,
      14,    15,    33,   102,    77,   104,   105,   106,    75,   108,
      73,    75,    13,    58,    50,    16,    78,    81,    54,    82,
      93,    66,    67,    68,    91,   251,    91,    76,    94,    70,
      73,   245,   305,    81,    70,   116,   113,    91,   119,   101,
      74,    70,    96,    97,    80,    74,    92,    83,    84,    90,
      91,    94,    88,    89,    73,    91,    77,    93,   118,    75,
     120,   121,   122,   123,    93,    73,   126,    81,    82,    73,
      73,   131,   153,    73,   134,    94,   157,   137,    73,   289,
      70,   141,     3,    70,     5,     6,    94,    82,    70,    92,
      94,   172,    92,   303,     6,   309,    17,    18,     3,   241,
       5,     6,    85,    86,    87,   159,    73,    71,    75,    73,
      71,    32,    73,    34,   195,    71,   171,    73,    71,    71,
      73,    73,    71,   185,    73,    71,    47,    73,    71,    50,
      73,     6,    71,    54,    73,    71,   217,    73,     3,     6,
       5,     6,    11,    12,     6,    83,    84,   313,   314,    70,
       6,     6,    17,    18,     0,    73,   298,    10,    75,    80,
      79,    77,    83,    84,    78,    80,    75,    88,    89,    34,
      91,    73,     6,   254,    74,   256,    70,   239,    81,    42,
     242,   243,    93,    93,    77,    50,   240,     6,   269,    54,
      77,   246,   247,    70,    70,    75,    74,   252,    93,    34,
      81,    93,    93,    93,    77,    70,     6,     6,    74,     6,
      75,    70,    37,    92,    75,    80,    19,    75,    83,    84,
       3,    93,    71,    88,    89,    34,    91,   234,   268,   231,
      16,   120,   118,   314,   296,   123,   121,     3,   122,     5,
       6,   137,   215,   126,    95,   300,    19,   171,   306,   131,
     305,    17,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    32,   329,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    54,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,     3,    -1,     5,     6,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    83,    84,    17,
      18,    -1,    88,    89,    -1,    91,    -1,    93,    94,    -1,
      -1,    -1,    -1,    -1,    32,    -1,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    54,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,     3,    -1,     5,     6,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    83,    84,    17,    18,    -1,
      88,    89,    -1,    91,    -1,    93,    94,    -1,    -1,    -1,
      -1,    -1,    32,    -1,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,    -1,     5,     6,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    75,    -1,    17,    18,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    88,    89,
      -1,    91,    32,    93,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    54,    55,    -1,    -1,    -1,     3,
      -1,     5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    88,    89,
      34,    91,    -1,    93,    -1,    -1,    -1,    -1,    -1,     3,
      -1,     5,     6,    -1,    -1,    -1,    50,    -1,    -1,    -1,
      54,    -1,    -1,    17,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    88,    89,    50,    91,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    88,    89,    -1,    91
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,     6,    17,    18,    32,    34,    35,    36,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    54,    55,    70,    80,    83,
      84,    88,    89,    91,    93,    96,    97,    98,    99,   103,
     104,   105,   106,   107,   109,   112,   113,   114,   115,   116,
     117,   118,   120,   122,   124,   126,   128,   129,   130,   133,
     134,   135,   137,   141,   142,   143,   144,   145,   146,   147,
     148,   150,   165,   166,   167,   169,   172,   174,    77,    93,
       6,   151,   152,   153,     6,    81,   163,     6,    34,    75,
     105,    70,    75,    75,   103,    70,    70,    70,   111,   112,
     128,    77,     6,   151,     6,     6,     6,     6,     6,   105,
     107,   136,    94,    97,     0,    98,    73,    75,     9,    76,
      10,    78,    79,    80,    13,    16,   119,    14,    15,    81,
      82,   121,    11,    12,   123,    83,    84,   125,    85,    86,
      87,   127,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    74,   108,   128,    17,    18,    70,    90,    91,
     132,    75,   103,     3,     5,     6,   138,   139,   140,    77,
     154,    73,    74,   163,     6,   164,    70,    75,   105,    42,
     106,   149,   150,   105,   105,    77,   103,   163,    75,   163,
     163,   163,    93,   163,    71,    73,    92,    94,   107,   114,
     107,   115,   116,   117,   118,   120,   122,   124,   126,   128,
     107,   107,   131,     6,   105,    73,    94,    77,     6,    58,
      66,    67,    68,   155,   156,   157,   158,   159,   161,   152,
     107,    70,    73,    82,    33,   100,   101,   102,   153,    71,
      70,    75,    71,    71,   103,    74,    93,    93,    93,    34,
      99,   173,    93,   107,    77,    71,    73,    92,   139,   107,
      81,   162,    30,    91,   100,     6,   102,    71,    73,    74,
     103,   105,   106,   103,   103,   155,   153,   168,   168,     6,
     170,   171,    94,    99,   168,   107,   107,   155,   160,    70,
       3,    92,    71,   154,   101,   107,    37,    71,    75,    75,
      73,    94,    94,    70,    74,    93,    73,    94,    94,    73,
      82,   160,    92,   154,    19,   103,    75,   106,   153,   160,
       3,   168,   171,   155,    71,   141,   107,   110,   141,    71,
      71,    94,   103
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    95,    96,    96,    97,    97,    98,    98,    99,   100,
     100,   100,   101,   101,   102,   102,   103,   103,   103,   103,
     103,   103,   103,   103,   103,   103,   103,   103,   103,   104,
     105,   105,   106,   106,   107,   107,   107,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   109,   110,
     110,   111,   112,   112,   113,   113,   114,   114,   115,   115,
     116,   116,   117,   117,   118,   118,   119,   119,   120,   120,
     121,   121,   121,   121,   122,   122,   123,   123,   124,   124,
     125,   125,   126,   126,   127,   127,   127,   128,   128,   129,
     129,   129,   129,   129,   129,   129,   130,   130,   130,   130,
     130,   131,   131,   131,   132,   132,   133,   133,   133,   133,
     133,   134,   134,   134,   134,   135,   136,   136,   136,   136,
     137,   138,   138,   138,   138,   139,   140,   140,   140,   141,
     141,   142,   142,   142,   143,   143,   144,   145,   146,   146,
     146,   147,   147,   147,   148,   148,   148,   149,   149,   150,
     150,   151,   151,   152,   152,   153,   154,   154,   155,   155,
     155,   155,   155,   156,   156,   156,   156,   157,   158,   159,
     160,   160,   160,   161,   161,   162,   163,   163,   164,   164,
     165,   166,   166,   167,   168,   168,   168,   168,   169,   170,
     170,   170,   171,   171,   171,   171,   172,   173,   173,   174
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     2,     1,     1,     1,     8,     1,
       3,     0,     1,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     2,
       1,     3,     1,     0,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     8,     1,
       1,     1,     1,     5,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     3,
       1,     1,     1,     3,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     3,     4,
       4,     1,     3,     0,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     3,     1,     3,     2,     0,
       4,     1,     3,     2,     0,     3,     1,     1,     1,     3,
       2,     1,     1,     1,     2,     3,     2,     2,     3,     4,
       3,     5,     7,     5,     5,     7,     9,     1,     1,     2,
       2,     1,     3,     1,     3,     2,     2,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     5,
       1,     3,     0,     1,     2,     3,     3,     0,     1,     3,
       6,     3,     6,     6,     1,     3,     2,     0,     6,     1,
       3,     2,     1,     4,     4,     3,     5,     1,     2,     6
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
#line 1966 "bison.tab.c"
    break;

  case 3: /* program: %empty  */
#line 69 "./src/parser/gens/bison.y"
                  {
        ptree_g = ptree_create(PTREE_EOF, 0);
    }
#line 1974 "bison.tab.c"
    break;

  case 4: /* source_elements: source_elements source_element  */
#line 75 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 1982 "bison.tab.c"
    break;

  case 5: /* source_elements: source_element  */
#line 78 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_SOURCE_ELEMENTS, 1, (yyvsp[0].ptree));
    }
#line 1990 "bison.tab.c"
    break;

  case 6: /* source_element: function_declaration  */
#line 84 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 1998 "bison.tab.c"
    break;

  case 7: /* source_element: statement  */
#line 87 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2006 "bison.tab.c"
    break;

  case 8: /* function_declaration: FUN WORD typed_parameters '(' parameters_list ')' type_annotation block_statement  */
#line 93 "./src/parser/gens/bison.y"
                                                                                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-6].str));
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_DECLARATION, 7, w, (yyvsp[-5].ptree), lp, (yyvsp[-3].ptree), rp, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2017 "bison.tab.c"
    break;

  case 9: /* parameters_list: parameter_is_mut  */
#line 102 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_LIST, 1, (yyvsp[0].ptree));
    }
#line 2025 "bison.tab.c"
    break;

  case 10: /* parameters_list: parameters_list ',' parameter_is_mut  */
#line 105 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2033 "bison.tab.c"
    break;

  case 11: /* parameters_list: %empty  */
#line 108 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 2041 "bison.tab.c"
    break;

  case 12: /* parameter_is_mut: parameter  */
#line 114 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_CONST, 1, (yyvsp[0].ptree));
    }
#line 2049 "bison.tab.c"
    break;

  case 13: /* parameter_is_mut: MUT parameter  */
#line 117 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_MUT, 1, (yyvsp[0].ptree));
    }
#line 2057 "bison.tab.c"
    break;

  case 14: /* parameter: identifier  */
#line 123 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 2065 "bison.tab.c"
    break;

  case 15: /* parameter: identifier '=' assignment_expression  */
#line 126 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 2074 "bison.tab.c"
    break;

  case 16: /* statement: expression_statement  */
#line 133 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2082 "bison.tab.c"
    break;

  case 17: /* statement: block_statement  */
#line 136 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2090 "bison.tab.c"
    break;

  case 18: /* statement: jump_statement  */
#line 139 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2098 "bison.tab.c"
    break;

  case 19: /* statement: labeled_statement  */
#line 142 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2106 "bison.tab.c"
    break;

  case 20: /* statement: selection_statement  */
#line 145 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2114 "bison.tab.c"
    break;

  case 21: /* statement: iteration_statement  */
#line 148 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2122 "bison.tab.c"
    break;

  case 22: /* statement: variable_statement ';'  */
#line 151 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2130 "bison.tab.c"
    break;

  case 23: /* statement: type_statement  */
#line 154 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2138 "bison.tab.c"
    break;

  case 24: /* statement: struct_statement  */
#line 157 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2146 "bison.tab.c"
    break;

  case 25: /* statement: union_statement  */
#line 160 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2154 "bison.tab.c"
    break;

  case 26: /* statement: enum_statement  */
#line 163 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2162 "bison.tab.c"
    break;

  case 27: /* statement: methods_statement  */
#line 166 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2170 "bison.tab.c"
    break;

  case 28: /* statement: class_statement  */
#line 169 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2178 "bison.tab.c"
    break;

  case 29: /* expression_statement: optional_expression ';'  */
#line 176 "./src/parser/gens/bison.y"
                            {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION_STATEMENT, 2, (yyvsp[-1].ptree), sc);
    }
#line 2187 "bison.tab.c"
    break;

  case 30: /* expression: assignment_expression  */
#line 183 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2195 "bison.tab.c"
    break;

  case 31: /* expression: expression ',' assignment_expression  */
#line 186 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 2, (yyvsp[0].ptree));
    }
#line 2203 "bison.tab.c"
    break;

  case 32: /* optional_expression: expression  */
#line 192 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2211 "bison.tab.c"
    break;

  case 33: /* optional_expression: %empty  */
#line 195 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2219 "bison.tab.c"
    break;

  case 34: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 201 "./src/parser/gens/bison.y"
                                                               {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2227 "bison.tab.c"
    break;

  case 35: /* assignment_expression: function_expression  */
#line 204 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2235 "bison.tab.c"
    break;

  case 36: /* assignment_expression: conditional_expression  */
#line 207 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2243 "bison.tab.c"
    break;

  case 37: /* assignment_operator: '='  */
#line 213 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_ASSIGN, 0);
    }
#line 2251 "bison.tab.c"
    break;

  case 38: /* assignment_operator: OR_ASSIGN  */
#line 216 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_OR_ASSIGN, 0);
    }
#line 2259 "bison.tab.c"
    break;

  case 39: /* assignment_operator: MUL_ASSIGN  */
#line 219 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MUL_ASSIGN, 0);
    }
#line 2267 "bison.tab.c"
    break;

  case 40: /* assignment_operator: DIV_ASSIGN  */
#line 222 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_DIV_ASSIGN, 0);
    }
#line 2275 "bison.tab.c"
    break;

  case 41: /* assignment_operator: MOD_ASSIGN  */
#line 225 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MOD_ASSIGN, 0);
    }
#line 2283 "bison.tab.c"
    break;

  case 42: /* assignment_operator: ADD_ASSIGN  */
#line 228 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_ADD_ASSIGN, 0);
    }
#line 2291 "bison.tab.c"
    break;

  case 43: /* assignment_operator: SUB_ASSIGN  */
#line 231 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SUB_ASSIGN, 0);
    }
#line 2299 "bison.tab.c"
    break;

  case 44: /* assignment_operator: SHL_ASSIGN  */
#line 234 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHL_ASSIGN, 0);
    }
#line 2307 "bison.tab.c"
    break;

  case 45: /* assignment_operator: SHR_ASSIGN  */
#line 237 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHR_ASSIGN, 0);
    }
#line 2315 "bison.tab.c"
    break;

  case 46: /* assignment_operator: AND_ASSIGN  */
#line 240 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_AND_ASSIGN, 0);
    }
#line 2323 "bison.tab.c"
    break;

  case 47: /* assignment_operator: XOR_ASSIGN  */
#line 243 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_XOR_ASSIGN, 0);
    }
#line 2331 "bison.tab.c"
    break;

  case 48: /* function_expression: FUN typed_parameters '(' parameters_list ')' type_annotation FORWARD_ARROW function_expression_body  */
#line 249 "./src/parser/gens/bison.y"
                                                                                                        {
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        ptree_t *fa = ptree_create(PTREE_FORWARD_ARROW, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_EXPRESSION, 7, (yyvsp[-6].ptree), lp, (yyvsp[-4].ptree), rp, (yyvsp[-2].ptree), fa, (yyvsp[0].ptree));
    }
#line 2342 "bison.tab.c"
    break;

  case 49: /* function_expression_body: assignment_expression  */
#line 258 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2350 "bison.tab.c"
    break;

  case 50: /* function_expression_body: block_statement  */
#line 261 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2358 "bison.tab.c"
    break;

  case 51: /* constant_expression: conditional_expression  */
#line 267 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2366 "bison.tab.c"
    break;

  case 52: /* conditional_expression: logical_or_expression  */
#line 273 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2374 "bison.tab.c"
    break;

  case 53: /* conditional_expression: logical_or_expression '?' assignment_expression ':' assignment_expression  */
#line 276 "./src/parser/gens/bison.y"
                                                                                {
        ptree_t *q = ptree_create(PTREE_QUESTION, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TERNARY_EXPRESSION, 5, (yyvsp[-4].ptree), q, (yyvsp[-2].ptree), sc, (yyvsp[0].ptree));
    }
#line 2384 "bison.tab.c"
    break;

  case 54: /* logical_or_expression: logical_and_expression  */
#line 284 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2392 "bison.tab.c"
    break;

  case 55: /* logical_or_expression: logical_or_expression OR logical_and_expression  */
#line 287 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *op = ptree_create(PTREE_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2401 "bison.tab.c"
    break;

  case 56: /* logical_and_expression: bitwise_or_expression  */
#line 294 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2409 "bison.tab.c"
    break;

  case 57: /* logical_and_expression: logical_and_expression AND bitwise_or_expression  */
#line 297 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2418 "bison.tab.c"
    break;

  case 58: /* bitwise_or_expression: bitwise_xor_expression  */
#line 304 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2426 "bison.tab.c"
    break;

  case 59: /* bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression  */
#line 307 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_BIT_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2435 "bison.tab.c"
    break;

  case 60: /* bitwise_xor_expression: bitwise_and_expression  */
#line 314 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2443 "bison.tab.c"
    break;

  case 61: /* bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression  */
#line 317 "./src/parser/gens/bison.y"
                                                        {
        ptree_t *op = ptree_create(PTREE_BIT_XOR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2452 "bison.tab.c"
    break;

  case 62: /* bitwise_and_expression: equality_expression  */
#line 324 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2460 "bison.tab.c"
    break;

  case 63: /* bitwise_and_expression: bitwise_and_expression '&' equality_expression  */
#line 327 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *op = ptree_create(PTREE_BIT_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2469 "bison.tab.c"
    break;

  case 64: /* equality_expression: relational_expression  */
#line 334 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2477 "bison.tab.c"
    break;

  case 65: /* equality_expression: equality_expression equality_operator relational_expression  */
#line 337 "./src/parser/gens/bison.y"
                                                                  {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2485 "bison.tab.c"
    break;

  case 66: /* equality_operator: IS_EQUAL  */
#line 343 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_IS_EQUAL, 0);
    }
#line 2493 "bison.tab.c"
    break;

  case 67: /* equality_operator: NOT_EQUAL  */
#line 346 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_NOT_EQUAL, 0);
    }
#line 2501 "bison.tab.c"
    break;

  case 68: /* relational_expression: shift_expression  */
#line 352 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2509 "bison.tab.c"
    break;

  case 69: /* relational_expression: relational_expression relational_operator shift_expression  */
#line 355 "./src/parser/gens/bison.y"
                                                                 {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2517 "bison.tab.c"
    break;

  case 70: /* relational_operator: '<'  */
#line 361 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_LESS, 0);
    }
#line 2525 "bison.tab.c"
    break;

  case 71: /* relational_operator: '>'  */
#line 364 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_GREATER, 0);
    }
#line 2533 "bison.tab.c"
    break;

  case 72: /* relational_operator: LESS_EQ  */
#line 367 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_LESS_EQ, 0);
    }
#line 2541 "bison.tab.c"
    break;

  case 73: /* relational_operator: GREATER_EQ  */
#line 370 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_GREATER_EQ, 0);
    }
#line 2549 "bison.tab.c"
    break;

  case 74: /* shift_expression: additive_expression  */
#line 376 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2557 "bison.tab.c"
    break;

  case 75: /* shift_expression: shift_expression shift_operator additive_expression  */
#line 379 "./src/parser/gens/bison.y"
                                                          {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2565 "bison.tab.c"
    break;

  case 76: /* shift_operator: LSHIFT  */
#line 385 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create(PTREE_LSHIFT, 0);
    }
#line 2573 "bison.tab.c"
    break;

  case 77: /* shift_operator: RSHIFT  */
#line 388 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_RSHIFT, 0);
    }
#line 2581 "bison.tab.c"
    break;

  case 78: /* additive_expression: multiplicative_expression  */
#line 394 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2589 "bison.tab.c"
    break;

  case 79: /* additive_expression: additive_expression additive_operator multiplicative_expression  */
#line 397 "./src/parser/gens/bison.y"
                                                                      {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2597 "bison.tab.c"
    break;

  case 80: /* additive_operator: '+'  */
#line 403 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2605 "bison.tab.c"
    break;

  case 81: /* additive_operator: '-'  */
#line 406 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2613 "bison.tab.c"
    break;

  case 82: /* multiplicative_expression: unary_expression  */
#line 412 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2621 "bison.tab.c"
    break;

  case 83: /* multiplicative_expression: multiplicative_expression multiplicative_operator unary_expression  */
#line 415 "./src/parser/gens/bison.y"
                                                                         {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2629 "bison.tab.c"
    break;

  case 84: /* multiplicative_operator: '*'  */
#line 421 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_MULTIPLY, 0);
    }
#line 2637 "bison.tab.c"
    break;

  case 85: /* multiplicative_operator: '/'  */
#line 424 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_DIVIDE, 0);
    }
#line 2645 "bison.tab.c"
    break;

  case 86: /* multiplicative_operator: '%'  */
#line 427 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MODULO, 0);
    }
#line 2653 "bison.tab.c"
    break;

  case 87: /* unary_expression: postfix_expression  */
#line 433 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2661 "bison.tab.c"
    break;

  case 88: /* unary_expression: unary_operator unary_expression  */
#line 436 "./src/parser/gens/bison.y"
                                      {
        (yyval.ptree) = ptree_create(PTREE_UNARY, 2, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2669 "bison.tab.c"
    break;

  case 89: /* unary_operator: INCREMENT  */
#line 442 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2677 "bison.tab.c"
    break;

  case 90: /* unary_operator: DECREMENT  */
#line 445 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2685 "bison.tab.c"
    break;

  case 91: /* unary_operator: '!'  */
#line 448 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_NOT, 0);
    }
#line 2693 "bison.tab.c"
    break;

  case 92: /* unary_operator: '~'  */
#line 451 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_TILDE, 0);
    }
#line 2701 "bison.tab.c"
    break;

  case 93: /* unary_operator: '+'  */
#line 454 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2709 "bison.tab.c"
    break;

  case 94: /* unary_operator: '-'  */
#line 457 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2717 "bison.tab.c"
    break;

  case 95: /* unary_operator: '&'  */
#line 460 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_BIT_AND, 0);
    }
#line 2725 "bison.tab.c"
    break;

  case 96: /* postfix_expression: primary_expression  */
#line 466 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2733 "bison.tab.c"
    break;

  case 97: /* postfix_expression: postfix_expression postfix_operator  */
#line 469 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_create(PTREE_POSTFIX, 2, (yyvsp[0].ptree), (yyvsp[-1].ptree));
    }
#line 2741 "bison.tab.c"
    break;

  case 98: /* postfix_expression: postfix_expression '.' WORD  */
#line 472 "./src/parser/gens/bison.y"
                                  {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        ptree_t *dot =ptree_create(PTREE_DOT, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_ACCESS, 3, (yyvsp[-2].ptree), dot, w);
    }
#line 2751 "bison.tab.c"
    break;

  case 99: /* postfix_expression: postfix_expression '[' expression ']'  */
#line 477 "./src/parser/gens/bison.y"
                                            {
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_ACCESS, 3, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2761 "bison.tab.c"
    break;

  case 100: /* postfix_expression: postfix_expression '(' arguments ')'  */
#line 482 "./src/parser/gens/bison.y"
                                           {
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_CALL, 4, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2771 "bison.tab.c"
    break;

  case 101: /* arguments: assignment_expression  */
#line 490 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ARGUMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2779 "bison.tab.c"
    break;

  case 102: /* arguments: arguments ',' assignment_expression  */
#line 493 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2787 "bison.tab.c"
    break;

  case 103: /* arguments: %empty  */
#line 496 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2795 "bison.tab.c"
    break;

  case 104: /* postfix_operator: INCREMENT  */
#line 502 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2803 "bison.tab.c"
    break;

  case 105: /* postfix_operator: DECREMENT  */
#line 505 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2811 "bison.tab.c"
    break;

  case 106: /* primary_expression: WORD  */
#line 511 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 2819 "bison.tab.c"
    break;

  case 107: /* primary_expression: literal  */
#line 514 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2827 "bison.tab.c"
    break;

  case 108: /* primary_expression: array_literal  */
#line 517 "./src/parser/gens/bison.y"
                    {
       (yyval.ptree) = (yyvsp[0].ptree); 
    }
#line 2835 "bison.tab.c"
    break;

  case 109: /* primary_expression: object_literal  */
#line 520 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2843 "bison.tab.c"
    break;

  case 110: /* primary_expression: '(' expression ')'  */
#line 523 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2853 "bison.tab.c"
    break;

  case 111: /* literal: NUMBER  */
#line 531 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2861 "bison.tab.c"
    break;

  case 112: /* literal: TRUE_VAL  */
#line 534 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create_bool(1);
    }
#line 2869 "bison.tab.c"
    break;

  case 113: /* literal: FALSE_VAL  */
#line 537 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create_bool(0);
    }
#line 2877 "bison.tab.c"
    break;

  case 114: /* literal: STRING  */
#line 540 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2885 "bison.tab.c"
    break;

  case 115: /* array_literal: '[' element_list ']'  */
#line 546 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *right = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_LITERAL, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2895 "bison.tab.c"
    break;

  case 116: /* element_list: assignment_expression  */
#line 554 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ELEMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2903 "bison.tab.c"
    break;

  case 117: /* element_list: element_list ',' assignment_expression  */
#line 557 "./src/parser/gens/bison.y"
                                             {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2911 "bison.tab.c"
    break;

  case 118: /* element_list: element_list ','  */
#line 560 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2919 "bison.tab.c"
    break;

  case 119: /* element_list: %empty  */
#line 563 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2927 "bison.tab.c"
    break;

  case 120: /* object_literal: WORD '{' keyvalue_list '}'  */
#line 569 "./src/parser/gens/bison.y"
                               {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_LITERAL, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 2938 "bison.tab.c"
    break;

  case 121: /* keyvalue_list: keyvalue  */
#line 578 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE_LIST, 1, (yyvsp[0].ptree));
    }
#line 2946 "bison.tab.c"
    break;

  case 122: /* keyvalue_list: keyvalue_list ',' keyvalue  */
#line 581 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2954 "bison.tab.c"
    break;

  case 123: /* keyvalue_list: keyvalue_list ','  */
#line 584 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2962 "bison.tab.c"
    break;

  case 124: /* keyvalue_list: %empty  */
#line 587 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2970 "bison.tab.c"
    break;

  case 125: /* keyvalue: property_name ':' assignment_expression  */
#line 593 "./src/parser/gens/bison.y"
                                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 2979 "bison.tab.c"
    break;

  case 126: /* property_name: WORD  */
#line 600 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_word((yyvsp[0].str));
    }
#line 2987 "bison.tab.c"
    break;

  case 127: /* property_name: STRING  */
#line 603 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2995 "bison.tab.c"
    break;

  case 128: /* property_name: NUMBER  */
#line 606 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 3003 "bison.tab.c"
    break;

  case 129: /* block_statement: '{' source_elements '}'  */
#line 612 "./src/parser/gens/bison.y"
                            {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, (yyvsp[-1].ptree), right);
    }
#line 3014 "bison.tab.c"
    break;

  case 130: /* block_statement: '{' '}'  */
#line 618 "./src/parser/gens/bison.y"
              {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, empty, right);
    }
#line 3025 "bison.tab.c"
    break;

  case 131: /* jump_statement: return_statement  */
#line 627 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3033 "bison.tab.c"
    break;

  case 132: /* jump_statement: break_statement  */
#line 630 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3041 "bison.tab.c"
    break;

  case 133: /* jump_statement: continue_statement  */
#line 633 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3049 "bison.tab.c"
    break;

  case 134: /* return_statement: RETURN ';'  */
#line 638 "./src/parser/gens/bison.y"
               {
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, empty);
    }
#line 3058 "bison.tab.c"
    break;

  case 135: /* return_statement: RETURN expression ';'  */
#line 642 "./src/parser/gens/bison.y"
                            {
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, (yyvsp[-1].ptree));
    }
#line 3066 "bison.tab.c"
    break;

  case 136: /* break_statement: BREAK ';'  */
#line 648 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_BREAK, 0);
    }
#line 3074 "bison.tab.c"
    break;

  case 137: /* continue_statement: CONTINUE ';'  */
#line 654 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_CONTINUE, 0);
    }
#line 3082 "bison.tab.c"
    break;

  case 138: /* labeled_statement: WORD ':' statement  */
#line 660 "./src/parser/gens/bison.y"
                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LABEL, 3, w, c, (yyvsp[0].ptree));
    }
#line 3092 "bison.tab.c"
    break;

  case 139: /* labeled_statement: CASE constant_expression ':' statement  */
#line 665 "./src/parser/gens/bison.y"
                                             {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CASE_LABEL, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 3101 "bison.tab.c"
    break;

  case 140: /* labeled_statement: DEFAULT ':' statement  */
#line 669 "./src/parser/gens/bison.y"
                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_DEFAULT_LABEL, 2, c, (yyvsp[0].ptree));
    }
#line 3110 "bison.tab.c"
    break;

  case 141: /* selection_statement: IF '(' expression ')' statement  */
#line 677 "./src/parser/gens/bison.y"
    {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3121 "bison.tab.c"
    break;

  case 142: /* selection_statement: IF '(' expression ')' statement ELSE statement  */
#line 683 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *e = ptree_create(PTREE_ELSE, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF_ELSE, 6, left, (yyvsp[-4].ptree), right, (yyvsp[-2].ptree), e, (yyvsp[0].ptree));
    }
#line 3133 "bison.tab.c"
    break;

  case 143: /* selection_statement: SWITCH '(' expression ')' statement  */
#line 690 "./src/parser/gens/bison.y"
                                          {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_SWITCH, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3144 "bison.tab.c"
    break;

  case 144: /* iteration_statement: WHILE '(' expression ')' statement  */
#line 699 "./src/parser/gens/bison.y"
                                       {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_WHILE, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3154 "bison.tab.c"
    break;

  case 145: /* iteration_statement: DO statement WHILE '(' expression ')' ';'  */
#line 704 "./src/parser/gens/bison.y"
                                                {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *wh = ptree_create(PTREE_WHILE, 0);

        (yyval.ptree) = ptree_create(PTREE_DO, 6, (yyvsp[-5].ptree), wh, left, (yyvsp[-2].ptree), right, sc);
    }
#line 3167 "bison.tab.c"
    break;

  case 146: /* iteration_statement: FOR '(' for_initialization ';' optional_expression ';' optional_expression ')' statement  */
#line 712 "./src/parser/gens/bison.y"
                                                                                               {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *sc2 = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_FOR, 8, left, (yyvsp[-6].ptree), sc, (yyvsp[-4].ptree), sc2, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3179 "bison.tab.c"
    break;

  case 147: /* for_initialization: optional_expression  */
#line 722 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3187 "bison.tab.c"
    break;

  case 148: /* for_initialization: variable_statement  */
#line 725 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3195 "bison.tab.c"
    break;

  case 149: /* variable_statement: LET variable_list  */
#line 731 "./src/parser/gens/bison.y"
                      {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LET_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3204 "bison.tab.c"
    break;

  case 150: /* variable_statement: CONST variable_list  */
#line 735 "./src/parser/gens/bison.y"
                          {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CONST_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3213 "bison.tab.c"
    break;

  case 151: /* variable_list: variable  */
#line 742 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3221 "bison.tab.c"
    break;

  case 152: /* variable_list: variable_list ',' variable  */
#line 745 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3229 "bison.tab.c"
    break;

  case 153: /* variable: identifier  */
#line 751 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 3237 "bison.tab.c"
    break;

  case 154: /* variable: identifier '=' assignment_expression  */
#line 754 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 3246 "bison.tab.c"
    break;

  case 155: /* identifier: WORD type_annotation  */
#line 761 "./src/parser/gens/bison.y"
                         {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_IDENTIFIER, 2, w, (yyvsp[0].ptree));
    }
#line 3255 "bison.tab.c"
    break;

  case 156: /* type_annotation: ':' type  */
#line 768 "./src/parser/gens/bison.y"
             {
        ptree_t *sc = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ANNOTATION, 2, sc, (yyvsp[0].ptree)); 
    }
#line 3264 "bison.tab.c"
    break;

  case 157: /* type_annotation: %empty  */
#line 772 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3272 "bison.tab.c"
    break;

  case 158: /* type: primitive_type  */
#line 778 "./src/parser/gens/bison.y"
                   {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3280 "bison.tab.c"
    break;

  case 159: /* type: array_type  */
#line 781 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3288 "bison.tab.c"
    break;

  case 160: /* type: tuple_type  */
#line 784 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3296 "bison.tab.c"
    break;

  case 161: /* type: function_type  */
#line 787 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = (yyvsp[0].ptree);        
    }
#line 3304 "bison.tab.c"
    break;

  case 162: /* type: type_reference  */
#line 790 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3312 "bison.tab.c"
    break;

  case 163: /* primitive_type: T_VOID  */
#line 796 "./src/parser/gens/bison.y"
           { 
        (yyval.ptree) = ptree_create(PTREE_VOID_TYPE, 0);
    }
#line 3320 "bison.tab.c"
    break;

  case 164: /* primitive_type: S32  */
#line 799 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_INTEGER_TYPE, 0);
    }
#line 3328 "bison.tab.c"
    break;

  case 165: /* primitive_type: T_STRING  */
#line 802 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_STRING_TYPE, 0);
    }
#line 3336 "bison.tab.c"
    break;

  case 166: /* primitive_type: T_BOOLEAN  */
#line 805 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_BOOLEAN_TYPE, 0);
    }
#line 3344 "bison.tab.c"
    break;

  case 167: /* array_type: type '[' ']'  */
#line 811 "./src/parser/gens/bison.y"
                 {
        ptree_t *lb = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_TYPE, 3, (yyvsp[-2].ptree), lb, rb);
    }
#line 3354 "bison.tab.c"
    break;

  case 168: /* tuple_type: type '[' NUMBER ']'  */
#line 819 "./src/parser/gens/bison.y"
                        {
        ptree_t *num = ptree_create_num((yyvsp[-1].num));
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_TUPLE_TYPE, 4, (yyvsp[-3].ptree), lb, num, rb);
    }
#line 3365 "bison.tab.c"
    break;

  case 169: /* function_type: type BACKWARD_ARROW '(' types_list ')'  */
#line 828 "./src/parser/gens/bison.y"
                                           {
        ptree_t *a = ptree_create(PTREE_BACKWARD_ARROW, 0);
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_TYPE, 5, (yyvsp[-4].ptree), a, lb, (yyvsp[-1].ptree), rb);
    }
#line 3376 "bison.tab.c"
    break;

  case 170: /* types_list: type  */
#line 837 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create(PTREE_TYPE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3384 "bison.tab.c"
    break;

  case 171: /* types_list: types_list ',' type  */
#line 840 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3392 "bison.tab.c"
    break;

  case 172: /* types_list: %empty  */
#line 843 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0); 
    }
#line 3400 "bison.tab.c"
    break;

  case 173: /* type_reference: WORD  */
#line 849 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 1, w);
    }
#line 3409 "bison.tab.c"
    break;

  case 174: /* type_reference: WORD typed_arguments  */
#line 853 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 2, w, (yyvsp[0].ptree));
    }
#line 3418 "bison.tab.c"
    break;

  case 175: /* typed_arguments: '<' types_list '>'  */
#line 860 "./src/parser/gens/bison.y"
                       {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_ARGUMENTS, 3, l, (yyvsp[-1].ptree), g);
    }
#line 3428 "bison.tab.c"
    break;

  case 176: /* typed_parameters: '<' type_params '>'  */
#line 868 "./src/parser/gens/bison.y"
                        {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_PARAMETERS, 3, l, (yyvsp[-1].ptree), g); 
    }
#line 3438 "bison.tab.c"
    break;

  case 177: /* typed_parameters: %empty  */
#line 873 "./src/parser/gens/bison.y"
                  {
         (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3446 "bison.tab.c"
    break;

  case 178: /* type_params: WORD  */
#line 879 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_PARAMS, 1, w); 
    }
#line 3455 "bison.tab.c"
    break;

  case 179: /* type_params: type_params ',' WORD  */
#line 883 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, w);
    }
#line 3464 "bison.tab.c"
    break;

  case 180: /* type_statement: TYPE WORD typed_parameters '=' type ';'  */
#line 890 "./src/parser/gens/bison.y"
                                            {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ALIAS, 5, w, (yyvsp[-3].ptree), eq, (yyvsp[-1].ptree), sc); 
    }
#line 3475 "bison.tab.c"
    break;

  case 181: /* struct_statement: STRUCT WORD ';'  */
#line 899 "./src/parser/gens/bison.y"
                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_UNIT_STRUCT, 1, w);
    }
#line 3484 "bison.tab.c"
    break;

  case 182: /* struct_statement: STRUCT WORD typed_parameters '{' field_list '}'  */
#line 903 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_STRUCT, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3495 "bison.tab.c"
    break;

  case 183: /* union_statement: UNION WORD typed_parameters '{' field_list '}'  */
#line 912 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_UNION, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3506 "bison.tab.c"
    break;

  case 184: /* field_list: identifier  */
#line 921 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_FIELDS, 1, (yyvsp[0].ptree));
    }
#line 3514 "bison.tab.c"
    break;

  case 185: /* field_list: field_list ',' identifier  */
#line 924 "./src/parser/gens/bison.y"
                                {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3522 "bison.tab.c"
    break;

  case 186: /* field_list: field_list ','  */
#line 927 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3530 "bison.tab.c"
    break;

  case 187: /* field_list: %empty  */
#line 930 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3538 "bison.tab.c"
    break;

  case 188: /* enum_statement: ENUM WORD typed_parameters '{' enum_members '}'  */
#line 936 "./src/parser/gens/bison.y"
                                                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3549 "bison.tab.c"
    break;

  case 189: /* enum_members: enum_member  */
#line 945 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_ENUM_MEMBERS, 1, (yyvsp[0].ptree));
    }
#line 3557 "bison.tab.c"
    break;

  case 190: /* enum_members: enum_members ',' enum_member  */
#line 948 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3565 "bison.tab.c"
    break;

  case 191: /* enum_members: enum_members ','  */
#line 951 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3573 "bison.tab.c"
    break;

  case 192: /* enum_member: WORD  */
#line 957 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 3581 "bison.tab.c"
    break;

  case 193: /* enum_member: WORD '(' types_list ')'  */
#line 960 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LPRN, 0);
        ptree_t *right = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3592 "bison.tab.c"
    break;

  case 194: /* enum_member: WORD '{' field_list '}'  */
#line 966 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_NAMED_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3603 "bison.tab.c"
    break;

  case 195: /* enum_member: WORD '=' NUMBER  */
#line 972 "./src/parser/gens/bison.y"
                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *num = ptree_create_num((yyvsp[0].num));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_DEFAULT, 3, w, eq, num);
    }
#line 3614 "bison.tab.c"
    break;

  case 196: /* methods_statement: METHODS WORD '{' method_list '}'  */
#line 981 "./src/parser/gens/bison.y"
                                     {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_METHODS, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3625 "bison.tab.c"
    break;

  case 197: /* method_list: function_declaration  */
#line 990 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = ptree_create(PTREE_METHODS_LIST, 1, (yyvsp[0].ptree));
    }
#line 3633 "bison.tab.c"
    break;

  case 198: /* method_list: method_list function_declaration  */
#line 993 "./src/parser/gens/bison.y"
                                       {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 3641 "bison.tab.c"
    break;

  case 199: /* class_statement: CLASS WORD typed_parameters '{' field_list '}'  */
#line 999 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_CLASS, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3652 "bison.tab.c"
    break;


#line 3656 "bison.tab.c"

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

#line 1008 "./src/parser/gens/bison.y"
