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
  YYSYMBOL_T_INTEGER = 55,                 /* T_INTEGER  */
  YYSYMBOL_T_STRING = 56,                  /* T_STRING  */
  YYSYMBOL_T_BOOLEAN = 57,                 /* T_BOOLEAN  */
  YYSYMBOL_T_VOID = 58,                    /* T_VOID  */
  YYSYMBOL_59_ = 59,                       /* '('  */
  YYSYMBOL_60_ = 60,                       /* ')'  */
  YYSYMBOL_LOWER_THAN_ELSE = 61,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_62_ = 62,                       /* ','  */
  YYSYMBOL_63_ = 63,                       /* '='  */
  YYSYMBOL_64_ = 64,                       /* ';'  */
  YYSYMBOL_65_ = 65,                       /* '?'  */
  YYSYMBOL_66_ = 66,                       /* ':'  */
  YYSYMBOL_67_ = 67,                       /* '|'  */
  YYSYMBOL_68_ = 68,                       /* '^'  */
  YYSYMBOL_69_ = 69,                       /* '&'  */
  YYSYMBOL_70_ = 70,                       /* '<'  */
  YYSYMBOL_71_ = 71,                       /* '>'  */
  YYSYMBOL_72_ = 72,                       /* '+'  */
  YYSYMBOL_73_ = 73,                       /* '-'  */
  YYSYMBOL_74_ = 74,                       /* '*'  */
  YYSYMBOL_75_ = 75,                       /* '/'  */
  YYSYMBOL_76_ = 76,                       /* '%'  */
  YYSYMBOL_77_ = 77,                       /* '!'  */
  YYSYMBOL_78_ = 78,                       /* '~'  */
  YYSYMBOL_79_ = 79,                       /* '.'  */
  YYSYMBOL_80_ = 80,                       /* '['  */
  YYSYMBOL_81_ = 81,                       /* ']'  */
  YYSYMBOL_82_ = 82,                       /* '{'  */
  YYSYMBOL_83_ = 83,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 84,                  /* $accept  */
  YYSYMBOL_program = 85,                   /* program  */
  YYSYMBOL_source_elements = 86,           /* source_elements  */
  YYSYMBOL_source_element = 87,            /* source_element  */
  YYSYMBOL_function_declaration = 88,      /* function_declaration  */
  YYSYMBOL_parameters_list = 89,           /* parameters_list  */
  YYSYMBOL_parameter_is_mut = 90,          /* parameter_is_mut  */
  YYSYMBOL_parameter = 91,                 /* parameter  */
  YYSYMBOL_statement = 92,                 /* statement  */
  YYSYMBOL_expression_statement = 93,      /* expression_statement  */
  YYSYMBOL_expression = 94,                /* expression  */
  YYSYMBOL_optional_expression = 95,       /* optional_expression  */
  YYSYMBOL_assignment_expression = 96,     /* assignment_expression  */
  YYSYMBOL_assignment_operator = 97,       /* assignment_operator  */
  YYSYMBOL_function_expression = 98,       /* function_expression  */
  YYSYMBOL_function_expression_body = 99,  /* function_expression_body  */
  YYSYMBOL_constant_expression = 100,      /* constant_expression  */
  YYSYMBOL_conditional_expression = 101,   /* conditional_expression  */
  YYSYMBOL_logical_or_expression = 102,    /* logical_or_expression  */
  YYSYMBOL_logical_and_expression = 103,   /* logical_and_expression  */
  YYSYMBOL_bitwise_or_expression = 104,    /* bitwise_or_expression  */
  YYSYMBOL_bitwise_xor_expression = 105,   /* bitwise_xor_expression  */
  YYSYMBOL_bitwise_and_expression = 106,   /* bitwise_and_expression  */
  YYSYMBOL_equality_expression = 107,      /* equality_expression  */
  YYSYMBOL_equality_operator = 108,        /* equality_operator  */
  YYSYMBOL_relational_expression = 109,    /* relational_expression  */
  YYSYMBOL_relational_operator = 110,      /* relational_operator  */
  YYSYMBOL_shift_expression = 111,         /* shift_expression  */
  YYSYMBOL_shift_operator = 112,           /* shift_operator  */
  YYSYMBOL_additive_expression = 113,      /* additive_expression  */
  YYSYMBOL_additive_operator = 114,        /* additive_operator  */
  YYSYMBOL_multiplicative_expression = 115, /* multiplicative_expression  */
  YYSYMBOL_multiplicative_operator = 116,  /* multiplicative_operator  */
  YYSYMBOL_unary_expression = 117,         /* unary_expression  */
  YYSYMBOL_unary_operator = 118,           /* unary_operator  */
  YYSYMBOL_postfix_expression = 119,       /* postfix_expression  */
  YYSYMBOL_arguments = 120,                /* arguments  */
  YYSYMBOL_postfix_operator = 121,         /* postfix_operator  */
  YYSYMBOL_primary_expression = 122,       /* primary_expression  */
  YYSYMBOL_literal = 123,                  /* literal  */
  YYSYMBOL_array_literal = 124,            /* array_literal  */
  YYSYMBOL_element_list = 125,             /* element_list  */
  YYSYMBOL_object_literal = 126,           /* object_literal  */
  YYSYMBOL_keyvalue_list = 127,            /* keyvalue_list  */
  YYSYMBOL_keyvalue = 128,                 /* keyvalue  */
  YYSYMBOL_property_name = 129,            /* property_name  */
  YYSYMBOL_block_statement = 130,          /* block_statement  */
  YYSYMBOL_jump_statement = 131,           /* jump_statement  */
  YYSYMBOL_return_statement = 132,         /* return_statement  */
  YYSYMBOL_break_statement = 133,          /* break_statement  */
  YYSYMBOL_continue_statement = 134,       /* continue_statement  */
  YYSYMBOL_labeled_statement = 135,        /* labeled_statement  */
  YYSYMBOL_selection_statement = 136,      /* selection_statement  */
  YYSYMBOL_iteration_statement = 137,      /* iteration_statement  */
  YYSYMBOL_for_initialization = 138,       /* for_initialization  */
  YYSYMBOL_variable_statement = 139,       /* variable_statement  */
  YYSYMBOL_variable_list = 140,            /* variable_list  */
  YYSYMBOL_variable = 141,                 /* variable  */
  YYSYMBOL_identifier = 142,               /* identifier  */
  YYSYMBOL_type_annotation = 143,          /* type_annotation  */
  YYSYMBOL_type = 144,                     /* type  */
  YYSYMBOL_primitive_type = 145,           /* primitive_type  */
  YYSYMBOL_array_type = 146,               /* array_type  */
  YYSYMBOL_tuple_type = 147,               /* tuple_type  */
  YYSYMBOL_function_type = 148,            /* function_type  */
  YYSYMBOL_types_list = 149,               /* types_list  */
  YYSYMBOL_type_reference = 150,           /* type_reference  */
  YYSYMBOL_typed_arguments = 151,          /* typed_arguments  */
  YYSYMBOL_typed_parameters = 152,         /* typed_parameters  */
  YYSYMBOL_type_params = 153,              /* type_params  */
  YYSYMBOL_type_statement = 154,           /* type_statement  */
  YYSYMBOL_struct_statement = 155,         /* struct_statement  */
  YYSYMBOL_union_statement = 156,          /* union_statement  */
  YYSYMBOL_field_list = 157,               /* field_list  */
  YYSYMBOL_enum_statement = 158,           /* enum_statement  */
  YYSYMBOL_enum_members = 159,             /* enum_members  */
  YYSYMBOL_enum_member = 160,              /* enum_member  */
  YYSYMBOL_methods_statement = 161,        /* methods_statement  */
  YYSYMBOL_method_list = 162               /* method_list  */
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
#define YYLAST   614

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  84
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  79
/* YYNRULES -- Number of rules.  */
#define YYNRULES  197
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  326

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   314


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
       2,     2,     2,    77,     2,     2,     2,    76,    69,     2,
      59,    60,    74,    72,    62,    73,    79,    75,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    66,    64,
      70,    63,    71,    65,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    80,     2,    81,    68,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    82,    67,    83,    78,     2,     2,     2,
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
      55,    56,    57,    58,    61
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    65,    65,    69,    75,    78,    84,    87,    93,   102,
     105,   108,   114,   117,   123,   126,   133,   136,   139,   142,
     145,   148,   151,   154,   157,   160,   163,   166,   173,   180,
     183,   189,   192,   198,   201,   204,   210,   213,   216,   219,
     222,   225,   228,   231,   234,   237,   240,   246,   254,   257,
     263,   269,   272,   280,   283,   290,   293,   300,   303,   310,
     313,   320,   323,   330,   333,   339,   342,   348,   351,   357,
     360,   363,   366,   372,   375,   381,   384,   390,   393,   399,
     402,   408,   411,   417,   420,   423,   429,   432,   438,   441,
     444,   447,   450,   453,   456,   462,   465,   468,   473,   478,
     486,   489,   492,   498,   501,   507,   510,   513,   516,   519,
     527,   530,   533,   536,   542,   550,   553,   556,   559,   565,
     574,   577,   580,   583,   589,   596,   599,   602,   608,   614,
     623,   626,   629,   634,   638,   644,   650,   656,   661,   665,
     672,   679,   686,   695,   700,   708,   718,   721,   727,   731,
     738,   741,   747,   750,   757,   764,   768,   774,   777,   780,
     783,   786,   792,   795,   798,   801,   807,   815,   824,   833,
     836,   839,   845,   849,   856,   864,   869,   875,   879,   886,
     895,   899,   908,   917,   920,   923,   926,   932,   941,   944,
     947,   953,   956,   962,   968,   977,   986,   989
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
  "TRUE_VAL", "ENUM", "METHODS", "MATCH", "FALSE_VAL", "T_INTEGER",
  "T_STRING", "T_BOOLEAN", "T_VOID", "'('", "')'", "LOWER_THAN_ELSE",
  "','", "'='", "';'", "'?'", "':'", "'|'", "'^'", "'&'", "'<'", "'>'",
  "'+'", "'-'", "'*'", "'/'", "'%'", "'!'", "'~'", "'.'", "'['", "']'",
  "'{'", "'}'", "$accept", "program", "source_elements", "source_element",
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
  "enum_member", "methods_statement", "method_list", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-246)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-33)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     393,  -246,  -246,   -23,  -246,  -246,    24,     2,   186,    -6,
     -24,    17,   450,    -1,    10,    53,   534,    52,    30,    24,
     121,   130,  -246,   162,   167,  -246,   500,  -246,  -246,  -246,
    -246,  -246,   500,   279,   153,   393,  -246,  -246,  -246,  -246,
     114,   120,  -246,  -246,  -246,     1,   175,   119,   122,   124,
      16,     5,   147,    71,    29,   189,   534,    -2,  -246,  -246,
    -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,
     131,  -246,  -246,  -246,  -246,  -246,   450,   132,   133,   126,
    -246,   135,   137,   188,   163,   142,   137,  -246,    59,   500,
    -246,  -246,   154,   128,   500,   500,   159,  -246,  -246,   450,
     137,   126,    37,   137,   137,   146,    66,  -246,   -29,  -246,
     336,  -246,  -246,   500,  -246,   534,   500,   534,   534,   534,
     534,  -246,  -246,   534,  -246,  -246,  -246,  -246,   534,  -246,
    -246,   534,  -246,  -246,   534,  -246,  -246,  -246,   534,  -246,
    -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,  -246,
     500,  -246,  -246,  -246,   500,   223,   500,  -246,  -246,  -246,
    -246,  -246,  -246,   -44,  -246,   164,    93,  -246,    24,   500,
     172,  -246,   -11,    15,  -246,    70,   174,  -246,   171,  -246,
      80,    94,   450,  -246,   176,  -246,   160,   161,   165,   207,
    -246,   500,  -246,  -246,  -246,   175,   178,   119,   122,   124,
      16,     5,   147,    71,    29,  -246,  -246,  -246,    95,  -246,
     -16,   132,  -246,   500,   179,  -246,  -246,  -246,  -246,    14,
    -246,  -246,  -246,  -246,  -246,  -246,  -246,    15,   240,  -246,
      24,   104,  -246,  -246,   185,   450,   500,   500,   450,   450,
    -246,    93,    24,    24,   245,   247,  -246,   -10,  -246,   500,
    -246,   500,  -246,  -246,  -246,    93,  -246,   195,     3,   105,
    -246,  -246,   133,    15,   500,   219,   109,   193,  -246,  -246,
     -19,  -246,   -34,   -28,   -32,   -27,  -246,  -246,  -246,  -246,
    -246,    14,     0,    93,   180,  -246,   133,   229,  -246,  -246,
     450,   198,   500,  -246,    24,  -246,  -246,    93,   262,    24,
     245,  -246,    93,  -246,   112,  -246,   187,    20,  -246,  -246,
     208,  -246,   117,  -246,   -20,  -246,    14,  -246,  -246,  -246,
    -246,  -246,   450,  -246,  -246,  -246
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
    -246,  -246,   234,   -22,  -180,    43,     8,    42,   -12,  -246,
      -4,   -90,   -30,  -246,  -246,  -246,  -246,   257,  -246,   166,
     157,   158,   156,   168,  -246,   169,  -246,   151,  -246,   149,
    -246,   152,  -246,    -9,  -246,  -246,  -246,  -246,  -246,  -246,
    -246,  -246,  -246,  -246,    72,  -246,  -126,  -246,  -246,  -246,
    -246,  -246,  -246,  -246,  -246,   194,   271,   123,    -5,  -245,
    -161,  -246,  -246,  -246,  -246,  -201,  -246,  -246,    13,  -246,
    -246,  -246,  -246,  -231,  -246,  -246,    -7,  -246,  -246
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
      92,    81,   107,   177,    88,   219,   284,    98,    82,   246,
     115,   257,   273,   112,    81,   152,   153,   287,   211,   124,
     125,    78,   106,     1,   245,     2,    85,   297,   294,   121,
      78,   298,   122,   191,   294,   300,   100,     4,     5,   212,
      90,   306,   294,    76,   257,   293,   113,   151,   230,   295,
     299,   228,   192,    89,    86,   296,   301,   154,    93,    77,
     229,   258,   302,   324,   159,   252,   116,   278,   314,    94,
      22,   303,    83,   277,    25,   126,   127,   155,   156,    26,
     270,    91,   304,   194,   285,   175,   196,   183,   112,    27,
     180,   181,    28,    29,   258,   170,   312,    30,    31,   214,
      32,   185,    33,   135,   136,   137,    98,    83,    98,    98,
      98,    98,    95,   184,    98,   186,   187,   188,    99,    98,
     206,   113,    98,   174,   207,    98,   190,   102,   113,   205,
     235,     1,   113,     2,    85,   160,   103,   161,   162,   226,
     238,   316,   113,   132,   133,     4,     5,   267,   215,   216,
     217,   218,   210,   111,   239,   250,   113,   251,   129,   130,
       6,   248,    86,    81,   262,   286,   263,   263,   104,   291,
     240,   113,   317,   105,   302,    19,   113,   323,    22,   302,
     318,   321,    25,   254,   114,   117,   118,    26,   168,     1,
     119,     2,    85,   120,   171,   158,   176,    27,   169,   166,
      28,    29,   310,     4,     5,    30,    31,    83,    32,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   279,
      86,   280,   173,   265,    77,   182,   268,   269,   189,   209,
     213,   227,   266,   236,   289,   237,    22,   271,   271,   241,
      25,   245,   242,   243,   249,    26,   260,   244,   264,   255,
      87,   274,   149,    82,   283,    27,   290,   292,    28,    29,
     307,   305,   309,    30,    31,   313,    32,   110,   322,    33,
     259,   288,   261,    97,   197,   199,   198,   319,   308,   202,
     203,   195,     1,   253,     2,     3,   204,   179,   200,   311,
     101,   225,   201,   315,   271,     0,     4,     5,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     325,     6,     0,     7,     8,     9,     0,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,     0,    25,     0,     0,     0,     0,    26,     1,
       0,     2,     3,     0,     0,     0,     0,     0,    27,     0,
       0,    28,    29,     4,     5,     0,    30,    31,     0,    32,
       0,    33,   109,     0,     0,     0,     0,     0,     6,     0,
       7,     8,     9,     0,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,     0,
      25,     0,     0,     0,     0,    26,     1,     0,     2,     3,
       0,     0,     0,     0,     0,    27,     0,     0,    28,    29,
       4,     5,     0,    30,    31,     0,    32,     0,    33,   193,
       0,     0,     0,     0,     0,     6,     0,     7,     8,     9,
       0,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,     0,    25,     0,     0,
       0,     0,    26,     1,     0,     2,     3,   -32,     0,     0,
       0,     0,    27,     0,     0,    28,    29,     4,     5,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
       0,     0,     6,     0,    86,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,     1,    25,     2,    85,     0,     0,    26,
       0,     0,     0,     0,     0,     0,     0,     4,     5,    27,
       0,     0,    28,    29,     0,     0,     0,    30,    31,     0,
      32,     0,    33,     0,    86,     0,     0,     1,     0,     2,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      22,     4,     5,     0,    25,     0,     0,     0,     0,    26,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    27,
       0,     0,    28,    29,     0,     0,     0,    30,    31,     0,
      32,     0,     0,     0,    22,     0,     0,     0,    25,     0,
       0,     0,     0,    26,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,    28,    29,     0,     0,
       0,    30,    31,     0,    32
};

static const yytype_int16 yycheck[] =
{
      12,     6,    32,    93,     8,   166,     3,    16,     6,   189,
       9,    30,   243,    35,    19,    17,    18,   262,    62,    14,
      15,     6,    26,     3,    34,     5,     6,    59,    62,    13,
       6,    63,    16,    62,    62,    62,     6,    17,    18,    83,
      64,   286,    62,    66,    30,    64,    62,    56,    33,    83,
      82,    62,    81,    59,    34,    83,    83,    59,    59,    82,
      71,    80,    62,    83,    76,    81,    65,   247,   299,    59,
      50,    71,    70,    83,    54,    70,    71,    79,    80,    59,
     241,    64,   283,   113,    81,    89,   116,    99,   110,    69,
      94,    95,    72,    73,    80,    82,   297,    77,    78,     6,
      80,    64,    82,    74,    75,    76,   115,    70,   117,   118,
     119,   120,    59,   100,   123,   102,   103,   104,    66,   128,
     150,    62,   131,    64,   154,   134,    60,     6,    62,   138,
      60,     3,    62,     5,     6,     3,     6,     5,     6,   169,
      60,   302,    62,    72,    73,    17,    18,   237,    55,    56,
      57,    58,   156,     0,    60,    60,    62,    62,    11,    12,
      32,   191,    34,   168,    60,    60,    62,    62,     6,    60,
     182,    62,    60,     6,    62,    47,    62,    60,    50,    62,
     306,   307,    54,   213,    64,    10,    67,    59,    62,     3,
      68,     5,     6,    69,     6,    64,    42,    69,    63,    66,
      72,    73,   292,    17,    18,    77,    78,    70,    80,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,   249,
      34,   251,    59,   235,    82,    66,   238,   239,    82,     6,
      66,    59,   236,    59,   264,    64,    50,   242,   243,    63,
      54,    34,    82,    82,    66,    59,     6,    82,    63,    70,
      64,     6,    63,     6,    59,    69,    37,    64,    72,    73,
      31,    81,    64,    77,    78,     3,    80,    33,    60,    82,
     227,   263,   230,    16,   117,   119,   118,   307,   290,   128,
     131,   115,     3,   211,     5,     6,   134,    93,   120,   294,
      19,   168,   123,   300,   299,    -1,    17,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    32,    -1,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    54,    -1,    -1,    -1,    -1,    59,     3,
      -1,     5,     6,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    17,    18,    -1,    77,    78,    -1,    80,
      -1,    82,    83,    -1,    -1,    -1,    -1,    -1,    32,    -1,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    59,     3,    -1,     5,     6,
      -1,    -1,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      17,    18,    -1,    77,    78,    -1,    80,    -1,    82,    83,
      -1,    -1,    -1,    -1,    -1,    32,    -1,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    59,     3,    -1,     5,     6,    64,    -1,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    17,    18,    -1,
      77,    78,    -1,    80,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    32,    -1,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,     3,    54,     5,     6,    -1,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,    18,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    77,    78,    -1,
      80,    -1,    82,    -1,    34,    -1,    -1,     3,    -1,     5,
       6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    17,    18,    -1,    54,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    77,    78,    -1,
      80,    -1,    -1,    -1,    50,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    77,    78,    -1,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,     6,    17,    18,    32,    34,    35,    36,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    54,    59,    69,    72,    73,
      77,    78,    80,    82,    85,    86,    87,    88,    92,    93,
      94,    95,    96,    98,   101,   102,   103,   104,   105,   106,
     107,   109,   111,   113,   115,   117,   118,   119,   122,   123,
     124,   126,   130,   131,   132,   133,   134,   135,   136,   137,
     139,   154,   155,   156,   158,   161,    66,    82,     6,   140,
     141,   142,     6,    70,   152,     6,    34,    64,    94,    59,
      64,    64,    92,    59,    59,    59,   100,   101,   117,    66,
       6,   140,     6,     6,     6,     6,    94,    96,   125,    83,
      86,     0,    87,    62,    64,     9,    65,    10,    67,    68,
      69,    13,    16,   108,    14,    15,    70,    71,   110,    11,
      12,   112,    72,    73,   114,    74,    75,    76,   116,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    63,
      97,   117,    17,    18,    59,    79,    80,   121,    64,    92,
       3,     5,     6,   127,   128,   129,    66,   143,    62,    63,
     152,     6,   153,    59,    64,    94,    42,    95,   138,   139,
      94,    94,    66,    92,   152,    64,   152,   152,   152,    82,
      60,    62,    81,    83,    96,   103,    96,   104,   105,   106,
     107,   109,   111,   113,   115,   117,    96,    96,   120,     6,
      94,    62,    83,    66,     6,    55,    56,    57,    58,   144,
     145,   146,   147,   148,   150,   141,    96,    59,    62,    71,
      33,    89,    90,    91,   142,    60,    59,    64,    60,    60,
      92,    63,    82,    82,    82,    34,    88,   162,    96,    66,
      60,    62,    81,   128,    96,    70,   151,    30,    80,    89,
       6,    91,    60,    62,    63,    92,    94,    95,    92,    92,
     144,   142,   157,   157,     6,   159,   160,    83,    88,    96,
      96,   144,   149,    59,     3,    81,    60,   143,    90,    96,
      37,    60,    64,    64,    62,    83,    83,    59,    63,    82,
      62,    83,    62,    71,   149,    81,   143,    31,    92,    64,
      95,   142,   149,     3,   157,   160,   144,    60,   130,    96,
      99,   130,    60,    60,    83,    92
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    84,    85,    85,    86,    86,    87,    87,    88,    89,
      89,    89,    90,    90,    91,    91,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    93,    94,
      94,    95,    95,    96,    96,    96,    97,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    98,    99,    99,
     100,   101,   101,   102,   102,   103,   103,   104,   104,   105,
     105,   106,   106,   107,   107,   108,   108,   109,   109,   110,
     110,   110,   110,   111,   111,   112,   112,   113,   113,   114,
     114,   115,   115,   116,   116,   116,   117,   117,   118,   118,
     118,   118,   118,   118,   118,   119,   119,   119,   119,   119,
     120,   120,   120,   121,   121,   122,   122,   122,   122,   122,
     123,   123,   123,   123,   124,   125,   125,   125,   125,   126,
     127,   127,   127,   127,   128,   129,   129,   129,   130,   130,
     131,   131,   131,   132,   132,   133,   134,   135,   135,   135,
     136,   136,   136,   137,   137,   137,   138,   138,   139,   139,
     140,   140,   141,   141,   142,   143,   143,   144,   144,   144,
     144,   144,   145,   145,   145,   145,   146,   147,   148,   149,
     149,   149,   150,   150,   151,   152,   152,   153,   153,   154,
     155,   155,   156,   157,   157,   157,   157,   158,   159,   159,
     159,   160,   160,   160,   160,   161,   162,   162
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
#line 1939 "bison.tab.c"
    break;

  case 3: /* program: %empty  */
#line 69 "./src/parser/gens/bison.y"
                  {
        ptree_g = ptree_create(PTREE_EOF, 0);
    }
#line 1947 "bison.tab.c"
    break;

  case 4: /* source_elements: source_elements source_element  */
#line 75 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 1955 "bison.tab.c"
    break;

  case 5: /* source_elements: source_element  */
#line 78 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_SOURCE_ELEMENTS, 1, (yyvsp[0].ptree));
    }
#line 1963 "bison.tab.c"
    break;

  case 6: /* source_element: function_declaration  */
#line 84 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 1971 "bison.tab.c"
    break;

  case 7: /* source_element: statement  */
#line 87 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 1979 "bison.tab.c"
    break;

  case 8: /* function_declaration: FUN WORD typed_parameters '(' parameters_list ')' type_annotation block_statement  */
#line 93 "./src/parser/gens/bison.y"
                                                                                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-6].str));
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_DECLARTION, 7, w, (yyvsp[-5].ptree), lp, (yyvsp[-3].ptree), rp, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 1990 "bison.tab.c"
    break;

  case 9: /* parameters_list: parameter_is_mut  */
#line 102 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_LIST, 1, (yyvsp[0].ptree));
    }
#line 1998 "bison.tab.c"
    break;

  case 10: /* parameters_list: parameters_list ',' parameter_is_mut  */
#line 105 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2006 "bison.tab.c"
    break;

  case 11: /* parameters_list: %empty  */
#line 108 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 2014 "bison.tab.c"
    break;

  case 12: /* parameter_is_mut: parameter  */
#line 114 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_CONST, 1, (yyvsp[0].ptree));
    }
#line 2022 "bison.tab.c"
    break;

  case 13: /* parameter_is_mut: MUT parameter  */
#line 117 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_MUT, 1, (yyvsp[0].ptree));
    }
#line 2030 "bison.tab.c"
    break;

  case 14: /* parameter: identifier  */
#line 123 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 2038 "bison.tab.c"
    break;

  case 15: /* parameter: identifier '=' assignment_expression  */
#line 126 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_PARAMETER_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 2047 "bison.tab.c"
    break;

  case 16: /* statement: expression_statement  */
#line 133 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2055 "bison.tab.c"
    break;

  case 17: /* statement: block_statement  */
#line 136 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2063 "bison.tab.c"
    break;

  case 18: /* statement: jump_statement  */
#line 139 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2071 "bison.tab.c"
    break;

  case 19: /* statement: labeled_statement  */
#line 142 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2079 "bison.tab.c"
    break;

  case 20: /* statement: selection_statement  */
#line 145 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2087 "bison.tab.c"
    break;

  case 21: /* statement: iteration_statement  */
#line 148 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2095 "bison.tab.c"
    break;

  case 22: /* statement: variable_statement ';'  */
#line 151 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2103 "bison.tab.c"
    break;

  case 23: /* statement: type_statement  */
#line 154 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2111 "bison.tab.c"
    break;

  case 24: /* statement: struct_statement  */
#line 157 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2119 "bison.tab.c"
    break;

  case 25: /* statement: union_statement  */
#line 160 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2127 "bison.tab.c"
    break;

  case 26: /* statement: enum_statement  */
#line 163 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2135 "bison.tab.c"
    break;

  case 27: /* statement: methods_statement  */
#line 166 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2143 "bison.tab.c"
    break;

  case 28: /* expression_statement: optional_expression ';'  */
#line 173 "./src/parser/gens/bison.y"
                            {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION_STATEMENT, 2, (yyvsp[-1].ptree), sc);
    }
#line 2152 "bison.tab.c"
    break;

  case 29: /* expression: assignment_expression  */
#line 180 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2160 "bison.tab.c"
    break;

  case 30: /* expression: expression ',' assignment_expression  */
#line 183 "./src/parser/gens/bison.y"
                                           {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 2, (yyvsp[0].ptree));
    }
#line 2168 "bison.tab.c"
    break;

  case 31: /* optional_expression: expression  */
#line 189 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2176 "bison.tab.c"
    break;

  case 32: /* optional_expression: %empty  */
#line 192 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2184 "bison.tab.c"
    break;

  case 33: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 198 "./src/parser/gens/bison.y"
                                                               {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2192 "bison.tab.c"
    break;

  case 34: /* assignment_expression: function_expression  */
#line 201 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2200 "bison.tab.c"
    break;

  case 35: /* assignment_expression: conditional_expression  */
#line 204 "./src/parser/gens/bison.y"
                             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2208 "bison.tab.c"
    break;

  case 36: /* assignment_operator: '='  */
#line 210 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_ASSIGN, 0);
    }
#line 2216 "bison.tab.c"
    break;

  case 37: /* assignment_operator: OR_ASSIGN  */
#line 213 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_OR_ASSIGN, 0);
    }
#line 2224 "bison.tab.c"
    break;

  case 38: /* assignment_operator: MUL_ASSIGN  */
#line 216 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MUL_ASSIGN, 0);
    }
#line 2232 "bison.tab.c"
    break;

  case 39: /* assignment_operator: DIV_ASSIGN  */
#line 219 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_DIV_ASSIGN, 0);
    }
#line 2240 "bison.tab.c"
    break;

  case 40: /* assignment_operator: MOD_ASSIGN  */
#line 222 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_MOD_ASSIGN, 0);
    }
#line 2248 "bison.tab.c"
    break;

  case 41: /* assignment_operator: ADD_ASSIGN  */
#line 225 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_ADD_ASSIGN, 0);
    }
#line 2256 "bison.tab.c"
    break;

  case 42: /* assignment_operator: SUB_ASSIGN  */
#line 228 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SUB_ASSIGN, 0);
    }
#line 2264 "bison.tab.c"
    break;

  case 43: /* assignment_operator: SHL_ASSIGN  */
#line 231 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHL_ASSIGN, 0);
    }
#line 2272 "bison.tab.c"
    break;

  case 44: /* assignment_operator: SHR_ASSIGN  */
#line 234 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_SHR_ASSIGN, 0);
    }
#line 2280 "bison.tab.c"
    break;

  case 45: /* assignment_operator: AND_ASSIGN  */
#line 237 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_AND_ASSIGN, 0);
    }
#line 2288 "bison.tab.c"
    break;

  case 46: /* assignment_operator: XOR_ASSIGN  */
#line 240 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_XOR_ASSIGN, 0);
    }
#line 2296 "bison.tab.c"
    break;

  case 47: /* function_expression: FUN typed_parameters '(' parameters_list ')' type_annotation FUN_ARROW function_expression_body  */
#line 246 "./src/parser/gens/bison.y"
                                                                                                    {
        ptree_t *lp = ptree_create(PTREE_LPRN, 0);
        ptree_t *rp = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_EXPRESSION, 6, (yyvsp[-6].ptree), lp, (yyvsp[-4].ptree), rp, (yyvsp[-2].ptree), (yyvsp[0].ptree));
    }
#line 2306 "bison.tab.c"
    break;

  case 48: /* function_expression_body: assignment_expression  */
#line 254 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2314 "bison.tab.c"
    break;

  case 49: /* function_expression_body: block_statement  */
#line 257 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2322 "bison.tab.c"
    break;

  case 50: /* constant_expression: conditional_expression  */
#line 263 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2330 "bison.tab.c"
    break;

  case 51: /* conditional_expression: logical_or_expression  */
#line 269 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2338 "bison.tab.c"
    break;

  case 52: /* conditional_expression: logical_or_expression '?' assignment_expression ':' assignment_expression  */
#line 272 "./src/parser/gens/bison.y"
                                                                                {
        ptree_t *q = ptree_create(PTREE_QUESTION, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TERTIARY_EXPRESSION, 5, (yyvsp[-4].ptree), q, (yyvsp[-2].ptree), sc, (yyvsp[0].ptree));
    }
#line 2348 "bison.tab.c"
    break;

  case 53: /* logical_or_expression: logical_and_expression  */
#line 280 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2356 "bison.tab.c"
    break;

  case 54: /* logical_or_expression: logical_or_expression OR logical_and_expression  */
#line 283 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *op = ptree_create(PTREE_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2365 "bison.tab.c"
    break;

  case 55: /* logical_and_expression: bitwise_or_expression  */
#line 290 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2373 "bison.tab.c"
    break;

  case 56: /* logical_and_expression: logical_and_expression AND bitwise_or_expression  */
#line 293 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2382 "bison.tab.c"
    break;

  case 57: /* bitwise_or_expression: bitwise_xor_expression  */
#line 300 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2390 "bison.tab.c"
    break;

  case 58: /* bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression  */
#line 303 "./src/parser/gens/bison.y"
                                                       {
        ptree_t *op = ptree_create(PTREE_BIT_OR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2399 "bison.tab.c"
    break;

  case 59: /* bitwise_xor_expression: bitwise_and_expression  */
#line 310 "./src/parser/gens/bison.y"
                           {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2407 "bison.tab.c"
    break;

  case 60: /* bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression  */
#line 313 "./src/parser/gens/bison.y"
                                                        {
        ptree_t *op = ptree_create(PTREE_BIT_XOR, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2416 "bison.tab.c"
    break;

  case 61: /* bitwise_and_expression: equality_expression  */
#line 320 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2424 "bison.tab.c"
    break;

  case 62: /* bitwise_and_expression: bitwise_and_expression '&' equality_expression  */
#line 323 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *op = ptree_create(PTREE_BIT_AND, 0);
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), op, (yyvsp[0].ptree));
    }
#line 2433 "bison.tab.c"
    break;

  case 63: /* equality_expression: relational_expression  */
#line 330 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2441 "bison.tab.c"
    break;

  case 64: /* equality_expression: equality_expression equality_operator relational_expression  */
#line 333 "./src/parser/gens/bison.y"
                                                                  {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2449 "bison.tab.c"
    break;

  case 65: /* equality_operator: IS_EQUAL  */
#line 339 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_IS_EQUAL, 0);
    }
#line 2457 "bison.tab.c"
    break;

  case 66: /* equality_operator: NOT_EQUAL  */
#line 342 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_NOT_EQUAL, 0);
    }
#line 2465 "bison.tab.c"
    break;

  case 67: /* relational_expression: shift_expression  */
#line 348 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2473 "bison.tab.c"
    break;

  case 68: /* relational_expression: relational_expression relational_operator shift_expression  */
#line 351 "./src/parser/gens/bison.y"
                                                                 {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2481 "bison.tab.c"
    break;

  case 69: /* relational_operator: '<'  */
#line 357 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_LESS, 0);
    }
#line 2489 "bison.tab.c"
    break;

  case 70: /* relational_operator: '>'  */
#line 360 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_GREATER, 0);
    }
#line 2497 "bison.tab.c"
    break;

  case 71: /* relational_operator: LESS_EQ  */
#line 363 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_LESS_EQ, 0);
    }
#line 2505 "bison.tab.c"
    break;

  case 72: /* relational_operator: GREATER_EQ  */
#line 366 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_GREATER_EQ, 0);
    }
#line 2513 "bison.tab.c"
    break;

  case 73: /* shift_expression: additive_expression  */
#line 372 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2521 "bison.tab.c"
    break;

  case 74: /* shift_expression: shift_expression shift_operator additive_expression  */
#line 375 "./src/parser/gens/bison.y"
                                                          {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2529 "bison.tab.c"
    break;

  case 75: /* shift_operator: LSHIFT  */
#line 381 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create(PTREE_LSHIFT, 0);
    }
#line 2537 "bison.tab.c"
    break;

  case 76: /* shift_operator: RSHIFT  */
#line 384 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_RSHIFT, 0);
    }
#line 2545 "bison.tab.c"
    break;

  case 77: /* additive_expression: multiplicative_expression  */
#line 390 "./src/parser/gens/bison.y"
                              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2553 "bison.tab.c"
    break;

  case 78: /* additive_expression: additive_expression additive_operator multiplicative_expression  */
#line 393 "./src/parser/gens/bison.y"
                                                                      {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2561 "bison.tab.c"
    break;

  case 79: /* additive_operator: '+'  */
#line 399 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2569 "bison.tab.c"
    break;

  case 80: /* additive_operator: '-'  */
#line 402 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2577 "bison.tab.c"
    break;

  case 81: /* multiplicative_expression: unary_expression  */
#line 408 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2585 "bison.tab.c"
    break;

  case 82: /* multiplicative_expression: multiplicative_expression multiplicative_operator unary_expression  */
#line 411 "./src/parser/gens/bison.y"
                                                                         {
        (yyval.ptree) = ptree_create(PTREE_BINARY_EXPRESSION, 3, (yyvsp[-2].ptree), (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2593 "bison.tab.c"
    break;

  case 83: /* multiplicative_operator: '*'  */
#line 417 "./src/parser/gens/bison.y"
        {
        (yyval.ptree) = ptree_create(PTREE_MULTIPLY, 0);
    }
#line 2601 "bison.tab.c"
    break;

  case 84: /* multiplicative_operator: '/'  */
#line 420 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_DIVIDE, 0);
    }
#line 2609 "bison.tab.c"
    break;

  case 85: /* multiplicative_operator: '%'  */
#line 423 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MODULO, 0);
    }
#line 2617 "bison.tab.c"
    break;

  case 86: /* unary_expression: postfix_expression  */
#line 429 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2625 "bison.tab.c"
    break;

  case 87: /* unary_expression: unary_operator unary_expression  */
#line 432 "./src/parser/gens/bison.y"
                                      {
        (yyval.ptree) = ptree_create(PTREE_UNARY, 2, (yyvsp[-1].ptree), (yyvsp[0].ptree));
    }
#line 2633 "bison.tab.c"
    break;

  case 88: /* unary_operator: INCREMENT  */
#line 438 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2641 "bison.tab.c"
    break;

  case 89: /* unary_operator: DECREMENT  */
#line 441 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2649 "bison.tab.c"
    break;

  case 90: /* unary_operator: '!'  */
#line 444 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_NOT, 0);
    }
#line 2657 "bison.tab.c"
    break;

  case 91: /* unary_operator: '~'  */
#line 447 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_TILDE, 0);
    }
#line 2665 "bison.tab.c"
    break;

  case 92: /* unary_operator: '+'  */
#line 450 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_PLUS, 0);
    }
#line 2673 "bison.tab.c"
    break;

  case 93: /* unary_operator: '-'  */
#line 453 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_MINUS, 0);
    }
#line 2681 "bison.tab.c"
    break;

  case 94: /* unary_operator: '&'  */
#line 456 "./src/parser/gens/bison.y"
          {
        (yyval.ptree) = ptree_create(PTREE_BIT_AND, 0);
    }
#line 2689 "bison.tab.c"
    break;

  case 95: /* postfix_expression: primary_expression  */
#line 462 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2697 "bison.tab.c"
    break;

  case 96: /* postfix_expression: postfix_expression postfix_operator  */
#line 465 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_create(PTREE_POSTFIX, 2, (yyvsp[0].ptree), (yyvsp[-1].ptree));
    }
#line 2705 "bison.tab.c"
    break;

  case 97: /* postfix_expression: postfix_expression '.' WORD  */
#line 468 "./src/parser/gens/bison.y"
                                  {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        ptree_t *dot =ptree_create(PTREE_DOT, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_ACCESS, 3, (yyvsp[-2].ptree), dot, w);
    }
#line 2715 "bison.tab.c"
    break;

  case 98: /* postfix_expression: postfix_expression '[' expression ']'  */
#line 473 "./src/parser/gens/bison.y"
                                            {
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_ACCESS, 3, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2725 "bison.tab.c"
    break;

  case 99: /* postfix_expression: postfix_expression '(' arguments ')'  */
#line 478 "./src/parser/gens/bison.y"
                                           {
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_CALL, 4, (yyvsp[-3].ptree), lb, (yyvsp[-1].ptree), rb);
    }
#line 2735 "bison.tab.c"
    break;

  case 100: /* arguments: assignment_expression  */
#line 486 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ARGUMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2743 "bison.tab.c"
    break;

  case 101: /* arguments: arguments ',' assignment_expression  */
#line 489 "./src/parser/gens/bison.y"
                                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2751 "bison.tab.c"
    break;

  case 102: /* arguments: %empty  */
#line 492 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2759 "bison.tab.c"
    break;

  case 103: /* postfix_operator: INCREMENT  */
#line 498 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_INCREMENT, 0);
    }
#line 2767 "bison.tab.c"
    break;

  case 104: /* postfix_operator: DECREMENT  */
#line 501 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_DECREMENT, 0);
    }
#line 2775 "bison.tab.c"
    break;

  case 105: /* primary_expression: WORD  */
#line 507 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 2783 "bison.tab.c"
    break;

  case 106: /* primary_expression: literal  */
#line 510 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2791 "bison.tab.c"
    break;

  case 107: /* primary_expression: array_literal  */
#line 513 "./src/parser/gens/bison.y"
                    {
       (yyval.ptree) = (yyvsp[0].ptree); 
    }
#line 2799 "bison.tab.c"
    break;

  case 108: /* primary_expression: object_literal  */
#line 516 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2807 "bison.tab.c"
    break;

  case 109: /* primary_expression: '(' expression ')'  */
#line 519 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_EXPRESSION, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2817 "bison.tab.c"
    break;

  case 110: /* literal: NUMBER  */
#line 527 "./src/parser/gens/bison.y"
           {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2825 "bison.tab.c"
    break;

  case 111: /* literal: TRUE_VAL  */
#line 530 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create_bool(1);
    }
#line 2833 "bison.tab.c"
    break;

  case 112: /* literal: FALSE_VAL  */
#line 533 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create_bool(0);
    }
#line 2841 "bison.tab.c"
    break;

  case 113: /* literal: STRING  */
#line 536 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2849 "bison.tab.c"
    break;

  case 114: /* array_literal: '[' element_list ']'  */
#line 542 "./src/parser/gens/bison.y"
                         {
        ptree_t *left = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *right = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_LITERAL, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2859 "bison.tab.c"
    break;

  case 115: /* element_list: assignment_expression  */
#line 550 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_create(PTREE_ELEMENT_LIST, 1, (yyvsp[0].ptree));
    }
#line 2867 "bison.tab.c"
    break;

  case 116: /* element_list: element_list ',' assignment_expression  */
#line 553 "./src/parser/gens/bison.y"
                                             {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2875 "bison.tab.c"
    break;

  case 117: /* element_list: element_list ','  */
#line 556 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2883 "bison.tab.c"
    break;

  case 118: /* element_list: %empty  */
#line 559 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2891 "bison.tab.c"
    break;

  case 119: /* object_literal: WORD '{' keyvalue_list '}'  */
#line 565 "./src/parser/gens/bison.y"
                               {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_OBJECT_LITERAL, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 2902 "bison.tab.c"
    break;

  case 120: /* keyvalue_list: keyvalue  */
#line 574 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2910 "bison.tab.c"
    break;

  case 121: /* keyvalue_list: keyvalue_list ',' keyvalue  */
#line 577 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 2918 "bison.tab.c"
    break;

  case 122: /* keyvalue_list: keyvalue_list ','  */
#line 580 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 2926 "bison.tab.c"
    break;

  case 123: /* keyvalue_list: %empty  */
#line 583 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);
    }
#line 2934 "bison.tab.c"
    break;

  case 124: /* keyvalue: property_name ':' assignment_expression  */
#line 589 "./src/parser/gens/bison.y"
                                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_KEYVALUE, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 2943 "bison.tab.c"
    break;

  case 125: /* property_name: WORD  */
#line 596 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 2951 "bison.tab.c"
    break;

  case 126: /* property_name: STRING  */
#line 599 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_str((yyvsp[0].str));
    }
#line 2959 "bison.tab.c"
    break;

  case 127: /* property_name: NUMBER  */
#line 602 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create_num((yyvsp[0].num));
    }
#line 2967 "bison.tab.c"
    break;

  case 128: /* block_statement: '{' source_elements '}'  */
#line 608 "./src/parser/gens/bison.y"
                            {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, (yyvsp[-1].ptree), right);
    }
#line 2978 "bison.tab.c"
    break;

  case 129: /* block_statement: '{' '}'  */
#line 614 "./src/parser/gens/bison.y"
              {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_BLOCK, 3, left, empty, right);
    }
#line 2989 "bison.tab.c"
    break;

  case 130: /* jump_statement: return_statement  */
#line 623 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 2997 "bison.tab.c"
    break;

  case 131: /* jump_statement: break_statement  */
#line 626 "./src/parser/gens/bison.y"
                      {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3005 "bison.tab.c"
    break;

  case 132: /* jump_statement: continue_statement  */
#line 629 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3013 "bison.tab.c"
    break;

  case 133: /* return_statement: RETURN ';'  */
#line 634 "./src/parser/gens/bison.y"
               {
        ptree_t *empty = ptree_create(PTREE_EMPTY, 0);
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, empty);
    }
#line 3022 "bison.tab.c"
    break;

  case 134: /* return_statement: RETURN expression ';'  */
#line 638 "./src/parser/gens/bison.y"
                            {
        (yyval.ptree) = ptree_create(PTREE_RETURN, 1, (yyvsp[-1].ptree));
    }
#line 3030 "bison.tab.c"
    break;

  case 135: /* break_statement: BREAK ';'  */
#line 644 "./src/parser/gens/bison.y"
              {
        (yyval.ptree) = ptree_create(PTREE_BREAK, 0);
    }
#line 3038 "bison.tab.c"
    break;

  case 136: /* continue_statement: CONTINUE ';'  */
#line 650 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = ptree_create(PTREE_CONTINUE, 0);
    }
#line 3046 "bison.tab.c"
    break;

  case 137: /* labeled_statement: WORD ':' statement  */
#line 656 "./src/parser/gens/bison.y"
                       {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LABEL, 3, w, c, (yyvsp[0].ptree));
    }
#line 3056 "bison.tab.c"
    break;

  case 138: /* labeled_statement: CASE constant_expression ':' statement  */
#line 661 "./src/parser/gens/bison.y"
                                             {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CASE_LABEL, 3, (yyvsp[-2].ptree), c, (yyvsp[0].ptree));
    }
#line 3065 "bison.tab.c"
    break;

  case 139: /* labeled_statement: DEFAULT ':' statement  */
#line 665 "./src/parser/gens/bison.y"
                            {
        ptree_t *c = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_DEFAULT_LABEL, 2, c, (yyvsp[0].ptree));
    }
#line 3074 "bison.tab.c"
    break;

  case 140: /* selection_statement: IF '(' expression ')' statement  */
#line 673 "./src/parser/gens/bison.y"
    {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3085 "bison.tab.c"
    break;

  case 141: /* selection_statement: IF '(' expression ')' statement ELSE statement  */
#line 679 "./src/parser/gens/bison.y"
                                                     {
        ptree_t *e = ptree_create(PTREE_ELSE, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_IF_ELSE, 6, left, (yyvsp[-4].ptree), right, (yyvsp[-2].ptree), e, (yyvsp[0].ptree));
    }
#line 3097 "bison.tab.c"
    break;

  case 142: /* selection_statement: SWITCH '(' expression ')' statement  */
#line 686 "./src/parser/gens/bison.y"
                                          {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);

        (yyval.ptree) = ptree_create(PTREE_SWITCH, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3108 "bison.tab.c"
    break;

  case 143: /* iteration_statement: WHILE '(' expression ')' statement  */
#line 695 "./src/parser/gens/bison.y"
                                       {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_WHILE, 4, left, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3118 "bison.tab.c"
    break;

  case 144: /* iteration_statement: DO statement WHILE '(' expression ')' ';'  */
#line 700 "./src/parser/gens/bison.y"
                                                {
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *wh = ptree_create(PTREE_WHILE, 0);

        (yyval.ptree) = ptree_create(PTREE_DO, 6, (yyvsp[-5].ptree), wh, left, (yyvsp[-2].ptree), right, sc);
    }
#line 3131 "bison.tab.c"
    break;

  case 145: /* iteration_statement: FOR '(' for_initialization ';' optional_expression ';' optional_expression ')' statement  */
#line 708 "./src/parser/gens/bison.y"
                                                                                               {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *sc2 = ptree_create(PTREE_SEMICOLON, 0);
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_FOR, 8, left, (yyvsp[-6].ptree), sc, (yyvsp[-4].ptree), sc2, (yyvsp[-2].ptree), right, (yyvsp[0].ptree));
    }
#line 3143 "bison.tab.c"
    break;

  case 146: /* for_initialization: optional_expression  */
#line 718 "./src/parser/gens/bison.y"
                        {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3151 "bison.tab.c"
    break;

  case 147: /* for_initialization: variable_statement  */
#line 721 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3159 "bison.tab.c"
    break;

  case 148: /* variable_statement: LET variable_list  */
#line 727 "./src/parser/gens/bison.y"
                      {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_LET_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3168 "bison.tab.c"
    break;

  case 149: /* variable_statement: CONST variable_list  */
#line 731 "./src/parser/gens/bison.y"
                          {
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_CONST_STATEMENT, 2, (yyvsp[0].ptree), sc);
    }
#line 3177 "bison.tab.c"
    break;

  case 150: /* variable_list: variable  */
#line 738 "./src/parser/gens/bison.y"
             {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3185 "bison.tab.c"
    break;

  case 151: /* variable_list: variable_list ',' variable  */
#line 741 "./src/parser/gens/bison.y"
                                 {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3193 "bison.tab.c"
    break;

  case 152: /* variable: identifier  */
#line 747 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_UNASSIGNED, 1, (yyvsp[0].ptree));
    }
#line 3201 "bison.tab.c"
    break;

  case 153: /* variable: identifier '=' assignment_expression  */
#line 750 "./src/parser/gens/bison.y"
                                           {
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_VARIABLE_ASSIGNED, 3, (yyvsp[-2].ptree), eq, (yyvsp[0].ptree));
    }
#line 3210 "bison.tab.c"
    break;

  case 154: /* identifier: WORD type_annotation  */
#line 757 "./src/parser/gens/bison.y"
                         {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_IDENTIFIER, 2, w, (yyvsp[0].ptree));
    }
#line 3219 "bison.tab.c"
    break;

  case 155: /* type_annotation: ':' type  */
#line 764 "./src/parser/gens/bison.y"
             {
        ptree_t *sc = ptree_create(PTREE_COLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ANNOTATION, 2, sc, (yyvsp[0].ptree)); 
    }
#line 3228 "bison.tab.c"
    break;

  case 156: /* type_annotation: %empty  */
#line 768 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3236 "bison.tab.c"
    break;

  case 157: /* type: primitive_type  */
#line 774 "./src/parser/gens/bison.y"
                   {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3244 "bison.tab.c"
    break;

  case 158: /* type: array_type  */
#line 777 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3252 "bison.tab.c"
    break;

  case 159: /* type: tuple_type  */
#line 780 "./src/parser/gens/bison.y"
                 {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3260 "bison.tab.c"
    break;

  case 160: /* type: function_type  */
#line 783 "./src/parser/gens/bison.y"
                    {
        (yyval.ptree) = (yyvsp[0].ptree);        
    }
#line 3268 "bison.tab.c"
    break;

  case 161: /* type: type_reference  */
#line 786 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[0].ptree);
    }
#line 3276 "bison.tab.c"
    break;

  case 162: /* primitive_type: T_VOID  */
#line 792 "./src/parser/gens/bison.y"
           { 
        (yyval.ptree) = ptree_create(PTREE_VOID_TYPE, 0);
    }
#line 3284 "bison.tab.c"
    break;

  case 163: /* primitive_type: T_INTEGER  */
#line 795 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_INTEGER_TYPE, 0);
    }
#line 3292 "bison.tab.c"
    break;

  case 164: /* primitive_type: T_STRING  */
#line 798 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_STRING_TYPE, 0);
    }
#line 3300 "bison.tab.c"
    break;

  case 165: /* primitive_type: T_BOOLEAN  */
#line 801 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_BOOLEAN_TYPE, 0);
    }
#line 3308 "bison.tab.c"
    break;

  case 166: /* array_type: type '[' ']'  */
#line 807 "./src/parser/gens/bison.y"
                 {
        ptree_t *lb = ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb = ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_ARRAY_TYPE, 3, (yyvsp[-2].ptree), lb, rb);
    }
#line 3318 "bison.tab.c"
    break;

  case 167: /* tuple_type: type '[' NUMBER ']'  */
#line 815 "./src/parser/gens/bison.y"
                        {
        ptree_t *num = ptree_create_num((yyvsp[-1].num));
        ptree_t *lb =ptree_create(PTREE_LBRACKET, 0);
        ptree_t *rb =ptree_create(PTREE_RBRACKET, 0);
        (yyval.ptree) = ptree_create(PTREE_TUPLE_TYPE, 4, (yyvsp[-3].ptree), lb, num, rb);
    }
#line 3329 "bison.tab.c"
    break;

  case 168: /* function_type: type BACKWARD_ARROW '(' types_list ')'  */
#line 824 "./src/parser/gens/bison.y"
                                           {
        ptree_t *a = ptree_create(PTREE_BACKWARD_ARROW, 0);
        ptree_t *lb = ptree_create(PTREE_LPRN, 0);
        ptree_t *rb = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_FUNCTION_TYPE, 5, (yyvsp[-4].ptree), a, lb, (yyvsp[-1].ptree), rb);
    }
#line 3340 "bison.tab.c"
    break;

  case 169: /* types_list: type  */
#line 833 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create(PTREE_TYPE_LIST, 1, (yyvsp[0].ptree));
    }
#line 3348 "bison.tab.c"
    break;

  case 170: /* types_list: types_list ',' type  */
#line 836 "./src/parser/gens/bison.y"
                          {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3356 "bison.tab.c"
    break;

  case 171: /* types_list: %empty  */
#line 839 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0); 
    }
#line 3364 "bison.tab.c"
    break;

  case 172: /* type_reference: WORD  */
#line 845 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 1, w);
    }
#line 3373 "bison.tab.c"
    break;

  case 173: /* type_reference: WORD typed_arguments  */
#line 849 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_REFERENCE, 2, w, (yyvsp[0].ptree));
    }
#line 3382 "bison.tab.c"
    break;

  case 174: /* typed_arguments: '<' types_list '>'  */
#line 856 "./src/parser/gens/bison.y"
                       {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_ARGUMENTS, 3, l, (yyvsp[-1].ptree), g);
    }
#line 3392 "bison.tab.c"
    break;

  case 175: /* typed_parameters: '<' type_params '>'  */
#line 864 "./src/parser/gens/bison.y"
                        {
        ptree_t *l = ptree_create(PTREE_LESS, 0);
        ptree_t *g = ptree_create(PTREE_GREATER, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPED_PARAMETERS, 3, l, (yyvsp[-1].ptree), g); 
    }
#line 3402 "bison.tab.c"
    break;

  case 176: /* typed_parameters: %empty  */
#line 869 "./src/parser/gens/bison.y"
                  {
         (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3410 "bison.tab.c"
    break;

  case 177: /* type_params: WORD  */
#line 875 "./src/parser/gens/bison.y"
         {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_create(PTREE_TYPE_PARAMS, 1, w); 
    }
#line 3419 "bison.tab.c"
    break;

  case 178: /* type_params: type_params ',' WORD  */
#line 879 "./src/parser/gens/bison.y"
                           {
        ptree_t *w = ptree_create_symbol((yyvsp[0].str));
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, w);
    }
#line 3428 "bison.tab.c"
    break;

  case 179: /* type_statement: TYPE WORD typed_parameters '=' type ';'  */
#line 886 "./src/parser/gens/bison.y"
                                            {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        ptree_t *sc = ptree_create(PTREE_SEMICOLON, 0);
        (yyval.ptree) = ptree_create(PTREE_TYPE_ALIAS, 5, w, (yyvsp[-3].ptree), eq, (yyvsp[-1].ptree), sc); 
    }
#line 3439 "bison.tab.c"
    break;

  case 180: /* struct_statement: STRUCT WORD ';'  */
#line 895 "./src/parser/gens/bison.y"
                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-1].str));
        (yyval.ptree) = ptree_create(PTREE_UNIT_STRUCT, 1, w);
    }
#line 3448 "bison.tab.c"
    break;

  case 181: /* struct_statement: STRUCT WORD typed_parameters '{' field_list '}'  */
#line 899 "./src/parser/gens/bison.y"
                                                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_STRUCT, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3459 "bison.tab.c"
    break;

  case 182: /* union_statement: UNION WORD typed_parameters '{' field_list '}'  */
#line 908 "./src/parser/gens/bison.y"
                                                   {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_UNION, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3470 "bison.tab.c"
    break;

  case 183: /* field_list: identifier  */
#line 917 "./src/parser/gens/bison.y"
               {
        (yyval.ptree) = ptree_create(PTREE_FIELDS, 1, (yyvsp[0].ptree));
    }
#line 3478 "bison.tab.c"
    break;

  case 184: /* field_list: field_list ',' identifier  */
#line 920 "./src/parser/gens/bison.y"
                                {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3486 "bison.tab.c"
    break;

  case 185: /* field_list: field_list ','  */
#line 923 "./src/parser/gens/bison.y"
                     {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3494 "bison.tab.c"
    break;

  case 186: /* field_list: %empty  */
#line 926 "./src/parser/gens/bison.y"
                  {
        (yyval.ptree) = ptree_create(PTREE_EMPTY, 0);     
    }
#line 3502 "bison.tab.c"
    break;

  case 187: /* enum_statement: ENUM WORD typed_parameters '{' enum_members '}'  */
#line 932 "./src/parser/gens/bison.y"
                                                    {
        ptree_t *w = ptree_create_symbol((yyvsp[-4].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_LBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM, 5, w, (yyvsp[-3].ptree), left, (yyvsp[-1].ptree), right);
    }
#line 3513 "bison.tab.c"
    break;

  case 188: /* enum_members: enum_member  */
#line 941 "./src/parser/gens/bison.y"
                {
        (yyval.ptree) = ptree_create(PTREE_ENUM_MEMBERS, 1, (yyvsp[0].ptree));
    }
#line 3521 "bison.tab.c"
    break;

  case 189: /* enum_members: enum_members ',' enum_member  */
#line 944 "./src/parser/gens/bison.y"
                                   {
        (yyval.ptree) = ptree_add((yyvsp[-2].ptree), 1, (yyvsp[0].ptree));
    }
#line 3529 "bison.tab.c"
    break;

  case 190: /* enum_members: enum_members ','  */
#line 947 "./src/parser/gens/bison.y"
                       {
        (yyval.ptree) = (yyvsp[-1].ptree);
    }
#line 3537 "bison.tab.c"
    break;

  case 191: /* enum_member: WORD  */
#line 953 "./src/parser/gens/bison.y"
         {
        (yyval.ptree) = ptree_create_symbol((yyvsp[0].str));
    }
#line 3545 "bison.tab.c"
    break;

  case 192: /* enum_member: WORD '(' types_list ')'  */
#line 956 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LPRN, 0);
        ptree_t *right = ptree_create(PTREE_RPRN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3556 "bison.tab.c"
    break;

  case 193: /* enum_member: WORD '{' field_list '}'  */
#line 962 "./src/parser/gens/bison.y"
                              {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_NAMED_STRUCT, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3567 "bison.tab.c"
    break;

  case 194: /* enum_member: WORD '=' NUMBER  */
#line 968 "./src/parser/gens/bison.y"
                      {
        ptree_t *w = ptree_create_symbol((yyvsp[-2].str));
        ptree_t *num = ptree_create_num((yyvsp[0].num));
        ptree_t *eq = ptree_create(PTREE_ASSIGN, 0);
        (yyval.ptree) = ptree_create(PTREE_ENUM_DEFAULT, 3, w, eq, num);
    }
#line 3578 "bison.tab.c"
    break;

  case 195: /* methods_statement: METHODS WORD '{' method_list '}'  */
#line 977 "./src/parser/gens/bison.y"
                                     {
        ptree_t *w = ptree_create_symbol((yyvsp[-3].str));
        ptree_t *left = ptree_create(PTREE_LBRACE, 0);
        ptree_t *right = ptree_create(PTREE_RBRACE, 0);
        (yyval.ptree) = ptree_create(PTREE_METHODS, 4, w, left, (yyvsp[-1].ptree), right);
    }
#line 3589 "bison.tab.c"
    break;

  case 196: /* method_list: function_declaration  */
#line 986 "./src/parser/gens/bison.y"
                         {
        (yyval.ptree) = ptree_create(PTREE_METHODS_LIST, 1, (yyvsp[0].ptree));
    }
#line 3597 "bison.tab.c"
    break;

  case 197: /* method_list: method_list function_declaration  */
#line 989 "./src/parser/gens/bison.y"
                                       {
        (yyval.ptree) = ptree_add((yyvsp[-1].ptree), 1, (yyvsp[0].ptree));
    }
#line 3605 "bison.tab.c"
    break;


#line 3609 "bison.tab.c"

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

#line 994 "./src/parser/gens/bison.y"
