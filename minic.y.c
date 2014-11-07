/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "minic.y" /* yacc.c:339  */

 // Name: Maulik Padia 
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"

#include "list.h"
#include "symbol.h"

int num_errors;
extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror();
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;
 LLVMBuilderRef Builder;

LLVMValueRef Function=NULL;
LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params);


#line 101 "minic.y.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "minic.y.h".  */
#ifndef YY_YY_MINIC_Y_H_INCLUDED
# define YY_YY_MINIC_Y_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    SEMICOLON = 258,
    COMMA = 259,
    COLON = 260,
    LBRACE = 261,
    RBRACE = 262,
    LPAREN = 263,
    RPAREN = 264,
    LBRACKET = 265,
    RBRACKET = 266,
    ASSIGN = 267,
    PLUS = 268,
    MINUS = 269,
    STAR = 270,
    DIV = 271,
    MOD = 272,
    LT = 273,
    GT = 274,
    LTE = 275,
    GTE = 276,
    EQ = 277,
    NEQ = 278,
    NOT = 279,
    LOGICAL_AND = 280,
    LOGICAL_OR = 281,
    BITWISE_OR = 282,
    BITWISE_XOR = 283,
    LSHIFT = 284,
    RSHIFT = 285,
    BITWISE_INVERT = 286,
    DOT = 287,
    ARROW = 288,
    AMPERSAND = 289,
    QUESTION_MARK = 290,
    FOR = 291,
    WHILE = 292,
    IF = 293,
    ELSE = 294,
    DO = 295,
    STRUCT = 296,
    SIZEOF = 297,
    RETURN = 298,
    BREAK = 299,
    CONTINUE = 300,
    INT = 301,
    VOID = 302,
    STATIC = 303,
    AUTO = 304,
    EXTERN = 305,
    TYPEDEF = 306,
    CONST = 307,
    VOLATILE = 308,
    ENUM = 309,
    UNION = 310,
    REGISTER = 311,
    NUMBER = 312,
    ID = 313,
    LOWER_THAN_ELSE = 314
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 38 "minic.y" /* yacc.c:355  */

  int num;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  paramlist_t *params;

#line 210 "minic.y.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_MINIC_Y_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 225 "minic.y.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

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

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
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


#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   172

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  60
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  50
/* YYNRULES -- Number of rules.  */
#define YYNRULES  103
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  176

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   110,   110,   111,   114,   126,   133,   132,   152,   151,
     173,   186,   201,   205,   212,   219,   226,   229,   236,   240,
     244,   248,   253,   258,   266,   267,   268,   269,   270,   271,
     272,   275,   279,   285,   292,   295,   302,   305,   311,   315,
     321,   337,   355,   377,   354,   390,   397,   390,   426,   434,
     441,   425,   472,   476,   482,   487,   504,   510,   514,   535,
     539,   546,   550,   573,   577,   601,   605,   612,   616,   623,
     627,   633,   637,   643,   651,   655,   661,   667,   673,   681,
     685,   690,   697,   701,   713,   728,   732,   736,   740,   746,
     750,   768,   789,   793,   805,   810,   815,   819,   823,   833,
     839,   855,   859,   865
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SEMICOLON", "COMMA", "COLON", "LBRACE",
  "RBRACE", "LPAREN", "RPAREN", "LBRACKET", "RBRACKET", "ASSIGN", "PLUS",
  "MINUS", "STAR", "DIV", "MOD", "LT", "GT", "LTE", "GTE", "EQ", "NEQ",
  "NOT", "LOGICAL_AND", "LOGICAL_OR", "BITWISE_OR", "BITWISE_XOR",
  "LSHIFT", "RSHIFT", "BITWISE_INVERT", "DOT", "ARROW", "AMPERSAND",
  "QUESTION_MARK", "FOR", "WHILE", "IF", "ELSE", "DO", "STRUCT", "SIZEOF",
  "RETURN", "BREAK", "CONTINUE", "INT", "VOID", "STATIC", "AUTO", "EXTERN",
  "TYPEDEF", "CONST", "VOLATILE", "ENUM", "UNION", "REGISTER", "NUMBER",
  "ID", "LOWER_THAN_ELSE", "$accept", "translation_unit",
  "external_declaration", "function_definition", "$@1", "$@2",
  "declaration", "declaration_list", "type_specifier", "declarator",
  "param_list_opt", "param_list", "statement", "expr_stmt",
  "compound_stmt", "declaration_list_opt", "statement_list_opt",
  "statement_list", "break_stmt", "continue_stmt", "selection_stmt", "@3",
  "$@4", "iteration_stmt", "@5", "@6", "@7", "@8", "@9", "expr_opt",
  "jump_stmt", "expression", "assignment_expression",
  "conditional_expression", "logical_OR_expression",
  "logical_AND_expression", "inclusive_OR_expression",
  "exclusive_OR_expression", "AND_expression", "equality_expression",
  "relational_expression", "shift_expression", "additive_expression",
  "multiplicative_expression", "cast_expression", "lhs_expression",
  "unary_expression", "postfix_expression", "primary_expression",
  "constant", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314
};
# endif

#define YYPACT_NINF -151

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-151)))

#define YYTABLE_NINF -92

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -39,  -151,     4,  -151,  -151,  -151,   -10,  -151,  -151,   -46,
      14,    32,    39,    50,   -39,  -151,   -39,  -151,     1,    67,
      76,    83,    44,  -151,  -151,  -151,   -39,  -151,  -151,   102,
       6,   102,   -39,  -151,    44,  -151,  -151,  -151,   -39,    17,
       0,  -151,  -151,    44,  -151,    47,    55,    55,    -6,    55,
      55,     9,   109,   113,   115,    15,   124,   127,  -151,   120,
    -151,  -151,  -151,   134,     0,  -151,  -151,  -151,  -151,  -151,
     141,  -151,  -151,   -15,   121,   118,   119,   114,     3,    64,
      70,   106,    25,  -151,   137,  -151,  -151,  -151,  -151,   142,
       9,  -151,  -151,  -151,   138,  -151,  -151,  -151,  -151,    47,
    -151,    47,  -151,   149,  -151,  -151,  -151,  -151,  -151,    55,
      47,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    47,  -151,
    -151,  -151,    47,   144,  -151,   121,   150,   118,   119,   114,
       3,    64,    64,    70,    70,    70,    70,   106,   106,    25,
      25,  -151,  -151,  -151,  -151,   151,   147,  -151,    55,    47,
    -151,     0,  -151,  -151,     0,  -151,   154,  -151,   122,    47,
       0,  -151,  -151,   153,     0,  -151
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    14,     0,     2,     4,     5,     0,     1,     3,     0,
      15,     0,    15,     0,    16,    11,    16,    10,    23,     0,
      17,     0,     0,    15,    21,     6,     0,     8,    22,     0,
      20,     0,    34,     7,     0,    18,     9,    12,    35,     0,
      36,    19,    13,     0,    31,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   100,
      38,    24,    25,     0,    37,    29,    30,    26,    27,    28,
       0,    56,    57,    59,    61,    63,    65,    67,    69,    71,
      74,    79,    82,    85,     0,    89,    92,    99,   101,     0,
       0,   100,    96,    95,   100,    94,    98,    97,    93,    52,
      45,     0,    54,     0,    40,    41,    33,    39,    32,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   102,
      48,    53,     0,     0,    55,    62,     0,    64,    66,    68,
      70,    72,    73,    75,    76,    77,    78,    80,    81,    83,
      84,    86,    87,    88,    58,     0,     0,    42,     0,    52,
      46,     0,    60,    49,     0,    43,     0,    47,     0,    52,
       0,    50,    44,     0,     0,    51
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -151,  -151,   156,  -151,  -151,  -151,    -5,  -151,    77,    88,
     143,  -151,   -63,  -151,    59,  -151,  -151,  -151,  -151,  -151,
    -151,  -151,  -151,  -151,  -151,  -151,  -151,  -151,  -151,  -150,
    -151,   -45,    35,     2,  -151,    56,    53,    54,    57,    58,
      22,    16,    18,    19,   -31,  -151,    79,  -151,    26,  -151
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,     4,    29,    31,     5,    38,     6,    11,
      19,    20,    60,    61,    62,    40,    63,    64,    65,    66,
      67,   161,   168,    68,   132,   164,   155,   166,   173,   130,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      89,   107,    45,    44,     7,     9,    32,     1,    45,   163,
     103,   109,    12,    46,    47,    48,    22,    45,   102,   171,
     110,    34,    14,    45,    49,   115,   116,    37,    46,    47,
      48,    50,    43,    42,    51,    15,    52,    53,    54,    49,
     125,   126,   127,    55,    56,    57,    50,    16,    10,    51,
       1,    58,    94,    17,   131,    45,   133,    58,    59,    23,
      46,    47,    48,    45,    23,   136,    58,    91,    46,    47,
      90,    49,    58,    59,    95,    23,    25,    98,    50,    49,
      26,    51,   117,   118,   119,   120,    50,   156,    33,    51,
      36,    18,    27,    18,   151,   152,   153,    13,   165,   121,
     122,   167,    23,    30,    58,    59,    24,   172,    32,    39,
      28,   175,    58,    91,   131,    39,    95,    99,    35,   123,
     124,   100,    41,   101,   131,    92,    93,   104,    96,    97,
     105,    13,   -90,   143,   144,   145,   146,   141,   142,   147,
     148,   106,   149,   150,   108,   112,   111,   113,   114,   128,
     -91,   129,   134,   157,   159,   158,   160,   169,     8,    21,
     162,   170,   174,   154,   137,   135,   138,     0,     0,     0,
     139,     0,   140
};

static const yytype_int16 yycheck[] =
{
      45,    64,     8,     3,     0,    15,     6,    46,     8,   159,
      55,    26,    58,    13,    14,    15,    15,     8,     3,   169,
      35,    15,     8,     8,    24,    22,    23,    32,    13,    14,
      15,    31,    15,    38,    34,     3,    36,    37,    38,    24,
      15,    16,    17,    43,    44,    45,    31,     8,    58,    34,
      46,    57,    58,     3,    99,     8,   101,    57,    58,    58,
      13,    14,    15,     8,    58,   110,    57,    58,    13,    14,
      15,    24,    57,    58,    48,    58,     9,    51,    31,    24,
       4,    34,    18,    19,    20,    21,    31,   132,    29,    34,
      31,    14,     9,    16,   125,   126,   127,     9,   161,    29,
      30,   164,    58,    26,    57,    58,    18,   170,     6,    32,
      22,   174,    57,    58,   159,    38,    90,     8,    30,    13,
      14,     8,    34,     8,   169,    46,    47,     3,    49,    50,
       3,    43,    12,   117,   118,   119,   120,   115,   116,   121,
     122,     7,   123,   124,     3,    27,    25,    28,    34,    12,
      12,     9,     3,     9,     3,     5,     9,     3,     2,    16,
     158,    39,     9,   128,   111,   109,   112,    -1,    -1,    -1,
     113,    -1,   114
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    46,    61,    62,    63,    66,    68,     0,    62,    15,
      58,    69,    58,    69,     8,     3,     8,     3,    68,    70,
      71,    70,    15,    58,    69,     9,     4,     9,    69,    64,
      68,    65,     6,    74,    15,    69,    74,    66,    67,    68,
      75,    69,    66,    15,     3,     8,    13,    14,    15,    24,
      31,    34,    36,    37,    38,    43,    44,    45,    57,    58,
      72,    73,    74,    76,    77,    78,    79,    80,    83,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    91,
      15,    58,   106,   106,    58,   108,   106,   106,   108,     8,
       8,     8,     3,    91,     3,     3,     7,    72,     3,    26,
      35,    25,    27,    28,    34,    22,    23,    18,    19,    20,
      21,    29,    30,    13,    14,    15,    16,    17,    12,     9,
      89,    91,    84,    91,     3,    95,    91,    96,    97,    98,
      99,   100,   100,   101,   101,   101,   101,   102,   102,   103,
     103,   104,   104,   104,    92,    86,    91,     9,     5,     3,
       9,    81,    93,    89,    85,    72,    87,    72,    82,     3,
      39,    89,    72,    88,     9,    72
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    60,    61,    61,    62,    62,    64,    63,    65,    63,
      66,    66,    67,    67,    68,    69,    70,    70,    71,    71,
      71,    71,    71,    71,    72,    72,    72,    72,    72,    72,
      72,    73,    73,    74,    75,    75,    76,    76,    77,    77,
      78,    79,    81,    82,    80,    84,    85,    83,    86,    87,
      88,    83,    89,    89,    90,    90,    91,    92,    92,    93,
      93,    94,    94,    95,    95,    96,    96,    97,    97,    98,
      98,    99,    99,    99,   100,   100,   100,   100,   100,   101,
     101,   101,   102,   102,   102,   103,   103,   103,   103,   104,
     105,   105,   106,   106,   106,   106,   106,   106,   106,   107,
     108,   108,   108,   109
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     0,     7,     0,     8,
       4,     3,     1,     2,     1,     1,     0,     1,     4,     5,
       3,     2,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     4,     0,     1,     0,     1,     1,     2,
       2,     2,     0,     0,     9,     0,     0,     7,     0,     0,
       0,    12,     0,     1,     2,     3,     1,     1,     3,     1,
       5,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     3,     1,     3,     3,     3,     3,     1,
       3,     3,     1,     3,     3,     1,     3,     3,     3,     1,
       1,     2,     1,     2,     2,     2,     2,     2,     2,     1,
       1,     1,     3,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
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

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
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
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
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
            /* Fall through.  */
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

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
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
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
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
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
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
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
| yyreduce -- Do a reduction.  |
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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 115 "minic.y" /* yacc.c:1646  */
    {
  /* finish compiling function */
  if(num_errors>100)
    {
      minic_abort();
    }
  else if(num_errors==0)
    {
      
    }
}
#line 1455 "minic.y.c" /* yacc.c:1646  */
    break;

  case 5:
#line 127 "minic.y" /* yacc.c:1646  */
    { 
  /* nothing to be done here */
}
#line 1463 "minic.y.c" /* yacc.c:1646  */
    break;

  case 6:
#line 133 "minic.y" /* yacc.c:1646  */
    {
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction((yyvsp[-4].type),(yyvsp[-3].id),(yyvsp[-1].params));  
}
#line 1473 "minic.y.c" /* yacc.c:1646  */
    break;

  case 7:
#line 139 "minic.y" /* yacc.c:1646  */
    { 
  /* This is the rule completion */
  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      LLVMBuildRet(Builder,LLVMConstInt(LLVMInt32TypeInContext(Context),
					0,(LLVMBool)1));
    }

  symbol_pop_scope();
  /* make sure basic block has a terminator (a return statement) */
}
#line 1490 "minic.y.c" /* yacc.c:1646  */
    break;

  case 8:
#line 152 "minic.y" /* yacc.c:1646  */
    {
  symbol_push_scope();
  BuildFunction(LLVMPointerType((yyvsp[-5].type),0),(yyvsp[-3].id),(yyvsp[-1].params));
}
#line 1499 "minic.y.c" /* yacc.c:1646  */
    break;

  case 9:
#line 157 "minic.y" /* yacc.c:1646  */
    { 
  /* This is the rule completion */


  /* make sure basic block has a terminator (a return statement) */

  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType(LLVMInt32TypeInContext(Context),0)));
    }

  symbol_pop_scope();
}
#line 1518 "minic.y.c" /* yacc.c:1646  */
    break;

  case 10:
#line 174 "minic.y" /* yacc.c:1646  */
    {
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,LLVMPointerType((yyvsp[-3].type),0),(yyvsp[-1].id));
    } 
  else
    {
      symbol_insert((yyvsp[-1].id),  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType((yyvsp[-3].type),0),(yyvsp[-1].id)), /* build alloca */
		    0);  /* not an arg */
    }
}
#line 1535 "minic.y.c" /* yacc.c:1646  */
    break;

  case 11:
#line 187 "minic.y" /* yacc.c:1646  */
    {
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,(yyvsp[-2].type),(yyvsp[-1].id));
    }
  else
    {
      symbol_insert((yyvsp[-1].id),  /* map name to alloca */
		    LLVMBuildAlloca(Builder,(yyvsp[-2].type),(yyvsp[-1].id)), /* build alloca */
		    0);  /* not an arg */
    }
}
#line 1552 "minic.y.c" /* yacc.c:1646  */
    break;

  case 12:
#line 202 "minic.y" /* yacc.c:1646  */
    {

}
#line 1560 "minic.y.c" /* yacc.c:1646  */
    break;

  case 13:
#line 206 "minic.y" /* yacc.c:1646  */
    {

}
#line 1568 "minic.y.c" /* yacc.c:1646  */
    break;

  case 14:
#line 213 "minic.y" /* yacc.c:1646  */
    {
  (yyval.type) = LLVMInt32TypeInContext(Context);
}
#line 1576 "minic.y.c" /* yacc.c:1646  */
    break;

  case 15:
#line 220 "minic.y" /* yacc.c:1646  */
    {
  (yyval.id) = (yyvsp[0].id);
}
#line 1584 "minic.y.c" /* yacc.c:1646  */
    break;

  case 16:
#line 226 "minic.y" /* yacc.c:1646  */
    { 
  (yyval.params) = NULL;
}
#line 1592 "minic.y.c" /* yacc.c:1646  */
    break;

  case 17:
#line 230 "minic.y" /* yacc.c:1646  */
    { 
  (yyval.params) = (yyvsp[0].params);
}
#line 1600 "minic.y.c" /* yacc.c:1646  */
    break;

  case 18:
#line 237 "minic.y" /* yacc.c:1646  */
    {
  (yyval.params) = push_param((yyvsp[-3].params),(yyvsp[0].id),(yyvsp[-1].type));
}
#line 1608 "minic.y.c" /* yacc.c:1646  */
    break;

  case 19:
#line 241 "minic.y" /* yacc.c:1646  */
    {
  (yyval.params) = push_param((yyvsp[-4].params),(yyvsp[0].id),LLVMPointerType((yyvsp[-2].type),0));
}
#line 1616 "minic.y.c" /* yacc.c:1646  */
    break;

  case 20:
#line 245 "minic.y" /* yacc.c:1646  */
    {
  (yyval.params) = push_param((yyvsp[-2].params),NULL,(yyvsp[0].type));
}
#line 1624 "minic.y.c" /* yacc.c:1646  */
    break;

  case 21:
#line 249 "minic.y" /* yacc.c:1646  */
    {
  /* create a parameter list with this as the first entry */
  (yyval.params) = push_param(NULL, (yyvsp[0].id), (yyvsp[-1].type));
}
#line 1633 "minic.y.c" /* yacc.c:1646  */
    break;

  case 22:
#line 254 "minic.y" /* yacc.c:1646  */
    {
  /* create a parameter list with this as the first entry */
  (yyval.params) = push_param(NULL, (yyvsp[0].id), LLVMPointerType((yyvsp[-2].type),0));
}
#line 1642 "minic.y.c" /* yacc.c:1646  */
    break;

  case 23:
#line 259 "minic.y" /* yacc.c:1646  */
    {
  /* create a parameter list with this as the first entry */
  (yyval.params) = push_param(NULL, NULL, (yyvsp[0].type));
}
#line 1651 "minic.y.c" /* yacc.c:1646  */
    break;

  case 31:
#line 276 "minic.y" /* yacc.c:1646  */
    { 

}
#line 1659 "minic.y.c" /* yacc.c:1646  */
    break;

  case 32:
#line 280 "minic.y" /* yacc.c:1646  */
    { 

}
#line 1667 "minic.y.c" /* yacc.c:1646  */
    break;

  case 33:
#line 286 "minic.y" /* yacc.c:1646  */
    {

}
#line 1675 "minic.y.c" /* yacc.c:1646  */
    break;

  case 34:
#line 292 "minic.y" /* yacc.c:1646  */
    {

}
#line 1683 "minic.y.c" /* yacc.c:1646  */
    break;

  case 35:
#line 296 "minic.y" /* yacc.c:1646  */
    {

}
#line 1691 "minic.y.c" /* yacc.c:1646  */
    break;

  case 36:
#line 302 "minic.y" /* yacc.c:1646  */
    {

}
#line 1699 "minic.y.c" /* yacc.c:1646  */
    break;

  case 37:
#line 306 "minic.y" /* yacc.c:1646  */
    {

}
#line 1707 "minic.y.c" /* yacc.c:1646  */
    break;

  case 38:
#line 312 "minic.y" /* yacc.c:1646  */
    {

}
#line 1715 "minic.y.c" /* yacc.c:1646  */
    break;

  case 39:
#line 316 "minic.y" /* yacc.c:1646  */
    {

}
#line 1723 "minic.y.c" /* yacc.c:1646  */
    break;

  case 40:
#line 322 "minic.y" /* yacc.c:1646  */
    {
  // If Break called without loop, give error
  loop_info_t info = get_loop();
  if(info.body == NULL) {
     num_errors++;
     return 0;
  }
  // Make a new block which will have statements following break in it
  // Brach back to join
  LLVMBasicBlockRef breakTojoin = info.exit;
  LLVMBuildBr(Builder,breakTojoin);
  LLVMBasicBlockRef foundbreak = LLVMAppendBasicBlock(Function,"found.break");
  LLVMPositionBuilderAtEnd(Builder, foundbreak);
}
#line 1742 "minic.y.c" /* yacc.c:1646  */
    break;

  case 41:
#line 338 "minic.y" /* yacc.c:1646  */
    {
   // If Continue called without loop, give error
  loop_info_t info = get_loop();
  if(info.body == NULL) {
     num_errors++;
     return 0;
  }
  // Make a new block which will have statements following continue in it
  // Brach back to reinit
  LLVMBasicBlockRef contTocond = info.reinit;
  LLVMBuildBr(Builder,contTocond);
  LLVMBasicBlockRef foundcontinue = LLVMAppendBasicBlock(Function,"found.continue");
  LLVMPositionBuilderAtEnd(Builder, foundcontinue);  
}
#line 1761 "minic.y.c" /* yacc.c:1646  */
    break;

  case 42:
#line 355 "minic.y" /* yacc.c:1646  */
    {
  LLVMBasicBlockRef then = LLVMAppendBasicBlock(Function,"then.block");
  LLVMBasicBlockRef elsethen = LLVMAppendBasicBlock(Function,"elsethen.block");
  LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"join.block");

  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf((yyvsp[-1].value)),0,1);

  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-1].value),
                                  zero,"cond");
  LLVMValueRef cmpzero = LLVMConstInt(LLVMTypeOf(cond),0,1);
  LLVMBuildCondBr(Builder,cond,then,elsethen);
  
  // Check if contidion is true or false, insert flow in accordingly
  if (cond == cmpzero) {
    LLVMPositionBuilderAtEnd(Builder,then);
  }
  else {
    LLVMPositionBuilderAtEnd(Builder,elsethen);
  }
  (yyval.bb) = join;

}
#line 1788 "minic.y.c" /* yacc.c:1646  */
    break;

  case 43:
#line 377 "minic.y" /* yacc.c:1646  */
    {
  LLVMBasicBlockRef join = (yyvsp[-1].bb);
  LLVMBuildBr(Builder,join); // from then -> join
  LLVMPositionBuilderAtEnd(Builder,join);

}
#line 1799 "minic.y.c" /* yacc.c:1646  */
    break;

  case 44:
#line 383 "minic.y" /* yacc.c:1646  */
    { 
  LLVMBasicBlockRef join = (yyvsp[-4].bb);
  LLVMBuildBr(Builder,join); // from elsethen -> join
  LLVMPositionBuilderAtEnd(Builder,join);
}
#line 1809 "minic.y.c" /* yacc.c:1646  */
    break;

  case 45:
#line 390 "minic.y" /* yacc.c:1646  */
    { 
  // Setting up the condtion block of the while loop
  LLVMBasicBlockRef cond = LLVMAppendBasicBlock(Function,"while.cond");
  LLVMBuildBr(Builder,cond);
  LLVMPositionBuilderAtEnd(Builder, cond);
  (yyval.bb) = cond;

}
#line 1822 "minic.y.c" /* yacc.c:1646  */
    break;

  case 46:
#line 397 "minic.y" /* yacc.c:1646  */
    { 
  // Append new blocks join and body
  LLVMBasicBlockRef body = LLVMAppendBasicBlock(Function,"while.body");
  LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"while.join");

  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf((yyvsp[-1].value)),0,1);
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-1].value), zero,"cond");

  // based on contion true or false, take branch accordingly
  LLVMBuildCondBr(Builder,cond,body,join);

  LLVMPositionBuilderAtEnd(Builder,body);
  (yyval.bb) = join;


  // Used to support break and continue/
  push_loop(NULL,body,(yyvsp[-2].bb),join);
}
#line 1845 "minic.y.c" /* yacc.c:1646  */
    break;

  case 47:
#line 416 "minic.y" /* yacc.c:1646  */
    {
  // Check the loop on stack
  loop_info_t info = get_loop();
  LLVMBasicBlockRef cond = info.reinit;                  
  LLVMBuildBr(Builder,cond);   
  LLVMPositionBuilderAtEnd(Builder, info.exit);          
  // Pop loop from stack 
  pop_loop();
}
#line 1859 "minic.y.c" /* yacc.c:1646  */
    break;

  case 48:
#line 426 "minic.y" /* yacc.c:1646  */
    {
  // Setting up the condtion block of the for loop
  LLVMBasicBlockRef forcond = LLVMAppendBasicBlock(Function,"for.cond");
  LLVMBuildBr(Builder,forcond);                                        // insert flow in condition block  
  LLVMPositionBuilderAtEnd(Builder, forcond);                          
  (yyval.bb) = forcond;                                                     
}
#line 1871 "minic.y.c" /* yacc.c:1646  */
    break;

  case 49:
#line 434 "minic.y" /* yacc.c:1646  */
    {
  // Setting up the update condition block 
  LLVMBasicBlockRef forupdate = LLVMAppendBasicBlock(Function,"for.update");
  LLVMPositionBuilderAtEnd(Builder, forupdate);                           
  (yyval.bb) = forupdate;                                                    
}
#line 1882 "minic.y.c" /* yacc.c:1646  */
    break;

  case 50:
#line 441 "minic.y" /* yacc.c:1646  */
    {
  
  LLVMBuildBr(Builder,(yyvsp[-5].bb));                                         // insert flow in condition blok
  LLVMPositionBuilderAtEnd(Builder, (yyvsp[-5].bb));                           // insert statements in condition block
  
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf((yyvsp[-3].value)),0,1);                           
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-3].value), zero,"cond");         
  
  LLVMBasicBlockRef forstatement = LLVMAppendBasicBlock(Function,"for.statement");
  LLVMBasicBlockRef forjoin = LLVMAppendBasicBlock(Function,"for.join");          
  
  // based on condition true or flase, take branch accordingly
  LLVMBuildCondBr(Builder,cond,forstatement,forjoin);           
  (yyval.bb) = forjoin;                                                               
  LLVMPositionBuilderAtEnd(Builder, forstatement);                                
  
  // Pushed to support break or continue  
  push_loop(NULL, forstatement, (yyvsp[-5].bb), forjoin); 
}
#line 1906 "minic.y.c" /* yacc.c:1646  */
    break;

  case 51:
#line 461 "minic.y" /* yacc.c:1646  */
    {
  LLVMBasicBlockRef forupdate = (yyvsp[-5].bb);      
  LLVMBuildBr(Builder,forupdate);          
  LLVMPositionBuilderAtEnd(Builder,(yyvsp[-2].bb));

//  Pop loop from stack 
  pop_loop();
}
#line 1919 "minic.y.c" /* yacc.c:1646  */
    break;

  case 52:
#line 472 "minic.y" /* yacc.c:1646  */
    {
  // If there is not condition then it should make condition true 
  (yyval.value) = LLVMConstInt(LLVMIntType(1),1,1); 
}
#line 1928 "minic.y.c" /* yacc.c:1646  */
    break;

  case 53:
#line 477 "minic.y" /* yacc.c:1646  */
    { 
  (yyval.value) = (yyvsp[0].value);
}
#line 1936 "minic.y.c" /* yacc.c:1646  */
    break;

  case 54:
#line 483 "minic.y" /* yacc.c:1646  */
    { 
  LLVMBuildRetVoid(Builder);

}
#line 1945 "minic.y.c" /* yacc.c:1646  */
    break;

  case 55:
#line 488 "minic.y" /* yacc.c:1646  */
    {
  // If return is constant type just return it in 32 bit type
  int i;
  if (LLVMIsAConstantInt((yyvsp[-1].value))) {
    i =LLVMConstIntGetSExtValue((yyvsp[-1].value));
    LLVMValueRef ret = LLVMConstInt(LLVMInt32Type(),i,0);
    LLVMBuildRet(Builder,ret);
  }
  else {
    // If return is not constant type, just return the value, which will be point in our grammer
    LLVMBuildRet(Builder,(yyvsp[-1].value));
  }

}
#line 1964 "minic.y.c" /* yacc.c:1646  */
    break;

  case 56:
#line 505 "minic.y" /* yacc.c:1646  */
    { 
  (yyval.value)=(yyvsp[0].value);
}
#line 1972 "minic.y.c" /* yacc.c:1646  */
    break;

  case 57:
#line 511 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value)=(yyvsp[0].value);
}
#line 1980 "minic.y.c" /* yacc.c:1646  */
    break;

  case 58:
#line 515 "minic.y" /* yacc.c:1646  */
    {
  // if lhs is pointer type and rhs is constant, update the lhs converting rhs to pointer type 
  if ( LLVMTypeOf((yyvsp[-2].value)) == LLVMPointerType(LLVMPointerType(LLVMInt32Type(),0),0)) {
    if (LLVMTypeOf((yyvsp[0].value)) == LLVMInt32Type()) {
      LLVMValueRef IntToPtr = LLVMBuildIntToPtr(Builder,(yyvsp[0].value),LLVMPointerType(LLVMInt32Type(),0),"");
      LLVMBuildStore(Builder,IntToPtr,(yyvsp[-2].value));
    }
    else {
      // pointer to pointer assignment
      LLVMBuildStore(Builder,(yyvsp[0].value),(yyvsp[-2].value));
    }
  } 
  else {
    // int to int assignment
    LLVMBuildStore(Builder,(yyvsp[0].value),(yyvsp[-2].value));
  }
}
#line 2002 "minic.y.c" /* yacc.c:1646  */
    break;

  case 59:
#line 536 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value)=(yyvsp[0].value);
}
#line 2010 "minic.y.c" /* yacc.c:1646  */
    break;

  case 60:
#line 540 "minic.y" /* yacc.c:1646  */
    {
  // Use select command for ? : implelementation
  (yyval.value) = LLVMBuildSelect(Builder, (yyvsp[-4].value), (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2019 "minic.y.c" /* yacc.c:1646  */
    break;

  case 61:
#line 547 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2027 "minic.y.c" /* yacc.c:1646  */
    break;

  case 62:
#line 551 "minic.y" /* yacc.c:1646  */
    {
  // lhs or rhs are not 1 bit type then convert it first to 1 bit type
  LLVMValueRef zero1 = LLVMConstInt(LLVMTypeOf((yyvsp[-2].value)),0,1);
  LLVMValueRef zero2 = LLVMConstInt(LLVMTypeOf((yyvsp[0].value)),0,1);
  if(LLVMIntType(1) != LLVMTypeOf((yyvsp[-2].value))) {
    LLVMValueRef expr1 = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-2].value), zero1,"");
    if(LLVMIntType(1) != LLVMTypeOf((yyvsp[0].value))) {
      LLVMValueRef expr2 = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[0].value), zero2,"");
      LLVMValueRef OR = LLVMBuildOr(Builder, expr1, expr2, "");
      (yyval.value) = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
    }
    else {
      LLVMValueRef OR = LLVMBuildOr(Builder, expr1, (yyvsp[0].value), "");
      (yyval.value) = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
    }
  }
  else {
    LLVMValueRef OR = LLVMBuildOr(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
    (yyval.value) = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
  } 
}
#line 2053 "minic.y.c" /* yacc.c:1646  */
    break;

  case 63:
#line 574 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2061 "minic.y.c" /* yacc.c:1646  */
    break;

  case 64:
#line 578 "minic.y" /* yacc.c:1646  */
    {
  // lhs or rhs are not 1 bit type then convert it first to 1 bit type
  LLVMValueRef zero1 = LLVMConstInt(LLVMTypeOf((yyvsp[-2].value)),0,1);
  LLVMValueRef zero2 = LLVMConstInt(LLVMTypeOf((yyvsp[0].value)),0,1);
  if(LLVMIntType(1) != LLVMTypeOf((yyvsp[-2].value))) {
    LLVMValueRef expr1 = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-2].value), zero1,"");
    if(LLVMIntType(1) != LLVMTypeOf((yyvsp[0].value))) {
      LLVMValueRef expr2 = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[0].value), zero2,"");
      LLVMValueRef AND = LLVMBuildAnd(Builder, expr1, expr2, "");
      (yyval.value) = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
    }
    else {
      LLVMValueRef AND = LLVMBuildAnd(Builder, expr1, (yyvsp[0].value), "");
      (yyval.value) = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
    }
  }
  else {
    LLVMValueRef AND = LLVMBuildAnd(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
    (yyval.value) = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
  }
}
#line 2087 "minic.y.c" /* yacc.c:1646  */
    break;

  case 65:
#line 602 "minic.y" /* yacc.c:1646  */
    {
    (yyval.value)=(yyvsp[0].value);
}
#line 2095 "minic.y.c" /* yacc.c:1646  */
    break;

  case 66:
#line 606 "minic.y" /* yacc.c:1646  */
    {
  // Binary Operation OR
  (yyval.value) = LLVMBuildBinOp(Builder, LLVMOr, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2104 "minic.y.c" /* yacc.c:1646  */
    break;

  case 67:
#line 613 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2112 "minic.y.c" /* yacc.c:1646  */
    break;

  case 68:
#line 617 "minic.y" /* yacc.c:1646  */
    {
  // Binary Operation XOR
  (yyval.value) = LLVMBuildBinOp(Builder, LLVMXor, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2121 "minic.y.c" /* yacc.c:1646  */
    break;

  case 69:
#line 624 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2129 "minic.y.c" /* yacc.c:1646  */
    break;

  case 70:
#line 628 "minic.y" /* yacc.c:1646  */
    {
  // Binary Operation AND
  (yyval.value) = LLVMBuildBinOp(Builder, LLVMAnd, (yyvsp[-2].value), (yyvsp[0].value), "");}
#line 2137 "minic.y.c" /* yacc.c:1646  */
    break;

  case 71:
#line 634 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2145 "minic.y.c" /* yacc.c:1646  */
    break;

  case 72:
#line 638 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2155 "minic.y.c" /* yacc.c:1646  */
    break;

  case 73:
#line 644 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntNE, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2165 "minic.y.c" /* yacc.c:1646  */
    break;

  case 74:
#line 652 "minic.y" /* yacc.c:1646  */
    {
    (yyval.value)=(yyvsp[0].value);
}
#line 2173 "minic.y.c" /* yacc.c:1646  */
    break;

  case 75:
#line 656 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSLT, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2183 "minic.y.c" /* yacc.c:1646  */
    break;

  case 76:
#line 662 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSGT, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2193 "minic.y.c" /* yacc.c:1646  */
    break;

  case 77:
#line 668 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSLE, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2203 "minic.y.c" /* yacc.c:1646  */
    break;

  case 78:
#line 674 "minic.y" /* yacc.c:1646  */
    {
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSGE, (yyvsp[-2].value), (yyvsp[0].value),"");
  (yyval.value) = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
#line 2213 "minic.y.c" /* yacc.c:1646  */
    break;

  case 79:
#line 682 "minic.y" /* yacc.c:1646  */
    {
    (yyval.value)=(yyvsp[0].value);
}
#line 2221 "minic.y.c" /* yacc.c:1646  */
    break;

  case 80:
#line 686 "minic.y" /* yacc.c:1646  */
    {
  // Binary Operation Left Shift
   (yyval.value) = LLVMBuildBinOp(Builder, LLVMLShr, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2230 "minic.y.c" /* yacc.c:1646  */
    break;

  case 81:
#line 691 "minic.y" /* yacc.c:1646  */
    {
  // Binary Operation Right Shift
  (yyval.value) = LLVMBuildBinOp(Builder, LLVMShl, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2239 "minic.y.c" /* yacc.c:1646  */
    break;

  case 82:
#line 698 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2247 "minic.y.c" /* yacc.c:1646  */
    break;

  case 83:
#line 702 "minic.y" /* yacc.c:1646  */
    {
  // To implement pointer arithmetic using GEP
  if(LLVMTypeOf((yyvsp[-2].value)) == LLVMPointerType(LLVMInt32Type(),0)) {
    LLVMValueRef indices[1] = {(yyvsp[0].value)};
    LLVMValueRef gep = LLVMBuildGEP(Builder,(yyvsp[-2].value),indices,1,"");
    (yyval.value) = gep;
  }
  else {
    (yyval.value) = LLVMBuildAdd(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
  }
}
#line 2263 "minic.y.c" /* yacc.c:1646  */
    break;

  case 84:
#line 714 "minic.y" /* yacc.c:1646  */
    {
  // To implement pointer arithmetic using GEP
  if(LLVMTypeOf((yyvsp[-2].value)) == LLVMPointerType(LLVMInt32Type(),0)) {
    LLVMValueRef neg  = LLVMBuildNeg(Builder, (yyvsp[0].value), "");
    LLVMValueRef indices[1] = {neg};
    LLVMValueRef gep = LLVMBuildGEP(Builder,(yyvsp[-2].value),indices,1,"");
    (yyval.value) = gep;
  }
  else {
    (yyval.value) = LLVMBuildSub(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
  }
}
#line 2280 "minic.y.c" /* yacc.c:1646  */
    break;

  case 85:
#line 729 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2288 "minic.y.c" /* yacc.c:1646  */
    break;

  case 86:
#line 733 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = LLVMBuildMul(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2296 "minic.y.c" /* yacc.c:1646  */
    break;

  case 87:
#line 737 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = LLVMBuildSDiv(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2304 "minic.y.c" /* yacc.c:1646  */
    break;

  case 88:
#line 741 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = LLVMBuildSRem(Builder, (yyvsp[-2].value), (yyvsp[0].value), "");
}
#line 2312 "minic.y.c" /* yacc.c:1646  */
    break;

  case 89:
#line 747 "minic.y" /* yacc.c:1646  */
    { (yyval.value) = (yyvsp[0].value); }
#line 2318 "minic.y.c" /* yacc.c:1646  */
    break;

  case 90:
#line 751 "minic.y" /* yacc.c:1646  */
    {
  int isArg=0;
  LLVMValueRef val = symbol_find((yyvsp[0].id),&isArg);
  if(val == NULL) {
    num_errors++;     // error 
    return 0; 
  }
  else {
    if (isArg) {
      num_errors++;  // error
      return 0;
  }
  else {
    (yyval.value) = val;
  }
}
}
#line 2340 "minic.y.c" /* yacc.c:1646  */
    break;

  case 91:
#line 769 "minic.y" /* yacc.c:1646  */
    {
  int isArg=0;
  LLVMValueRef val = symbol_find((yyvsp[0].id),&isArg);
  if(val == NULL) {
    num_errors++;
    return 0;
  }
  else {
    if (isArg) {
      num_errors++;  // error
      return 0;
    }
    else {
     (yyval.value) = LLVMBuildLoad(Builder,val,"");
   }
 }
}
#line 2362 "minic.y.c" /* yacc.c:1646  */
    break;

  case 92:
#line 790 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2370 "minic.y.c" /* yacc.c:1646  */
    break;

  case 93:
#line 794 "minic.y" /* yacc.c:1646  */
    {
  if(LLVMIsALoadInst((yyvsp[0].value))) {
    LLVMValueRef pointer = (yyvsp[0].value);
    LLVMValueRef address = LLVMGetOperand(pointer,0);
    (yyval.value) = address;
  }
  else {
    num_errors++;
    return 0;
  }
}
#line 2386 "minic.y.c" /* yacc.c:1646  */
    break;

  case 94:
#line 806 "minic.y" /* yacc.c:1646  */
    {
  /* fixme */
  (yyval.value) = LLVMBuildLoad(Builder,(yyvsp[0].value),"");
}
#line 2395 "minic.y.c" /* yacc.c:1646  */
    break;

  case 95:
#line 811 "minic.y" /* yacc.c:1646  */
    {
   (yyval.value) = LLVMBuildNeg(Builder, (yyvsp[0].value), "");
  /* implement */
}
#line 2404 "minic.y.c" /* yacc.c:1646  */
    break;

  case 96:
#line 816 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2412 "minic.y.c" /* yacc.c:1646  */
    break;

  case 97:
#line 820 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = LLVMBuildNot(Builder, (yyvsp[0].value), "");
}
#line 2420 "minic.y.c" /* yacc.c:1646  */
    break;

  case 98:
#line 824 "minic.y" /* yacc.c:1646  */
    {
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf((yyvsp[0].value)),0,1);
  // Compare with zero
  (yyval.value) = LLVMBuildICmp(Builder, LLVMIntEQ, (yyvsp[0].value), zero,"cond");
  /* Implement */
}
#line 2431 "minic.y.c" /* yacc.c:1646  */
    break;

  case 99:
#line 834 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2439 "minic.y.c" /* yacc.c:1646  */
    break;

  case 100:
#line 840 "minic.y" /* yacc.c:1646  */
    { 
  int isArg=0;
  LLVMValueRef val = symbol_find((yyvsp[0].id),&isArg);
  if(val == NULL)
  {
    num_errors++;
    return 0;
  }
  else {
    if (isArg)
      (yyval.value) = val;
    else
      (yyval.value) = LLVMBuildLoad(Builder,val,"");
  }
}
#line 2459 "minic.y.c" /* yacc.c:1646  */
    break;

  case 101:
#line 856 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[0].value);
}
#line 2467 "minic.y.c" /* yacc.c:1646  */
    break;

  case 102:
#line 860 "minic.y" /* yacc.c:1646  */
    {
  (yyval.value) = (yyvsp[-1].value);
}
#line 2475 "minic.y.c" /* yacc.c:1646  */
    break;

  case 103:
#line 866 "minic.y" /* yacc.c:1646  */
    {
  // Saving Number as Const int type 32 bit
  (yyval.value) = LLVMConstInt(LLVMInt32Type(),(yyvsp[0].num),0);   
}
#line 2484 "minic.y.c" /* yacc.c:1646  */
    break;


#line 2488 "minic.y.c" /* yacc.c:1646  */
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
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



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
                      yytoken, &yylval);
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

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

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

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 872 "minic.y" /* yacc.c:1906  */


LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);
  LLVMTypeRef *ParamArray = malloc(sizeof(LLVMTypeRef)*size);
  LLVMTypeRef FunType;
  LLVMBasicBlockRef BasicBlock;

  paramlist_t *tmp = params;
  /* Build type for function */
  for(i=size-1; i>=0; i--) 
    {
      ParamArray[i] = tmp->type;
      tmp = next_param(tmp);
    }
  
  FunType = LLVMFunctionType(RetType,ParamArray,size,0);

  Function = LLVMAddFunction(Module,name,FunType);
  
  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);

  tmp = params;
  for(i=size-1; i>=0; i--)
    {
      LLVMValueRef alloca = LLVMBuildAlloca(Builder,tmp->type,tmp->name);
      LLVMBuildStore(Builder,LLVMGetParam(Function,i),alloca);
      symbol_insert(tmp->name,alloca,0);
      tmp=next_param(tmp);
    }

  return Function;
}

extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror()
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to eontinue.");
  exit(1);
}
