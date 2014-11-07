%{
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

%}

/* Data structure for tree nodes*/

%union {
  int num;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN 
%token BREAK CONTINUE
%token INT VOID

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER
/* NUMBER and ID have values associated with them returned from lex*/

%token <num> NUMBER /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/

%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression
%type <value> expr_opt
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <value> primary_expression
%type <value> constant
%type <type>  type_specifier
/* 
   The grammar used here is largely borrowed from Kernighan and Ritchie's "The C
   Programming Language," 2nd Edition, Prentice Hall, 1988. 

   But, some modifications have been made specifically for MiniC!
 */

%%

/* 
   Beginning of grammar: Rules
*/

translation_unit:	  external_declaration
			| translation_unit external_declaration
;

external_declaration:	  function_definition
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
                        | declaration 
{ 
  /* nothing to be done here */
}
;

function_definition:	  type_specifier ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction($1,$2,$4);  
} 
                          compound_stmt 
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
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  BuildFunction(LLVMPointerType($1,0),$3,$5);
} 
                          compound_stmt 
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
;

declaration:    type_specifier STAR declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,LLVMPointerType($1,0),$3);
    } 
  else
    {
      symbol_insert($3,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType($1,0),$3), /* build alloca */
		    0);  /* not an arg */
    }
} 
              | type_specifier declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,$1,$2);
    }
  else
    {
      symbol_insert($2,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,$1,$2), /* build alloca */
		    0);  /* not an arg */
    }
} 
;

declaration_list:	   declaration
{

}
                         | declaration_list declaration  
{

}
;


type_specifier:		  INT 
{
  $$ = LLVMInt32TypeInContext(Context);
}
;


declarator:		  ID
{
  $$ = $1;
}
;

param_list_opt:           
{ 
  $$ = NULL;
}
                        | param_list
{ 
  $$ = $1;
}
;

param_list:	
			  param_list COMMA type_specifier declarator
{
  $$ = push_param($1,$4,$3);
}
			| param_list COMMA type_specifier STAR declarator
{
  $$ = push_param($1,$5,LLVMPointerType($3,0));
}
                        | param_list COMMA type_specifier
{
  $$ = push_param($1,NULL,$3);
}
			|  type_specifier declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $2, $1);
}
			| type_specifier STAR declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $3, LLVMPointerType($1,0));
}
                        | type_specifier
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, NULL, $1);
}
;


statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
;

expr_stmt:	           SEMICOLON            
{ 

}
			|  expression SEMICOLON       
{ 

}
;

compound_stmt:		  LBRACE declaration_list_opt statement_list_opt RBRACE 
{

}
;

declaration_list_opt:	
{

}
			| declaration_list
{

}
;

statement_list_opt:	
{

}
			| statement_list
{

}
;

statement_list:		statement
{

}
			| statement_list statement
{

}
;

break_stmt:               BREAK SEMICOLON
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
};

continue_stmt:            CONTINUE SEMICOLON
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
};

selection_stmt:		  
		          IF LPAREN expression RPAREN 
{
  LLVMBasicBlockRef then = LLVMAppendBasicBlock(Function,"then.block");
  LLVMBasicBlockRef elsethen = LLVMAppendBasicBlock(Function,"elsethen.block");
  LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"join.block");

  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($3),0,1);

  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $3,
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
  $<bb>$ = join;

} statement 
{
  LLVMBasicBlockRef join = $<bb>5;
  LLVMBuildBr(Builder,join); // from then -> join
  LLVMPositionBuilderAtEnd(Builder,join);

} ELSE statement 
{ 
  LLVMBasicBlockRef join = $<bb>5;
  LLVMBuildBr(Builder,join); // from elsethen -> join
  LLVMPositionBuilderAtEnd(Builder,join);
}
;

iteration_stmt:		  WHILE LPAREN { 
  // Setting up the condtion block of the while loop
  LLVMBasicBlockRef cond = LLVMAppendBasicBlock(Function,"while.cond");
  LLVMBuildBr(Builder,cond);
  LLVMPositionBuilderAtEnd(Builder, cond);
  $<bb>$ = cond;

} expression RPAREN { 
  // Append new blocks join and body
  LLVMBasicBlockRef body = LLVMAppendBasicBlock(Function,"while.body");
  LLVMBasicBlockRef join = LLVMAppendBasicBlock(Function,"while.join");

  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($4),0,1);
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $4, zero,"cond");

  // based on contion true or false, take branch accordingly
  LLVMBuildCondBr(Builder,cond,body,join);

  LLVMPositionBuilderAtEnd(Builder,body);
  $<bb>$ = join;


  // Used to support break and continue/
  push_loop(NULL,body,$<bb>3,join);
} 
  statement
{
  // Check the loop on stack
  loop_info_t info = get_loop();
  LLVMBasicBlockRef cond = info.reinit;                  
  LLVMBuildBr(Builder,cond);   
  LLVMPositionBuilderAtEnd(Builder, info.exit);          
  // Pop loop from stack 
  pop_loop();
}
| FOR LPAREN expr_opt
{
  // Setting up the condtion block of the for loop
  LLVMBasicBlockRef forcond = LLVMAppendBasicBlock(Function,"for.cond");
  LLVMBuildBr(Builder,forcond);                                        // insert flow in condition block  
  LLVMPositionBuilderAtEnd(Builder, forcond);                          
  $<bb>$ = forcond;                                                     
} 
SEMICOLON expr_opt
{
  // Setting up the update condition block 
  LLVMBasicBlockRef forupdate = LLVMAppendBasicBlock(Function,"for.update");
  LLVMPositionBuilderAtEnd(Builder, forupdate);                           
  $<bb>$ = forupdate;                                                    
} 
SEMICOLON expr_opt
{
  
  LLVMBuildBr(Builder,$<bb>4);                                         // insert flow in condition blok
  LLVMPositionBuilderAtEnd(Builder, $<bb>4);                           // insert statements in condition block
  
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($6),0,1);                           
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $6, zero,"cond");         
  
  LLVMBasicBlockRef forstatement = LLVMAppendBasicBlock(Function,"for.statement");
  LLVMBasicBlockRef forjoin = LLVMAppendBasicBlock(Function,"for.join");          
  
  // based on condition true or flase, take branch accordingly
  LLVMBuildCondBr(Builder,cond,forstatement,forjoin);           
  $<bb>$ = forjoin;                                                               
  LLVMPositionBuilderAtEnd(Builder, forstatement);                                
  
  // Pushed to support break or continue  
  push_loop(NULL, forstatement, $<bb>4, forjoin); 
}
RPAREN statement
{
  LLVMBasicBlockRef forupdate = $<bb>7;      
  LLVMBuildBr(Builder,forupdate);          
  LLVMPositionBuilderAtEnd(Builder,$<bb>10);

//  Pop loop from stack 
  pop_loop();
}
;

expr_opt:		
{
  // If there is not condition then it should make condition true 
  $$ = LLVMConstInt(LLVMIntType(1),1,1); 
}
			| expression
{ 
  $$ = $1;
}
;

jump_stmt:		  RETURN SEMICOLON
{ 
  LLVMBuildRetVoid(Builder);

}
			| RETURN expression SEMICOLON
{
  // If return is constant type just return it in 32 bit type
  int i;
  if (LLVMIsAConstantInt($2)) {
    i =LLVMConstIntGetSExtValue($2);
    LLVMValueRef ret = LLVMConstInt(LLVMInt32Type(),i,0);
    LLVMBuildRet(Builder,ret);
  }
  else {
    // If return is not constant type, just return the value, which will be point in our grammer
    LLVMBuildRet(Builder,$2);
  }

}
;

expression:               assignment_expression
{ 
  $$=$1;
}
;

assignment_expression:    conditional_expression
{
  $$=$1;
}
                        | lhs_expression ASSIGN assignment_expression
{
  // if lhs is pointer type and rhs is constant, update the lhs converting rhs to pointer type 
  if ( LLVMTypeOf($1) == LLVMPointerType(LLVMPointerType(LLVMInt32Type(),0),0)) {
    if (LLVMTypeOf($3) == LLVMInt32Type()) {
      LLVMValueRef IntToPtr = LLVMBuildIntToPtr(Builder,$3,LLVMPointerType(LLVMInt32Type(),0),"");
      LLVMBuildStore(Builder,IntToPtr,$1);
    }
    else {
      // pointer to pointer assignment
      LLVMBuildStore(Builder,$3,$1);
    }
  } 
  else {
    // int to int assignment
    LLVMBuildStore(Builder,$3,$1);
  }
}
;


conditional_expression:   logical_OR_expression
{
  $$=$1;
}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
{
  // Use select command for ? : implelementation
  $$ = LLVMBuildSelect(Builder, $1, $3, $5, "");
}
;

logical_OR_expression:    logical_AND_expression
{
  $$ = $1;
}
                        | logical_OR_expression LOGICAL_OR logical_AND_expression
{
  // lhs or rhs are not 1 bit type then convert it first to 1 bit type
  LLVMValueRef zero1 = LLVMConstInt(LLVMTypeOf($1),0,1);
  LLVMValueRef zero2 = LLVMConstInt(LLVMTypeOf($3),0,1);
  if(LLVMIntType(1) != LLVMTypeOf($1)) {
    LLVMValueRef expr1 = LLVMBuildICmp(Builder, LLVMIntNE, $1, zero1,"");
    if(LLVMIntType(1) != LLVMTypeOf($3)) {
      LLVMValueRef expr2 = LLVMBuildICmp(Builder, LLVMIntNE, $3, zero2,"");
      LLVMValueRef OR = LLVMBuildOr(Builder, expr1, expr2, "");
      $$ = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
    }
    else {
      LLVMValueRef OR = LLVMBuildOr(Builder, expr1, $3, "");
      $$ = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
    }
  }
  else {
    LLVMValueRef OR = LLVMBuildOr(Builder, $1, $3, "");
    $$ = LLVMBuildZExt(Builder, OR, LLVMInt32Type(),"");
  } 
};

logical_AND_expression:   inclusive_OR_expression
{
  $$ = $1;
}
                        | logical_AND_expression LOGICAL_AND inclusive_OR_expression
{
  // lhs or rhs are not 1 bit type then convert it first to 1 bit type
  LLVMValueRef zero1 = LLVMConstInt(LLVMTypeOf($1),0,1);
  LLVMValueRef zero2 = LLVMConstInt(LLVMTypeOf($3),0,1);
  if(LLVMIntType(1) != LLVMTypeOf($1)) {
    LLVMValueRef expr1 = LLVMBuildICmp(Builder, LLVMIntNE, $1, zero1,"");
    if(LLVMIntType(1) != LLVMTypeOf($3)) {
      LLVMValueRef expr2 = LLVMBuildICmp(Builder, LLVMIntNE, $3, zero2,"");
      LLVMValueRef AND = LLVMBuildAnd(Builder, expr1, expr2, "");
      $$ = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
    }
    else {
      LLVMValueRef AND = LLVMBuildAnd(Builder, expr1, $3, "");
      $$ = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
    }
  }
  else {
    LLVMValueRef AND = LLVMBuildAnd(Builder, $1, $3, "");
    $$ = LLVMBuildZExt(Builder, AND, LLVMInt32Type(),"");
  }
}
;

inclusive_OR_expression:  exclusive_OR_expression
{
    $$=$1;
}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
{
  // Binary Operation OR
  $$ = LLVMBuildBinOp(Builder, LLVMOr, $1, $3, "");
}
;

exclusive_OR_expression:  AND_expression
{
  $$ = $1;
}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
{
  // Binary Operation XOR
  $$ = LLVMBuildBinOp(Builder, LLVMXor, $1, $3, "");
}
;

AND_expression:           equality_expression
{
  $$ = $1;
}
                        | AND_expression AMPERSAND equality_expression
{
  // Binary Operation AND
  $$ = LLVMBuildBinOp(Builder, LLVMAnd, $1, $3, "");}
;

equality_expression:      relational_expression
{
  $$ = $1;
}
                        | equality_expression EQ relational_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
                        | equality_expression NEQ relational_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntNE, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
;

relational_expression:    shift_expression
{
    $$=$1;
}
                        | relational_expression LT shift_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSLT, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
                        | relational_expression GT shift_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSGT, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
                        | relational_expression LTE shift_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSLE, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
                        | relational_expression GTE shift_expression
{
  // Converting result in 32 bit type to ensure proper function in some epxression used in parse tree
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntSGE, $1, $3,"");
  $$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(),"");
}
;

shift_expression:         additive_expression
{
    $$=$1;
}
                        | shift_expression LSHIFT additive_expression
{
  // Binary Operation Left Shift
   $$ = LLVMBuildBinOp(Builder, LLVMLShr, $1, $3, "");
}
                        | shift_expression RSHIFT additive_expression
{
  // Binary Operation Right Shift
  $$ = LLVMBuildBinOp(Builder, LLVMShl, $1, $3, "");
}
;

additive_expression:      multiplicative_expression
{
  $$ = $1;
}
                        | additive_expression PLUS multiplicative_expression
{
  // To implement pointer arithmetic using GEP
  if(LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) {
    LLVMValueRef indices[1] = {$3};
    LLVMValueRef gep = LLVMBuildGEP(Builder,$1,indices,1,"");
    $$ = gep;
  }
  else {
    $$ = LLVMBuildAdd(Builder, $1, $3, "");
  }
}
                        | additive_expression MINUS multiplicative_expression
{
  // To implement pointer arithmetic using GEP
  if(LLVMTypeOf($1) == LLVMPointerType(LLVMInt32Type(),0)) {
    LLVMValueRef neg  = LLVMBuildNeg(Builder, $3, "");
    LLVMValueRef indices[1] = {neg};
    LLVMValueRef gep = LLVMBuildGEP(Builder,$1,indices,1,"");
    $$ = gep;
  }
  else {
    $$ = LLVMBuildSub(Builder, $1, $3, "");
  }
}
;

multiplicative_expression:  cast_expression
{
  $$ = $1;
}
                        | multiplicative_expression STAR cast_expression
{
  $$ = LLVMBuildMul(Builder, $1, $3, "");
}
                        | multiplicative_expression DIV cast_expression
{
  $$ = LLVMBuildSDiv(Builder, $1, $3, "");
}
                        | multiplicative_expression MOD cast_expression
{
  $$ = LLVMBuildSRem(Builder, $1, $3, "");
}
;

cast_expression:          unary_expression
{ $$ = $1; }
;

lhs_expression:           ID 
{
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
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
    $$ = val;
  }
}
}
                        | STAR ID
{
  int isArg=0;
  LLVMValueRef val = symbol_find($2,&isArg);
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
     $$ = LLVMBuildLoad(Builder,val,"");
   }
 }
}

;

unary_expression:         postfix_expression
{
  $$ = $1;
}
                        | AMPERSAND primary_expression
{
  if(LLVMIsALoadInst($2)) {
    LLVMValueRef pointer = $2;
    LLVMValueRef address = LLVMGetOperand(pointer,0);
    $$ = address;
  }
  else {
    num_errors++;
    return 0;
  }
}
                        | STAR primary_expression
{
  /* fixme */
  $$ = LLVMBuildLoad(Builder,$2,"");
}
                        | MINUS unary_expression
{
   $$ = LLVMBuildNeg(Builder, $2, "");
  /* implement */
}
                        | PLUS unary_expression
{
  $$ = $2;
}
                        | BITWISE_INVERT unary_expression
{
  $$ = LLVMBuildNot(Builder, $2, "");
}
                        | NOT unary_expression
{
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($2),0,1);
  // Compare with zero
  $$ = LLVMBuildICmp(Builder, LLVMIntEQ, $2, zero,"cond");
  /* Implement */
}
;


postfix_expression:       primary_expression
{
  $$ = $1;
}
;

primary_expression:       ID 
{ 
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  if(val == NULL)
  {
    num_errors++;
    return 0;
  }
  else {
    if (isArg)
      $$ = val;
    else
      $$ = LLVMBuildLoad(Builder,val,"");
  }
}
                        | constant
{
  $$ = $1;
}
                        | LPAREN expression RPAREN
{
  $$ = $2;
}
;

constant:	          NUMBER  
{
  // Saving Number as Const int type 32 bit
  $$ = LLVMConstInt(LLVMInt32Type(),$1,0);   
} 
;

%%

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
