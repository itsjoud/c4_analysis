// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek


/* this code is a minimal compiler for a subset of C, plus a VM to execute the code
it compiles. The entire program is structured to be tiny but still supports
basic C features (types, functions, control flow, expressions)
It can compile itself and then run the resulting bytecode.
*/

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>

/*Note: i did this part so that the code compiles: I undef 'int' and then typedef 'int' as 'long long' so that all integer
references become 64-bit. This is done to ensure that our code runs
uniformly on 64-bit systems or to avoid certain 32-bit integer overflow issues.
*/
#undef int
typedef int rr;

#define int long long //forces all int refrences to be 64 bit



//these should hold the complier's internal state
char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions



/* token enumeration: aka tokens and classes (operators last and in precedence order)

here are the distinct token values that can appear 
The ones from 128 upward are custom tokens 
This includes both keywords (If, Else, While,...) and operators (Add, Sub, Div, ...),
plus some unique tokens for internal use (Num, Fun, Sys).
*/

enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
}; //identifiers and keywords and operators, basically all the possible tokens the simple language can parse

// opcodes
//basically machine instructions for the vm, bytecode
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };
	   
// base data types 
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
//symbol table offsets aka positions in the id array 
//  [Tk]:    token type of this identifier 
//  [Hash]:  hash value used to speed up identifier lookups.
//  [Name]:  points to the name in the original source code (char*).
//  [Class]: storage class aka (Num ,constant), (Fun ,function), (Sys ,system lib), (Glo ,global), (Loc ,local).
//  [Type]:  type of the identifier like CHAR, INT, PTR
//  [Val]:   value of the identifier, which could be an address in memory, a numeric literal, or the function’s entry point.
//  [HClass,HType,HVal]: temporary fields used when we enter a function scope so we can restore them after leaving that scope.



/* The core parts are:
1) next()    -> Tokenizer (lexical analysis).
2) expr()    -> Expression parser (produces VM instructions).
3) stmt()    -> Statement parser (produces VM instructions).
4) main()    -> The driver that handles parsing declarations, building bytecode, 
and then executing them in the built-in VM. */


void next()//a tokenizer or a lexical scanner


/* its whole purpose is that it moves through the source code text pointed to by p,
then decides what token is next tk and sets related fields like ival or id

how?? we skip whitespace and newlines and handle line counting then we detect comments
we also distinguish identifiers + numbers + operators + strings
lastly we fill in either the token type or the numeric value */

// each call to next() moves 'p' forward just past the returned token.
// this code uses minimal error checking so any wrong input can cause problems.

/* in other words, it scans the input stream, sets 'tk' to the next token, 
assigns 'ival' for numeric constants, and points 'id' to the symbol table for identifiers
also processes string literals, character constants, tracks lines, skips comments, 
and updates the symbol table.*/
{
  char *pp; // temporary pointer used for scanning identifiers and string literals

  while (tk = *p) {  // repeatedly read characters from source 'p' and interpret them as tokens.
		++p;// advance pointer in the source
		
		if (tk == '\n') {//newlines are taken care of
		  if (src) {
			// if 'src' flag is set then we print the line we just scanned plus any instructions emitted so far.
			rr length= (rr)(p - lp);
        	printf("%lld: %.*s", (long long)line, length, lp);
			lp = p;
			while (le < e) { //debugging
			  printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
							   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
							   "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
			  if (*le <= ADJ) printf(" %lld\n", *++le); else printf("\n");
			}
		  }
		  ++line;
		}
		
		
		else if (tk == '#') {//preprocessor "#" is taken care of
		  while (*p != 0 && *p != '\n') ++p; // we skip everything until the next newline
		}
		
		
		else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
		  //identifiers (a-z, A-Z, '_') are taken care of
		  pp = p - 1;
		 //hash is computed by multiplying the old hash by 147 and adding the next char and looks up in the symbol table
		  while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
			{tk = tk * 147 + *p++;}
			//we shift the token hash so that it won't clash with short identifiers and encode the length into it as well
		  tk = (tk << 6) + (p - pp);
		  //we look up the symbol
		  id = sym;

			while (id[Tk]) {
				// if the hash and the string match then return that token
				if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
				id = id + Idsz;
			}
		 // if we reach here then it's a new identifier so we store it in the symbol table
		  id[Name] = (int)pp;
		  id[Hash] = tk;
		  tk = id[Tk] = Id; //new identifier token
		  return;
		}
		
		
		else if (tk >= '0' && tk <= '9') {//checks nums such as decimal, hex, octal
		  if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
		  //hexa 
		  else if (*p == 'x' || *p == 'X') {
			while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
			  ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
		  }
		  //octal
		  else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
		  tk = Num;
		  return;
		}
		
		
		else if (tk == '/') {//checks for  '/' in either comment or division token
		  if (*p == '/') {
				++p;
				while (*p != 0 && *p != '\n') ++p;
		    }
		  else {
				//take it as a divition operator
				tk = Div;
				return;
		    }
		}
		
		
		else if (tk == '\'' || tk == '"') {//checks for string or char
		  pp = data;
		    // loop until we hit another quote of the same type
		  while (*p != 0 && *p != tk) {
				if ((ival = *p++) == '\\') {
					// handle escape sequences
					if ((ival = *p++) == 'n') ival = '\n';
				}
				if (tk == '"') *data++ = ival;
		    }

		  ++p;// skip closing quote
		  if (tk == '"') ival = (int)pp; else tk = Num;
		  return;
		}
		
		//basically handles every other operator known such as =, ==, +, ++, -, --, and more
		else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
		else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
		else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
		else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
		else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
		else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
		else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
		else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
		else if (tk == '^') { tk = Xor; return; }
		else if (tk == '%') { tk = Mod; return; }
		else if (tk == '*') { tk = Mul; return; }
		else if (tk == '[') { tk = Brak; return; }
		else if (tk == '?') { tk = Cond; return; }
		else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
	  }
	  //if nothing else, return the single character as a token
}


//---------------------------------------------------------------------------------------------------------------------


void expr(int lev)//Expression Parsing
/*its purpose is that it parses an expression and 
generates bytecode e array to evaluate it

how?
parses a “primary” (a literal, identifier, or subexpression in parentheses).
then repeatedly handle operators in correct precedence order (like +, -, *)
generates instructions like IMM, ADD for the embedded VM
 */
 
{
	/* it parses single factor level tokens like Num, string literal, sizeof, Id*/
  int t, *d;
 // if we've run out of tokens unexpectedly, that's a parse error
  if (!tk) { printf("%lld: unexpected eof in expression\n", line); exit(-1); }
  
  // emit IMM aka an immediate instruction with 'ival'
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  
  else if (tk == '"') {
	 // emit an imm with the address of the string literal in 'data'
		*++e = IMM; *++e = ival; next();
		// if there are consecutive string literals, read them all
		while (tk == '"') next();
		 // align data pointer to int boundary
		data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
	}
	
  else if (tk == Sizeof) {
		next(); 
		if (tk == '(') next(); 
		
		else { printf("%lld: open paren expected in sizeof\n", line); exit(-1); }
		ty = INT; 
		
		if (tk == Int) next(); 
		
		else if (tk == Char) { next(); ty = CHAR; }
		
		while (tk == Mul) { next(); ty = ty + PTR; }
		
		if (tk == ')') next();
		
		else { printf("%lld: close paren expected in sizeof\n", line); exit(-1); }
		// the result of sizeof is either sizeof(char) or sizeof(int)
		*++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
		ty = INT;
	}

   // identifier usage: can be a function call, a variable, or a numeric enum constant
  else if (tk == Id) {
		d = id; next();
		if (tk == '(') {
			  next();
			  // evaluate arguments, push them on stack in reverse order
			  t = 0;
			  
			  while (tk != ')') { 
				  expr(Assign); 
				  *++e = PSH; 
				  ++t; 
				  if (tk == ',') 
					  next(); 
			  }
			  next();
			  // distinguish between system calls (sys) and user functions (fun)
			  if (d[Class] == Sys) *++e = d[Val];
			  
			  else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
			  
			  else { printf("%lld: bad function call\n", line); exit(-1); }
			  // after call, if we pushed arguments, adjust stack to pop them
			  if (t) { *++e = ADJ; *++e = t; }
			  
			  ty = d[Type];
		}
		
		else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
		
		else { // a variable: local or global
			if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
			  
			else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
			  
			else { printf("%lld: undefined variable\n", line); exit(-1); }
			
			*++e = ((ty = d[Type]) == CHAR) ? LC : LI;
		}
	}

	// parenthesized expression or typecast
  else if (tk == '(') {
		next();
		if (tk == Int || tk == Char) {
		  t = (tk == Int) ? INT : CHAR; next();
		  while (tk == Mul) { next(); t = t + PTR; }
		  
		  if (tk == ')') next(); 
		  
		  else { printf("%lld: bad cast\n", line); exit(-1); }
		  expr(Inc);
		  ty = t;
		}
		
		else {
			 expr(Assign);
			 if (tk == ')') next(); 
			 else { printf("%lld: close paren expected\n", line); exit(-1); }
		}
	}
	// unary dereference: *expr
  else if (tk == Mul) {
		next(); expr(Inc);
		if (ty > INT) ty = ty - PTR; else { printf("%lld: bad dereference\n", line); exit(-1); }
		// after dereference, emit load instruction
		*++e = (ty == CHAR) ? LC : LI;
	}
  
  else if (tk == And) {
		next(); expr(Inc);
		// if the top of the bytecode is lc/li, we revert it to an address instead
       // because we want the address, not the loaded value.
		if (*e == LC || *e == LI) --e; else { printf("%lld: bad address-of\n", line); exit(-1); }
		ty = ty + PTR;
	}
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  
  else if (tk == Sub) {
		next(); *++e = IMM;
		// if next token is a literal, we do imm -literal
		if (tk == Num) { *++e = -ival; next(); } 
		
		else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
		ty = INT;
  }
  
  // pre-increment/pre-decrement
  else if (tk == Inc || tk == Dec) {
		t = tk; next(); expr(Inc);
		// must check that the expression is an lvalue
		if (*e == LC) { *e = PSH; *++e = LC; }
		
		else if (*e == LI) { *e = PSH; *++e = LI; }
		
		else { printf("%lld: bad lvalue in pre-increment\n", line); exit(-1); }
		
		*++e = PSH;
		*++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
		*++e = (t == Inc) ? ADD : SUB;
		*++e = (ty == CHAR) ? SC : SI;
  }
  
  
  //basically checks all the options and if none of the above then error out
  else { printf("%lld: bad expression\n", line); exit(-1); }

// now something called precedence climbing is being done where we handle operators with different precedence

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
		t = ty;
		if (tk == Assign) {
		  next();
		  if (*e == LC || *e == LI) *e = PSH; // convert top-of-stack from a load to an address
		  
		  else { printf("%lld: bad lvalue in assignment\n", line); exit(-1); }
		  expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
		}
		else if (tk == Cond) {
		  next();
		  *++e = BZ; d = ++e;
		  expr(Assign);
		   // after computing rhs, store it
		  if (tk == ':') next(); else { printf("%lld: conditional missing colon\n", line); exit(-1); }
		  
		  *d = (int)(e + 3); *++e = JMP; d = ++e;
		  expr(Cond);
		  *d = (int)(e + 1);
		}
		else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
		else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
		else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
		else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
		else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
		else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
		else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
		else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
		else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
		else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
		else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
		else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
		else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
		else if (tk == Add) {
		  next(); *++e = PSH; expr(Mul);
		  if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
		  
		  *++e = ADD;
		}
		else if (tk == Sub) {
		  next(); *++e = PSH; expr(Mul);
		  if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
		  // if the left side was a pointer type, we multiply the right side by sizeof(int)
         // for pointer arithmetic
		  else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
		  
		  else *++e = SUB;
		}
		else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
		
		else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
		
		else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
		
		else if (tk == Inc || tk == Dec) {
			 // we load the value, then do the increment, store it back, then subtract or add
             // the step from the result to get the old value on the top of the stack.
			  if (*e == LC) { *e = PSH; *++e = LC; }
			  
			  else if (*e == LI) { *e = PSH; *++e = LI; }
			  
			  else { printf("%lld: bad lvalue in post-increment\n", line); exit(-1); }
			  
			  *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
			  *++e = (tk == Inc) ? ADD : SUB;
			  *++e = (ty == CHAR) ? SC : SI;
			  *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
			  *++e = (tk == Inc) ? SUB : ADD;
			  next();
		}
		 // array indexing
		else if (tk == Brak) {
			  next(); *++e = PSH; expr(Assign);
			  if (tk == ']') next(); 
			  
			  else { printf("%lld: close bracket expected\n", line); exit(-1); }
			  // pointer offset calculation
			  if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
			  
			  else if (t < PTR) { printf("%lld: pointer type expected\n", line); exit(-1); }
			  
			  *++e = ADD;
			  *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
		}
		 // if we get here, it's an internal error in the parser
		else { printf("%lld: compiler error tk=%lld\n", line, tk); exit(-1); }
	}
}

//---------------------------------------------------------------------------------------------------------------------

void stmt() //statement parsing
/*it parses a single statement like: 
if, while, return, block, or a simple expression statement
generates bytecode for control flow like BZ, JMP 
or for expression evaluation */
{
	  int *a, *b; // used for jump backpatching

	  if (tk == If) {
		  	// parses if(...) statements 
			next();
			if (tk == '(') next(); else { printf("%lld: open paren expected\n", line); exit(-1); }
			
			expr(Assign);  // parse condition
			if (tk == ')') next(); else { printf("%lld: close paren expected\n", line); exit(-1); }
			
			*++e = BZ; b = ++e;
			stmt();
			if (tk == Else) {
			  *b = (int)(e + 3); *++e = JMP; b = ++e;
			  next();
			  stmt();  // parse 'else' clause
			}
			
			*b = (int)(e + 1);
		}
		
	  else if (tk == While) {
			  // parses while  statements 
			next();
			a = e + 1; // mark loop start
			if (tk == '(') next(); else { printf("%lld: open paren expected\n", line); exit(-1); }
			expr(Assign);
			if (tk == ')') next(); else { printf("%lld: close paren expected\n", line); exit(-1); }
			*++e = BZ; // jump if condition == 0
			b = ++e;
			stmt();
			*++e = JMP; 
			*++e = (int)a;
			*b = (int)(e + 1); // patch bz jump target
		}
	  
	  else if (tk == Return) { 
			// parses the return exprsn;
			next();
			if (tk != ';') expr(Assign);
			*++e = LEV;
			if (tk == ';') next(); else { printf("%lld: semicolon expected\n", line); exit(-1); }
		}
	  
	  else if (tk == '{') {
		  // parses compound statements  stmt; stmt; ... 
			next();
			while (tk != '}') stmt();
			next();
		}
	  
	  else if (tk == ';') {
		  // empty statement
			next(); 
		}
		
	  else {
		  // otherwise parses it as an expression statement
			expr(Assign);
			if (tk == ';') next(); else { printf("%lld: semicolon expected\n", line); exit(-1); }
		}
}


//---------------------------------------------------------------------------------------------------------------------


int main(int argc, char **argv)
/* this is technically the vm and has the following phases:
1- initialization: read file, allocate memory
2- parsing the input into an internal bytecode representation in array e
3- setup the stack so it calls main
4- VM Execution: loop reads instructions from pc and executes them
5- EXIT ends the program */
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps
	//parses command-line flags & open file
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

/*allocates memory 
symbol table: sym 
emitted code: e 
data space:   data 
stack space:  sp
*/
  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%lld) symbol area\n", poolsz); return -1; }
  
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%lld) text area\n", poolsz); return -1; }
  
  if (!(data = malloc(poolsz))) { printf("could not malloc(%lld) data area\n", poolsz); return -1; }
  
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%lld) stack area\n", poolsz); return -1; }

	// zero-initialize all these regions
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
	  // insert all keywords into the symbol table
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
 // insert library/system calls into symbol table 
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main
 // now read the actual file into 'p'
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%lld) source area\n", poolsz); return -1; }
  
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %lld\n", i); return -1; }
  
  p[i] = 0;
  close(fd);

  // parse declarations (global)
  line = 1;
  next();
  while (tk) {
		bt = INT; // basetype
		if (tk == Int) next();
		
		else if (tk == Char) { next(); bt = CHAR; }
		
		else if (tk == Enum) {
		  next();
		  if (tk != '{') next();
		  
		  if (tk == '{') {
				next();
				i = 0;
				while (tk != '}') {
				  if (tk != Id) { printf("%lld: bad enum identifier %lld\n", line, tk); return -1; }
				  
				  next();
				  if (tk == Assign) {
						next();
						
						if (tk != Num) { printf("%lld: bad enum initializer\n", line); return -1; }
						
						i = ival;
						next();
					}
					// store the enumerator into symbol table as a num
				  id[Class] = Num; 
				  id[Type] = INT; 
				  id[Val] = i++;
				  
				  if (tk == ',') next();
				}
				next();
			}
		}
		while (tk != ';' && tk != '}') {
			  ty = bt;
			  
			 while (tk == Mul) { next(); ty = ty + PTR; }
			  // we expect an identifier
			  if (tk != Id) { printf("%lld: bad global declaration\n", line); return -1; }
			  // check for redefinition
			  if (id[Class]) { printf("%lld: duplicate global definition\n", line); return -1; }
			  
			  next();
			  id[Type] = ty;
				if (tk == '(') { // function definition
						id[Class] = Fun;
						id[Val] = (int)(e + 1);// the entry point of the function in bytecode
						next(); i = 0;
						// parse function parameters
						while (tk != ')') {
						  ty = INT;
						  if (tk == Int) next();
						  
						  else if (tk == Char) { next(); ty = CHAR; }
						  
						  while (tk == Mul) { next(); ty = ty + PTR; }
						  
						  if (tk != Id) { printf("%lld: bad parameter declaration\n", line); return -1; }
						  // store parameter as a local variable in the symbol table
						  if (id[Class] == Loc) { printf("%lld: duplicate parameter definition\n", line); return -1; }
						  
						  id[HClass] = id[Class]; id[Class] = Loc;
						  id[HType]  = id[Type];  id[Type] = ty;
						  id[HVal]   = id[Val];   id[Val] = i++;
						  next();
						  
						  if (tk == ',') next();
						}
						next();
						
						if (tk != '{') { printf("%lld: bad function definition\n", line); return -1; }
						
						loc = ++i;
						next();
						
						while (tk == Int || tk == Char) {
							  bt = (tk == Int) ? INT : CHAR;
							  next();
							  while (tk != ';') {
									ty = bt;
									while (tk == Mul) { next(); ty = ty + PTR; }
									
									if (tk != Id) { printf("%lld: bad local declaration\n", line); return -1; }
									
									if (id[Class] == Loc) { printf("%lld: duplicate local definition\n", line); return -1; }
									
									id[HClass] = id[Class]; id[Class] = Loc;
									id[HType]  = id[Type];  id[Type] = ty;
									id[HVal]   = id[Val];   id[Val] = ++i;
									next();
									
									if (tk == ',') next();
								}
							  next();
							}
							*++e = ENT; *++e = i - loc;
							while (tk != '}') stmt();
							
							*++e = LEV;
							id = sym; // unwind symbol table locals
							while (id[Tk]) {
							  if (id[Class] == Loc) {
									id[Class] = id[HClass];
									id[Type] = id[HType];
									id[Val] = id[HVal];
								}
							  id = id + Idsz;
							}
					}
				  else {
						// global variable
						id[Class] = Glo;
						id[Val] = (int)data;
						data = data + sizeof(int);
					}
			  if (tk == ',') next();
			}
			next(); // skip ';' or '}'
	}
	//to locate main in the symbol table, get entry point
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  
  if (src) return 0;

  // setup stack
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
  cycle = 0;
  
  
  while (1) { //the VM loop
		i = *pc++; 
		++cycle;// count #instructions for debug
		if (debug) {
		  printf("%lld> %.4s", cycle,
			&"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
			 "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
			 "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
			 
		  if (i <= ADJ) printf(" %lld\n", *pc); else printf("\n");
		}
		
		if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
		else if (i == IMM) a = *pc++;                                         // load global address or immediate
		else if (i == JMP) pc = (int *)*pc;                                   // jump
		else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
		else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
		else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
		else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
		else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
		else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
		else if (i == LI)  a = *(int *)a;                                     // load int
		else if (i == LC)  a = *(char *)a;                                    // load char
		else if (i == SI)  *(int *)*sp++ = a;                                 // store int
		else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
		else if (i == PSH) *--sp = a;                                         // push


		else if (i == OR)  a = *sp++ |  a;
		else if (i == XOR) a = *sp++ ^  a;
		else if (i == AND) a = *sp++ &  a;
		else if (i == EQ)  a = *sp++ == a;
		else if (i == NE)  a = *sp++ != a;
		else if (i == LT)  a = *sp++ <  a;
		else if (i == GT)  a = *sp++ >  a;
		else if (i == LE)  a = *sp++ <= a;
		else if (i == GE)  a = *sp++ >= a;
		else if (i == SHL) a = *sp++ << a;
		else if (i == SHR) a = *sp++ >> a;
		else if (i == ADD) a = *sp++ +  a;
		else if (i == SUB) a = *sp++ -  a;
		else if (i == MUL) a = *sp++ *  a;
		else if (i == DIV) a = *sp++ /  a;
		else if (i == MOD) a = *sp++ %  a;
		
		//sys calls
		else if (i == OPEN) a = open((char *)sp[1], *sp);
		else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
		else if (i == CLOS) a = close(*sp);

		else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
		else if (i == MALC) a = (int)malloc(*sp);
		else if (i == FREE) free((void *)*sp);
		else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
		else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
		// exit the program
		else if (i == EXIT) { printf("exit(%lld) cycle = %lld\n", *sp, cycle); return *sp; }
		// any unrecognized instruction is a VM error
		else { printf("unknown instruction = %lld! cycle = %lld\n", i, cycle); return -1; }
    }
}