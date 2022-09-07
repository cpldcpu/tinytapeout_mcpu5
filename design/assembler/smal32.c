/* smal32.c
   language: C
   copyright 1996 by Douglas W. Jones
                     University of Iowa
                     Iowa City, Iowa  52242
                     USA

   Permission is granted to make copies of this program for any purpose,
   provided that the above copyright notice is preserved in the copy, and
   provided that the copy is not made for direct commercial advantage.

   Note: This software was developed with no outside funding.  If you find
   it useful, and especially if you find it useful in a profit making
   environment, please consider making a contribution to the University
   of Iowa Department of Computer Science in care of:

                     The University of Iowa Foundation
                     Alumni Center
                     University of Iowa
                     Iowa City, Iowa  52242
                     USA
*/


/* Simple macro assembler and linkage editor with a 32 bit word.
   Started by D. W. Jones in the fall 1980 at the University of Iowa.
   rev 4/20/82 includes complete macro and conditional support.
   rev 5/5/82 includes errata and extensions.
   rev 2/5/84 yields 32-bit math, hex conversion.
   rev 12/1/88 to remove NS32000 specific code and run on Berkeley Pascal.
   rev 8/6/96 to translate to C with the aid of the PtoC translator.
   rev 7/27/97 includes command arguments.
   rev 6/11/98 minor bug fix in command line arguments.
   rev 9/04/03 add -P, -U options
   rev 2/10/05 add T directive for 24-bit values
   rev 6/7/11 switch to C-99 stdint.h types, add java style >> vs >>> shifts
   rev 8/14/11 fix macro expansion listing so object code listed with call
   rev 9/9/11 upgrade symbol-table dump format
   rev 8/15/19 fix bug preventing negative relocation offsets, add _ to idents
   rev 9/15/19 make it use EXIT_FAILURE so it works in make scripts

  When you add to the above list, change the define for SMALversion to match

  Originally coded in Hull r-mode Pascal for Prime computers:
    this uses 16 bit integer representations and the char type has
    only 64 elements.
  Works compatably in Hull v-mode Pascal for Prime computers:
    this uses 32 bit integer representations and the char type uses
    ASCII codes with the high bit always set to zero.
  Works in Prime Pascal (v-mode) when keyword 'packed' is deleted:
    this uses 32 bit integer representations and the char type uses
    ASCII codes with the high bit always set to one.
  Works in Berkeley Pascal, after date and time inquiry formats were
    changed from Hull v-mode to make this work.
  as modified by E. Wedel, requires 32-bit integers, but only
    where explicitly called for by "oversized" subrange types.
  as transliterated to C by D. Jones, limited to ASCII and 32 bit integers
*/

/* FILES:
     normal input file suffix -- file.a
     listing file suffix      -- file.l
     object file suffix       -- file.l

   Note that the last dot, if any, in the file name is used
   to strip the suffix off of the input file name in order to
   create the output file names.

   Note also that the input file suffix is not checked.  This is
   useful when using the assembler as a linker, where the input
   files will typically be object files.

   COMMAND LINE OPTIONS:
     -L -- suppresses generation of listing file
     -D -- enable symbol table dump
     -P -- specify the number of lines to list, per page in the listing file
     -U -- specifies an alternate directory to search for USE file inclusion
*/

/* This assembler is written assuming that it will run on a
   machine with at least 32 bit two's complement integers.   */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

/* version control */
#define SMALversion "SMAL32 (rev  9/19)              "
                    /* Note:  The length of this    */
                    /* string MUST NOT change!  It  */
                    /* should give the specific     */
                    /* SMAL version and the date of */
                    /* the most recent revision     */

/* boolean values */
#define true 1
#define false 0
#define boolean int

/* number of symbols allowed in symboltable */
#define symsize         1000

/* opcodes and macro names allowed in optab */
#define opcodes         300

/* chars in stringpool; should be greater than the average symbol length
   times symsize, plus the average opcode or macro name length times
   opcodes, plus additional space for macro text storage, plus additional
   space for the macro call stack, at a minimum of 8 characters per macro
   nesting level, if no parameters are passed.
   note: poolsize must not be increased above 32767 unless appropriate
   changes are made to pushint and popint  */
#define poolsize        30000

/* offset of null symbol in pool */
#define relsym          1

/* offset for abs symbol (just below pool */
#define abssym          0

/* pool delimiter, an illegal character in programs, presently = control-A */
#define pooldel         '\001'

/* replacement for virgin occurrences of -pooldel- in input stream (ha!) */
#define pdelrep         '~'

/* number of files in input file stack */
#define maxgetl         4

/* number of macro parameters per call.
   note: increasing this above 9 will require changes to
   onepass.getlin.getmac and to onepass.macdef.getbody; these currently use
   decimal digits to encode parameter numbers, and must be changed to use
   some other scheme */
#define parmlim         8

/* one more than length of allowed input line */
#define linelen         120

/* last column of listing used for object code info
   note that if objcols is changed to be less than 26, the error messages
   must all be shortened to keep the error markup under the right places
   on the input line.     */
#define objcols         28

/* object code items (bytes,words) listed on a line */
#define listcodes       50

/* field-size allocated to title and sub-title texts */
#define ttlbuflen       28

/* lines per page on the listing device, discounting header and trailer */
#define defaultlinesper  60

/* characters per textual file name */
#define filelen         60

/* break character in file system path names.  used for implementation of USE
   in hierarchic file systems.  it is assumed in procedure insert that path
   names starting with pathbrk are absolute, while those starting with other
   characters are relative */
#define pathbrk         '/'

/* max 32-bit signed positive value 2^31-1*/
#define maxposint       INT32_MAX
#define maxposintdiv10  (maxposint/10)


/* symboltable types */

/* reference to a location in the string pool */
typedef short poolref;

typedef struct value {
	poolref base;   /* symbol from which offset is relocated */
	uint32_t offset; /* offset from that symbol */
} value;

/* types of symbol-value associations */
enum {
	def, lab,   /* is it a label or defined value */
	intdef,     /* is the value to be exported */
	usedyet,    /* has value been used this pass */
	setyet      /* has value been set this pass */
};

/* symbol-value association */
typedef struct association {
	/* fields known only to lookup routine */
	poolref id;

	/* fields known only by table users */
	value val;
	long use; /* a set, one bit per type */
} association;



/* opcode types */
typedef enum {
	operr,   /* error */

	/* assembler directives */
	opb,     /* byte (8 bit) constant */
	oph,     /* half-word (16 bit) constant */
	opt,     /* three-quarter-word (24 bit) constant */
	opw,     /* word (32 bit) constant */
	opascii, /* string constant */
	opif,    /* conditional assembly */
	opelseif,
	opelse,
	opendif,
	opint,   /* internal symbol definition */
	opext,   /* external symbol declaration */
	opcomon, /* common declaration */
	opmac,   /* macro declaration */
	opendm,
	opuse,   /* use text from secondary source */
	oplist,  /* listing control directive */
	operror, /* directive to list this line as an error msg */
	opttl,   /* title directive */
	opsbttl, /* subtitle directive */
	opeject, /* page eject directive */
	opstart, /* starting address specification */
	opend,   /* end of file */

	/* macro call type */
	opmcall  /* macro call */

	/* add machine instruction types here */
} optypes;

/* all textual information stored by the assembler is in the string pool */
static char strpool[poolsize - relsym + 1];

/* two data structures share the string pool: permanent text grows up
   from relsym, the last used location is pointed to by          */
static poolref poolpos;

/* transient text grows down from poolsize, organized as a stack.  the
   format of each stack entry is documented in onepass.pushget           */

/* when the stringpool fills up, it is indicated by:     */
static boolean poolfull;

/* some key-words are stored in the pool but not in any symbol table;
   these are pointed to by the following (constant) variables            */
static poolref funcdf, funcfw, functy, funcab, funcrl, funcle;

/* the assembler symbol table for labels etc */
static association symtab[symsize];
typedef short symptr; /* index type into symtab */

/* flag indicating symbol table is full */
static boolean symfull;

/* the assembler symbol table for opcodes, directives, and macro names */
typedef struct _REC_optab {
	poolref id;
	optypes typ;
	int val, subtyp;
} _REC_optab;
typedef int opptr; /* index type for optab */
static _REC_optab optab[opcodes];


/* flag indicating opcode table is full */
static boolean opfull;

/* textual names of input and output files */
typedef char filename[filelen];
char * usedirectory;
static filename outfile, objfile, dumpfile;
static filename infile[maxgetl];

/* shallow stack of source files */
static FILE *inp[maxgetl];

/* listing output file */
static FILE *o;

/* object code file */
static FILE *obj;

/* object code file */
static FILE *dmp;

/* input line buffer types */
typedef char linebuf[linelen];
typedef int inbufptr; /* index into a line buffer */

/* listing, title and subtitle buffers for listing */
static linebuf listbuf, titlebuf, sbttlbuf;
static inbufptr listlen, titlelen, sbttllen;
static int listlineno;

/* used to determine when to list title */
static int lineonpg;
static int linesper;

/* current page in assembly listing */
static int  pagenum;

/* global, set in -onepass-' routines */
static int errorcount;

/* date and time buffers for listing -- see getdatetime for details */
static char datestr[16];
static char timestr[9];


/* system date/time access routines */

static void getdatetime() {
	char *datetime;
	time_t tloc = time(0);
	datetime = ctime(&tloc);
	
	/* fill datestr with "Mon Jan 16 1973", timestr with "12:00:00" */
	memcpy(&(datestr[0]), &(datetime[0]), 11);
	memcpy(&(datestr[11]), &(datetime[20]), 4);
	datestr[15] = 0;
	memcpy(&(timestr[0]), &(datetime[11]), 8);
	timestr[8] = 0;
}

typedef char string[16];


/* inside smal32, procedures for doing output format conversion */

static void writedec(FILE *f, int v, int l) {
	/* write v as an l digit decimal number on file f */
	char digits[10];
	int i;
	char sign = ' ';

	if (v < 0) {
		v = -v;
		sign = '-';
	}
	i = 0;
	do {
		digits[i] = (v % 10) + '0';
		v = v / 10;
		i++;
	} while (v > 0);
	while ((--l) > i) {
		putc(' ', f);
	}
	putc(sign, f);
	do {
		i--;
		putc(digits[i], f);
	} while (i > 0);
}

static void writehex(FILE *f, uint32_t v, int l) {
	/* write v as an l digit hex number on file f */
	char digits[8];
	char digit;
	int i;

	if (l > 8) {
		for (i = l - 1; i >= 8; i--)
			putc(' ', f);
		l = 8;
	}
	for (i = 0; i < l; i++) {
		digit = v & 15;
		v = v >> 4;
		if (digit < 10)
			digits[i] = digit + '0';
		else
			digits[i] = digit - 10 + 'A';
	}
	for (i = l - 1; i >= 0; i--)
		putc(digits[i], f);
}


static void writesym(FILE *f, poolref pos) {
	/* write the symbol from the symboltable on the indicated file */
	while (strpool[pos - relsym] != pooldel) {
		putc(strpool[pos - relsym], f);
		pos++;
	}
}


static void genval(int siz, uint32_t offset, poolref base) {
	/* generate siz byte value in object file based on indicated
	   symbol with the given offset from that symbol */
	if (base == abssym) {
		putc('#', obj);
		writehex(obj, offset, siz * 2);
	} else {
		if (offset == 0)
			putc(' ', obj);
		else {
			putc('#', obj);
			writehex(obj, offset, 8);
			putc('+', obj);
		}
		putc('R', obj);
		writesym(obj, base);
	}
	putc('\n', obj);
}


/* inside smal32, procedures for symbol table management */

static void clearsym() {
	/* initialize all entries in the symbol table to unused */
	symptr i;
	association *WITH;

	symfull = false;
	for (i = 0; i < symsize; i++) {
		WITH = &symtab[i];
		WITH->id = 0;
		WITH->use = 0;
	}
}


static void clearuse() {
	/* clear symbol table flags in preparation for one pass */
	symptr sym;
	association *WITH;

	for (sym = 0; sym < symsize; sym++) {
		WITH = &symtab[sym];
		WITH->use &= ~((1L << usedyet)
			     | (1L << setyet));
	}
}


static void objsufx() {
	/* generate suffix to object file which defines all internal symbols */
	symptr sym;
	association *WITH;

	for (sym = 0; sym < symsize; sym++) {
		WITH = &symtab[sym];
		if (((1L << intdef) & WITH->use) != 0) {
			putc('R', obj);
			writesym(obj, WITH->id);
			putc('=', obj);
			genval( 4L, WITH->val.offset, WITH->val.base);
		}
	}
}


static poolref putpool(char *s) {
	poolref Result;
	int i;

	poolpos++;
	Result = poolpos;
	for (i = 0; i <= 7; i++) {
		if (s[i] != ' ') {
			strpool[poolpos - relsym] = s[i];
			poolpos++;
		}
	}
	strpool[poolpos - relsym] = pooldel;
	return Result;
}

static int hash(char *s, int modulus) {
	/* it is critical that this hash function match that inside onepass */
	int i;
	int acc;

	acc = 1;
	for (i = 0; i <= 7; i++) {
		if (s[i] != ' ') {
			acc = (acc * 5 + s[i]) % modulus + 1;
		}
	}
	return acc;
}

static void op(char *s, optypes t, int v, int *i) {
	*i = hash(s, opcodes);
	while (optab[(*i) - 1].id != 0) {
		*i = ((*i) % opcodes) + 1;
	}
	optab[*i - 1].id = putpool(s);
	optab[*i - 1].typ = t;
	optab[*i - 1].val = v;
	optab[*i - 1].subtyp = 0;
}


static void opinit() {
	/* initialize the opcode table and string pool (done only once) */
	int i;

	for (i = 1; i <= opcodes; i++)
		optab[i - 1].id = 0;
	poolfull = false;
	opfull = false;
	poolpos = relsym;
	strpool[poolpos - relsym] = pooldel;
	/* null symbol at start of pool is default relocation base */
	op("B       ", opb, 0L, &i);
	op("H       ", oph, 0L, &i);
	op("T       ", opt, 0L, &i);
	op("W       ", opw, 0L, &i);
	op("ASCII   ", opascii, 0L, &i);
	op("IF      ", opif, 0L, &i);
	op("ELSEIF  ", opelseif, 0L, &i);
	op("ELSE    ", opelse, 0L, &i);
	op("ENDIF   ", opendif, 0L, &i);
	op("INT     ", opint, 0L, &i);
	op("EXT     ", opext, 0L, &i);
	op("COMMON  ", opcomon, 0L, &i);
	op("MACRO   ", opmac, 0L, &i);
	op("ENDMAC  ", opendm, 0L, &i);
	op("USE     ", opuse, 0L, &i);
	op("LIST    ", oplist, 0L, &i);
	op("ERROR   ", operror, 0L, &i);
	op("TITLE   ", opttl, 0L, &i);
	op("SUBTITLE", opsbttl, 0L, &i);
	op("PAGE    ", opeject, 0L, &i);
	op("S       ", opstart, 0L, &i);
	op("END     ", opend, 0L, &i);
	/* note: when adding to this list, be sure to adjust the constant
	'opcodes' to reflect the additions; the local procedure
	'op' used above assumes that the opcode table will always
	have some free space.             */

	/* following establish unary function names.. */
	funcdf = putpool("DEF     ");
	funcfw = putpool("FWD     ");
	functy = putpool("TYP     ");
	funcab = putpool("ABS     ");
	funcrl = putpool("REL     ");
	funcle = putpool("LEN     ");
}


/* all kinds of error messages (no more than 31 of them!) */
typedef enum {
	minermsg,
	baddig, /* bad digit in number */
	baddir, /* bad assembly directive */
	badrad, /* bad radix */
	badrel, /* misuse of relocation in expression */
	bounds, /* value out of bounds */
	comexp, /* comma expected */
	fwdref, /* definition must precede use */
	idexp,  /* identifier expected */
	maxuse, /* too many source files */
	baduse, /* cannot open use file */
	miseif, /* missing endif */
	misemc, /* missing endmac */
	misquo, /* missing end quote */
	muldef, /* multiple symbol definition */
	mulstt, /* multiple start directives */
	notfit, /* text too long for line */
	notlab, /* expected label or directive */
	notval, /* expected a value, got something else */
	parexp, /* not a parenthesized list */
	parovf, /* too many macro parameters */
	phase,  /* phase error in label value between passes */
	quoexp, /* quoted string expected */
	unbal,  /* unbalanced parens */
	undef,  /* undefined symbol */
	unfunc, /* bad function */
	erropr, /* error operation */
	unproc, /* unprocessed data at end of line */
	maxermsg
} ermsg;

static char * errormsgs[] = { "---",
/*       12345678901234567890123456   all error messages are 26 chars long */
	"bad digit in number       ",
	"invalid directive         ",
	"bad radix                 ",
	"misuse of relocation      ",
	"value out of bounds       ",
	"comma expected            ",
	"name used before defined  ",
	"symbolic name expected    ",
	"too many use levels       ",
	"cannot open use file      ",
	"missing endif             ",
	"missing endmac            ",
	"missing end quote         ",
	"multiple label definition ",
	"multiple start directives ",
	"text too long for line    ",
	"not a label or directive  ",
	"bad value or expression   ",
	"not a parenthesized list  ",
	"too many macro parameters ",
	"label differed in pass 1  ",
	"quoted string expected    ",
	"unbalanced parentheses    ",
	"undefined symbol          ",
	"invalid function          ",
	"error message             ",
	"comment or eol expected   ",
	"+++"
};

/* types having to do with lexical analysis */
typedef enum {
	id,    /* identifier */
	num,   /* number (hex or decimal) */
	quote, /* quoted string */
	colon, /* : */
	dot,   /* . */
	comma, /* , */
	eq,    /* = */
	gt,    /* > */
	ge,    /* >= */
	sr,    /* >> */
	sru,   /* >>> */
	lt,    /* < */
	le,    /* <= */
	sl,    /* << */
	plus,  /* + */
	minus, /* - */
	notsym, /* \ */
	andsym, /* & */
	orsym, /* ! */
	bpar,  /* ( */
	epar,  /* ) */
	eol,   /* end of line and start of comment */
	junk   /* string of unclassified characters */
} lextypes;

typedef struct lexeme {
	/* start, end of lexeme in line */
	inbufptr pos;
	inbufptr lim;
	lextypes typ;
	union {
		/* value of number */
		uint32_t val;
	} UU;
} lexeme;


typedef int parm;

/* static variables used within onepass and affiliated code: */
static boolean firstpass;     /* true on the first pass only */
static boolean allowlist;     /* true on the final pass only */
static boolean permitlisting; /* is a listing file to be opened */
static boolean symtabdump;    /* is a symboltable dump to be created */

char charclass[256]; /* used to classify characters */
/* bits set in charclass to classify characters */
#define isvalid 1
#define ispunc 2
#define isquote 4
#define isdigit 8
#define isupper 16
#define islower 32
#define isalpha (isupper | islower)
#define isalphanum (isalpha | isdigit)

static void classchar( char* s, char c ) {
	/* in charclass, classify all chars in s as c */
	char ch;
	while ((ch = *s) != 0) {
		charclass[ch] |= c;
		s++;
	}
}

static value loc;    /* current assembly location counter */
static value objloc; /* current object code generation location counter */
static int lineno;   /* current line in assembly source */

static lexeme lex;   /* current lexeme */
static lexeme next;  /* next lexeme for lookahead use */

/* variables associated with stack on transient end of stringpool */
static poolref poolsp; /* pool stack pointer */
static poolref oldsp;  /* pointer to previous frame in stack */
static int actcnt;    /* count of actual params in current frame */
static poolref actparm[parmlim]; /* pointers to ap's in frame */

/* variables controlling source of input */
static poolref gettext; /* loc in pool from which macro text comes */
static int getlevel; /* file from which non-macro text comes */

static linebuf line; /* line buffers and lexical analysis */
static int pos; /* current input line position */
static int ch;  /* line[pos] */
static inbufptr length; /* current input line length */

/* record of errors on the current line and line pending listing*/
static linebuf erbuf, listerbuf;  /* markup under error */
static inbufptr ermax, listermax; /* max used position in erbuf */
static long erset, listerset;     /* set error messages */

/* record of code generated by current line */
typedef struct _REC_codebuf {
	value loc;
	value val;
	int form;
} _REC_codebuf;
static _REC_codebuf codebuf[listcodes];
static int codelen;	/* the number of object code chunks not yet listed */
static value codeloc;   /* the location counter of the first object output */
static int codemark;    /* number of chunks not yet listed from prior lines */

/* listing control variables */
static boolean listing; /* set to (listlevel > 0) and (allowlist) */
static int listlevel;  /* dec on macro call, inc on return */
static int listcol;   /* column in listing */

/* info about last expression */
static value expr;          /* value of expression */
static boolean exprdef;     /* is the value of the expression defined */
static boolean exprundef;   /* does expr. contain fwd ref(s)? */
static inbufptr exprpos, exprlim;    /* position of expression on line */

/* info about opcode decoded on current line */
static optypes optype;
static int opval, opsubt;
static inbufptr oppos, oplim;

/* routines to manage stack in stringpool,
   these routines assume that ord(maxch)>=32; if
   ord(maxch)>32, they may be recoded for greater
   efficiency without any effect on their users   */

static void pushchar(char ch) {
	/* push one char onto stack in stringpool */
	if (poolsp > poolpos) {
		strpool[poolsp - relsym] = ch;
		poolsp--;
	} else
		poolfull = true;
}

static poolref pushtext(inbufptr pos, inbufptr lim) {
	/* push the indicated text onto the stack from the line,
	   as a terminated string, return a reference to the first char */
	inbufptr i;

	pushchar(pooldel);
	for (i = lim - 2; i >= pos - 1; i--)
		pushchar(line[i]);
	return (poolsp + 1);
}

static poolref pushitxt(uint32_t i) {
	/* push the indicated integer as a decimal text string, terminated
	   with a pooldel, and return a reference to the first char */
	pushchar(pooldel);
	if (i == 0) {
		pushchar('0');
		return (poolsp + 1);
	}
	while (i != 0) {
		pushchar((char)((i % 10) + '0'));
		i = i / 10;
	}
}

static char popchar() {
	/* pop one char from stack in stringpool */
	poolsp++;
	return (strpool[poolsp - relsym]);
}

static void pushint(int i) {
	/* push an integer (15 bits) onto stack as a sequence of chars */
	pushchar((char)(i & 31));
	pushchar((char)((i / 32) & 31));
	pushchar((char)(i / 1024));
}

static int popint() {
	/* pop an integer from the stack as a sequence of chars */
	int i;

	i = popchar() * 1024;
	i += popchar() * 32;
	return (i + popchar());
}

static void pushget() {
	/* push a macro expansion control block on the stack */
	int i;

	listlevel--;
	for (i = 0; i < actcnt; i++)
		pushint((long)actparm[i]);
	pushchar(actcnt);
	pushint((long)oldsp);
	pushint((long)gettext);
	pushchar(getlevel);
	oldsp = poolsp;
}

static void popget() {
	/* pop a macro expansion control block from the stack */
	int i;

	if (poolfull) {  /* can't pop safely, so go all the way */
		listlevel = 1;
		gettext = 0;
		getlevel = 0;
		return;
	}
	/* can pop one level safely */
	listlevel++;
	poolsp = oldsp;
	getlevel = popchar();
	gettext = popint();
	oldsp = popint();
	actcnt = popchar();
	for (i = actcnt - 1; i >= 0; i--)
		actparm[i] = popint();
}


/* inside smal32.onepass, input/output routines */

static void errmsg(ermsg msg, inbufptr pos, inbufptr lim) {
	/* record error and position of error in line for later listing */
	inbufptr i;
	long msgbit = 1L << msg;
	
	if (!(erset & msgbit)) { /* first occurance, this error on this line */
		erset |= msgbit;
		if (allowlist) {
			errorcount++;   /* for posterity's sake */

			/* also gripe to stderr */
			writedec(stderr, lineno, 6);
			fputs("  ", stderr);
			fputs(errormsgs[ msg ], stderr);
			{
				int l = length;
				if ((l + 26) > 79) l = 79 - 26;
				for (i = 0; i < l; i++) {
					putc(line[i], stderr);
				}
			}
			putc('\n', stderr);
		}
	}
	if (allowlist) { /* record underlining of error */
		listing = allowlist;   /* force error line to be listed */
		for (i = ermax; i <= pos - 2; i++)
			erbuf[i] = ' ';
		for (i = pos - 1; i <= lim - 2; i++)
			erbuf[i] = '=';
		if (lim > ermax + 1)
			ermax = lim - 1;
	}
}


/* routines used by getlin */

static void makeend() {
	/* put an end directive on the line as result of end file */
	line[0] = 'E';
	line[1] = 'N';
	line[2] = 'D';
	length = 3;
	line[3] = ';';   /* char beyond end must be initialized */
}

static void getmac() {
	/* get one line of text from the string pool copy of macro body */
	int parmnum;
	poolref _parm;
	int i;
	char ch;

	i = 0;
	do {
		/* copy literal text from pool to line */
		ch = strpool[gettext - relsym];
		gettext++;
		while (ch != pooldel && i < linelen - 1) {
			i++;
			line[i - 1] = ch;
			ch = strpool[gettext - relsym];
			gettext++;
		}

		if (ch != pooldel) {  /* isn't space in the line */
			errmsg(notfit, 0, 0);
			while (strpool[gettext - relsym] != pooldel)
				gettext++;
			gettext++;
		}

		ch = strpool[gettext - relsym]; /* char after pooldel */
		gettext++;
		if (charclass[ch] & isdigit) {  /* macro param */
			parmnum = ch - '0';
			if (parmnum <= actcnt) {  /* param exists */
				_parm = actparm[parmnum - 1];
				if (_parm > 0) {  /* param is nonblank */
					ch = strpool[_parm - relsym];
					while (ch != pooldel && i < linelen - 1)
					{   /* loop copying text of parameter */
						i++;
						line[i - 1] = ch;
						_parm++;
						ch = strpool[_parm - relsym];
					}
					if (ch != pooldel)
						errmsg(notfit, 0, 0);
				}
			}
			ch = ' ';   /* force loop to continue */
		}
	} while ((ch != pooldel) && (ch != ','));
	if (ch == pooldel) {
		makeend();
	} else {
		length = i;
		line[i] = ';';   /* this char must be initialized */
	}
}

static void get(FILE *f) {
	/* read one line from appropriate input file */
	int i;
	int ch;

	i = 0;
	for (;;) {
		ch = getc(f);
		if ((ch == EOF) || (ch == '\n')) break;
		if (i >= (linelen - 1)) break;
		if (ch == pooldel)
			ch = pdelrep;
		if (ch == '\t')	{
			do {
				line[i] = ' ';
				i++;
			} while (((i & 7) != 0) && (i < (linelen - 1)));
		} else {
			line[i] = ch;
			i++;
		}
	}
	if (i >= (linelen - 1))
		errmsg(notfit, 0, 0);
	if ((ch == EOF) & (i == 0)) {
		makeend();
	} else {
		length = i;
		line[i] = ';';   /* this char must be initialized */
	}
}

static void getlin() {
	/*  read one line from the input file, initialize the
	    lexical analysis and listing control variables    */

	erset = 0;
	ermax = 0;
	if (gettext > 0) {
		getmac();
	} else {
		/* lines are only counted at the bottom level! */
		if (getlevel == 1) lineno++;
		get(inp[getlevel - 1]);
	}
	pos = 1;
	lex.typ = eol;
	lex.pos = 1;
	lex.lim = 1;
	next.typ = eol;
	next.pos = 1;
	next.lim = 1;
}

static void settitle(char *buf, inbufptr *len, inbufptr pos, inbufptr lim) {
	/* fill the indicated title buffer with the indicated part
		 of the input line,
		 suppress trailing blanks and quotes around string */
	inbufptr i;

	*len = 0;
	while (line[lim-1] == ' ') {
		lim--;
	}
	if ((line[pos-1] == '"') && (line[lim-1] == '"')) {
		pos++;
		lim--;
	}
	if (lim - pos > ttlbuflen - 3) {
		lim = pos + ttlbuflen - 3;
	}
	for (i = pos - 1; i < lim; i++) {
		buf[*len] = line[i];
		(*len)++;
	}
}

static void newpage() {
	/* force listing to the start of a new page */
	inbufptr i;

	if (!permitlisting) return;
	if (lineonpg <= 1) return;
	putc('\f', o);   /* formfeed character */
	/* note that instead of using a formfeed, this routine
	    could be written to simply output blank lines and
	    count them with lineonpg until the count was high
	    enough to push the listing to a new page      */
	lineonpg = 4;
	pagenum++;

	/* write title to listing file */
	fputs(SMALversion, o);
	for (i = 0; i < titlelen; i++)
		putc(titlebuf[i], o);
	for (i = titlelen; i <= ttlbuflen; i++)
		putc(' ', o);
	fputs(timestr, o);
	fputs("  Page ", o);
	writedec(o, pagenum, 1);
	putc('\n', o);   /* linefeed character */
	fputs("                                ", o);
	for (i = 0; i < sbttllen; i++)
		putc(sbttlbuf[i], o);
	for (i = sbttllen; i <= ttlbuflen; i++)
		putc(' ', o);
	fputs( datestr, o );
	putc('\n', o);   /* linefeed character */
	putc('\n', o);
}

static void markline() {
	/*  mark start of processing one line
	    all object code generated before this call is
	    associated with a previous line and will be
	    flushed to the listing before this line is
	    listed, if it ever is */
	codemark = codelen;
}

static void printobjectsnippet() {
	/* print as much object code as will fit on this line
	   only called when there is object code needing printing */

	value listloc; /* current listing location counter */
	int i; /* index into codebuf */
	int j; /* secondary index into codebuf */

	{ /* first, print location */
		_REC_codebuf *WITH;
		WITH = &codebuf[0];
		if (WITH->loc.base == abssym) {
			putc(' ', o);
		} else {
			putc('+', o);
		}
		writehex(o, WITH->loc.offset, 8L);
		putc(':', o);
		listcol = 11;
		listloc = WITH->loc;
	}
	i = 0; /* index of next item to print */

	do { /* list at least one code item, as many as will fit */
		int nextcol; /* look-ahead to column on output listing */
		_REC_codebuf *WITH;
		WITH = &codebuf[i];

		/* guarantee:  This does not exit loop on first iteration */
		if ((listloc.base != WITH->loc.base)
		||  (listloc.offset != WITH->loc.offset)) break;

		nextcol = listcol + (WITH->form * 2) + 2;
		/* guarantee:  This does not exit loop on first iteration */
		if (nextcol > (objcols + 1)) break;
		
		j = i; /* record index of last listed code */
		if (WITH->val.base != abssym) {
			putc('+', o);
		} else {
			putc(' ', o);
		}
		writehex(o, WITH->val.offset, WITH->form * 2);
		putc(' ', o);
		listcol = nextcol;
		listloc.offset += WITH->form;
		i++;
	} while (i < codemark);
	/* codebuf[0..j] has been printed */

	/* shift codebuf[j+1..codelen] down to [0.. ], adjust codemark, len */
	j++;    /* j is now index of first item not yet printed */
	{
		int k = j; /* number of items being shifted */
		i = 0;  /* i is index to which to shift items */
		while (j < codelen) {
			codebuf[i] = codebuf[j];
			i++;
			j++;
		}
		codemark = codemark - k;
		codelen = codelen - k;
	}
}

static void printlistbuf() {
	/* list line stored in linebuf */
	if (permitlisting) {
		int i; /* loop index */
		lineonpg++;
		if (lineonpg >= linesper) newpage();
		listcol = 1;
		if (codemark > 0) {  /* list generated code */
			printobjectsnippet();
			/* the above shifts unprinted codes down */
		}
		for (; listcol <= objcols; listcol++) putc(' ', o);
		writedec(o, listlineno, 6);
		putc(' ', o);
		putc(' ', o);
		for (i = 0; i < listlen; i++) putc(listbuf[i], o);
		putc('\n', o);
	}
	listlen = -1; /* indicate that line has been listed */
}

static void printerrbuf() {
	/* list accumulated error messages
	   only called if there are some */
	ermsg msg; /* error message number */
	int col;   /* column on listing */
	int i;     /* for loop index for printing string */

	if (permitlisting) {
		for (msg = minermsg; msg <= maxermsg; msg++) {
			if (((1L << msg) & listerset) != 0) {
				/* list one error msg */
				lineonpg++;
				if (lineonpg > linesper) newpage();
				fputs( errormsgs[ msg ], o );
				for (col = 26; col <= objcols+7; col++) {
					putc(' ', o);
				}
				for (i = 0; i < listermax; i++) {
					putc(listerbuf[i], o);
				}
				listermax = 0; /* underline only once */
				putc('\n', o);
			}
		}
	}
	listerset = 0; /* suppress further listing of errors */
}

static void printobject() {
	/* list object code from codebuf up to codemark */
	if (permitlisting) {
		lineonpg++;
		listcol = 1;
		if (codemark > 0) {  /* list generated code */
			printobjectsnippet();
			/* the above shifts unprinted codes down */
			putc('\n', o);
		}
	} else {
		codemark = 0;
	}
}

static void flushlist() {
	/* flush out any incomplete listing of prior lines */

	if (listlen >= 0) { /* list previous line */
		printlistbuf();
	}

	if (listerset != 0) { /* error messages that need listing */
		printerrbuf();
	}

	while (codemark > 0) { /* additional object code needs listing */
		printobject();
	}
}

static void listline() {
	/* commit to listing this line, some code for which may
	    already have been generated */
	flushlist();

	/* now copy linebuf to listbuf */
	for (listlen = 0; listlen < length; listlen++) {
		listbuf[listlen] = line[listlen];
	}
	listlineno = lineno;

	/* now copy errors to listerrors */
	for (listermax = 0; listermax < ermax; listermax++) {
		listerbuf[listermax] = erbuf[listermax];
	}
	listerset = erset;
}

static void putobj(int form, uint32_t offset, poolref base) {
	/* store value (offset,base) in current loc.
	   (base=abssym is used for absolute values; typically, relocatable
	   values will come from loc.base and loc.offset or expr.base and
	   expr.offset.)
	   use format form (.25, .5 or 1 word), save information for listing */

	if (allowlist) {
		/* first assure that code gets loaded in right loc */
		if (objloc.offset != loc.offset ||
		    objloc.base != loc.base) {
			objloc = loc;
			fputs(".=", obj);
			genval( 4L, loc.offset, loc.base);
		}
		/* now objloc = loc */

		/* save appropriate listing data */
		if (codelen == 0)
			codeloc = loc;
		if (codelen < listcodes) {
			codebuf[codelen].loc.offset = loc.offset;
			codebuf[codelen].loc.base = loc.base;
			codebuf[codelen].val.offset = offset;
			codebuf[codelen].val.base = base;
			codebuf[codelen].form = form;
			codelen++;
		}
		/* then generate correct object code */
		switch (form) {
			case 1: putc('B', obj); break;
			case 2: putc('H', obj); break;
			case 3: putc('T', obj); break;
			case 4: putc('W', obj); break;
		}
		genval( form, offset, base);
		objloc.offset = objloc.offset + (uint32_t)form;
	}

	loc.offset = loc.offset + (uint32_t)form;
}

static void putascii(inbufptr pos, inbufptr lim) {
	/* generate object code for ascii string */
	inbufptr i;
	for (i = pos; i < lim; i++) {
		putobj(1L, line[i - 1], abssym);
	}
}



/* procedures used only by nextlex */

static uint32_t number10() {
	uint32_t acc; /* accumulates the value */
	uint32_t maxdiv10 = UINT32_MAX / 10;
	unsigned int digit; /* the value of one digit */
	boolean ok; /* is it OK so far? */

	/* assume initially that ch is a valid digit */
	acc = 0;
	ok = true;
	do {
		if (charclass[ch] & isdigit) digit = ch - '0';

		pos++;
		ch = line[pos - 1];
			
		/* acc = (acc * 10) + digit; but detect overflows */
		if (acc <= maxdiv10) {
			acc = acc * 10;
			if (acc <= (UINT32_MAX - digit)) {
				acc = acc + digit;
			} else {
				errmsg(bounds, next.pos, pos);
				ok = false;
			}
		} else {
			errmsg(bounds, next.pos, pos);
			ok = false;
		}
	} while ((charclass[ch] & isdigit) && ok);
	if (!ok) {
		while (charclass[ch] & isdigit) { /* skip rest of number */
			pos++;
			ch = line[pos - 1];
		}
		return 0;
	} else {
		return (acc);
	}
}

static uint32_t number(int radix) {
	uint32_t acc; /* accumulates the value */
	uint32_t maxdivradix = UINT32_MAX / radix;
	int digit; /* the value of one digit */
	boolean ok; /* is it OK so far? */

	/* assume initially that ch is a valid digit */
	acc = 0;
	ok = true;
	do {
		if (charclass[ch] & isdigit)
			digit = ch - '0';
		else if (charclass[ch] & isupper)
			digit = (ch - 'A') + 10;
		else
			digit = (ch - 'a') + 10;

		if (digit >= radix) {
			errmsg(baddig, pos, pos + 1);
			ok = false;
		} else {
			pos++;
			ch = line[pos - 1];
			
			/* acc = (acc * radix) + digit; but detect overflows */
			if (acc <= maxdivradix) {
				acc = acc * radix;
				if (acc <= (UINT32_MAX - digit)) {
				 	acc = acc + digit;
				} else {
					errmsg(bounds, next.pos, pos);
					ok = false;
				}
			} else {
				errmsg(bounds, next.pos, pos);
				ok = false;
			}
		}
	} while ((charclass[ch] & isalphanum) && ok);
	if (!ok) {
		while (charclass[ch] & isalphanum) { /* skip rest of number */
			pos++;
			ch = line[pos - 1];
		}
		return 0;
	} else {
		return (acc);
	}
}


/* inside smal32.onepass, lexical analysis routines */

static void nextlex() {
	/* save the next lexeme information in the current lexeme
	   variable, then read a new next one from the input line */

	lex = next;
	while (line[pos - 1] == ' ') pos++;
	ch = line[pos - 1];
	next.pos = pos;
	if (ch == ';')
		next.typ = eol;
	else if (charclass[ch] & ispunc) {
		switch (ch) {   /* case */

		case ':': next.typ = colon; break;
		case '.': next.typ = dot; break;
		case ',': next.typ = comma; break;
		case '=': next.typ = eq; break;
		case '>': next.typ = gt; break;
		case '<': next.typ = lt; break;
		case '+': next.typ = plus; break;
		case '-': next.typ = minus; break;
		case '\\': next.typ = notsym; break;
		case '~': next.typ = notsym; break;
		case '&': next.typ = andsym; break;
		case '!': next.typ = orsym; break;
		case '|': next.typ = orsym; break;
		case '(': next.typ = bpar; break;
		case ')': next.typ = epar; break;

		}
		pos++;
	} else if (charclass[ch] & isdigit) {
		next.typ = num;
		next.UU.val = number10();
		if (ch == '#') {
			if (next.UU.val > 36 || next.UU.val < 2) {
				next.UU.val = 36;
				errmsg(badrad, next.pos, pos);
			}
			pos++;
			ch = line[pos - 1];
			if (charclass[ch] & isalphanum)
				next.UU.val = number(next.UU.val);
			else
				errmsg(baddig, next.pos, pos);
		}
	} else if (charclass[ch] & isalpha) {
		next.typ = id;
		do {
			pos++;
			ch = line[pos - 1];
		} while (charclass[ch] & isalphanum);
	} else if (ch == '#') {
		pos++;
		ch = line[pos - 1];
		if (charclass[ch] & isalphanum) {
			next.typ = num;
			next.UU.val = number(16L);
		} else   /* if */
			next.typ = junk;
	} else if (charclass[ch] & isquote) {
		char mark = ch;
		next.typ = quote;
		do {
			pos++;
		} while (line[pos - 1] != mark && pos <= length);
		if (pos <= length)
			pos++;
		else
			errmsg(misquo, next.pos, next.pos + 1);
	} else {
		do {
			pos++;
			ch = line[pos - 1];
		} while ((charclass[ch] & isvalid) == 0);
			next.typ = junk;
	}
	next.lim = pos;

	/* invalid lexeme */
}

static void startup() {
	/* setup for processing one line of input */
	getlin(); /* read input line */
	markline(); /* tell listing module this is start of new line */
	nextlex(); /* read first lexeme */
	nextlex(); /* read second lexeme (allow lookahead) */

	/* start parsing by looking for valid start of line */
	while ( (lex.typ != id) &&
		(lex.typ != eol) &&
		(lex.typ != dot)
	) {
		errmsg(notlab, lex.pos, lex.lim);
		nextlex();
	}
}

/* inside smal32.onepass, string pool and symbol table management */

static void putch(char ch) {
	/* put one char into permanent end of stringpool */
	if (poolsp > poolpos) {  /* there is room in pool */
		poolpos++;
		strpool[poolpos - relsym] = ch;
	} else  /* there isn't room */
		poolfull = true;
}

static poolref putpool_(inbufptr pos, inbufptr lim) {
	/* put the string between pos and lim-1 on the current line into
		 the string pool, returning it's index in the pool.  the string
		 delimiter is appended to the string in the pool.  it is assumed
		 that the string will fit (the caller must guarantee this)    */
	poolref Result;
	inbufptr i;

	poolpos++;
	Result = poolpos;
	for (i = pos - 1; i <= lim - 2; i++) {
		strpool[poolpos - relsym] = line[i];
		poolpos++;
	}
	strpool[poolpos - relsym] = pooldel;
	return Result;
}

static boolean poolfit(inbufptr pos, inbufptr lim) {
	/* check to see if text between pos and lim will fit in stringpool */
	/* poolfit */
	return (poolsp - poolpos > lim - pos);
}

static boolean poolcmp(poolref poolpos, inbufptr pos, inbufptr lim) {
	/* compare the string starting at poolpos in the stringpool with
		 that between pos and lim on the current line, return true if
		 they are the same; this relies on the fact that the string
		 delimiter in the stringpool will never occur in the line   */
	while (strpool[poolpos - relsym] == line[pos - 1]) {
		poolpos++;
		pos++;
	}
	return (strpool[poolpos - relsym] == pooldel && pos == lim);
}

static int hash_(inbufptr pos, inbufptr lim, int modulus) {
	/* compute hash of lexeme between pos and lim,
		 return a value between 1 and modulus (inclusive)
		 this hash function must match that in "opinit"  */
	int acc;
	inbufptr p;

	acc = 1;
	for (p = pos - 1; p <= lim - 2; p++) {
		acc = (acc * 5 + line[p]) % modulus + 1;
	}
	return acc;
}

static symptr lookup(inbufptr pos, inbufptr lim) {
	/* find the symbol between pos and lim on the current line in
	   the symbol table, return the index into the table where it
	   was found or inserted; if it could not be inserted, return
	   zero      */
	symptr Result, s, olds;
	association *WITH;

	s = hash_(pos, lim, (long)symsize);
	olds = s;
	Result = 0;   /* default return value */
	do {
		WITH = &symtab[s - 1];
		if (WITH->id != 0) {   /* with */
			if (poolcmp(WITH->id, pos, lim)) {
				Result = s;
				s = olds;   /* terminate loop */
			} else {
				if (s < symsize) s++;
				else s = 1;
				if (s == olds) symfull = true;
			}
		} else {  /* found unused table entry */
			if (poolfit(pos, lim)) {
				/* put the symbol in the pool and table */
				WITH->id = putpool_(pos, lim);
				Result = s;
			} else  /* no room in pool for sym */
				poolfull = true;
			s = olds;   /* terminate loop */
		}
	} while (s != olds);
	return Result;
}

static void symdump() {
        /* dump entire contents of symbol table */
        symptr i;
        association *WITH;

        for (i = 0; i < symsize; i++) {
                WITH = &symtab[i];
                if (WITH->id != 0) {  /* have nonblank entry */
                        if ((WITH->val.base == WITH->id)
                        &&  (WITH->val.offset == 0)) {
                                /* external symbol -- see makeext() */
                                fputs("EXT\t", dmp);
                                writesym(dmp, WITH->id);
                        } else {
                                /* not an external symbol */
                                putc('\t', dmp); /* sort field delimiter */
                                writesym(dmp, WITH->id);
                                putc('\t', dmp); /* sort field delimiter */
                                putc('=', dmp);
                                /* could have just called genval() but
                                   we do it locally to get sortable format */
                                if (WITH->val.base != abssym) {
                                        /* relocatable values */
                                        if (WITH->val.base == relsym) {
                                                fputs("REL(0)", dmp);
                                        } else {
                                                writesym(dmp, WITH->val.base);
                                        }
                                        if (WITH->val.offset != 0) {
                                                putc('+', dmp);
                                                putc('#', dmp);
                                                writehex(dmp,
                                                         WITH->val.offset, 8);
                                        }
                                } else {
                                        /* absolute values */
                                        putc('#', dmp);
                                        writehex(dmp, WITH->val.offset, 8);
                                }
                        }
                        putc('\n', dmp);
                }
        }
}

static opptr oplookup(inbufptr pos, inbufptr lim) {
	/* find the symbol between pos and lim on the current line in
		 the opcode table, return the index into the table where it
		 was found or should be put; return 0 if it isn't found and
		 the table is full         */
	opptr Result, s, olds;
	_REC_optab *WITH;

	s = hash_(pos, lim, (long)opcodes);
	olds = s;
	Result = 0;   /* default return value */
	do {
		WITH = &optab[s - 1];
		if (WITH->id != 0)   /* with */
		{  /* have nonblank entry */
			if (poolcmp(WITH->id, pos, lim)) {  /* found it */
				Result = s;
				s = olds;   /* terminate loop */
			} else {
				if (s < opcodes)
					s++;
			else
				s = 1;
			}
		} else {  /* found vacancy */
			Result = s;
			s = olds;   /* terminate loop */
		}
	} while (s != olds);
	return Result;
}


/* inside smal32.onepass, utility parsing procedures */

static void getcomma() {
	/* skip the comma, complain if there isn't one */
	if (lex.typ == comma)
		nextlex();
	else
		errmsg(comexp, lex.pos, lex.lim);
}

static void skipbal() {
	/* skip to maching end paren when given begin paren */
	int nest;
	lexeme par;

	/* assert lex.typ = bpar */
	nest = 1;
	par = lex;
	do {
		nextlex();
		if (lex.typ == bpar)
			nest++;
		else if (lex.typ == epar)
			nest--;
	} while (nest >= 1 && lex.typ != eol);
	if (lex.typ == eol)
		errmsg(unbal, par.pos, par.lim);
}

static void expression();

static void value_() {
	/* parse values of an expression of the form
	    <value> ::= <ident> ! <num> ! . ! ( <expression> )
		      ! <string> ! <identifier> ( <argument> )
	   return the value of the value in expr */
	symptr symbol;
	lexeme op, par;
	int i;

	association *WITH;

	exprpos = lex.pos;
	exprlim = lex.lim;
	exprundef = false;
	if (lex.typ == num) {  /* got a  + 1number */
		expr.offset = lex.UU.val;
		expr.base = abssym;
		exprdef = true;   /* read over number */
		nextlex();
	} else if (lex.typ == quote) {
		expr.base = 0;
		expr.offset = 0;
		exprdef = true;
		i = lex.lim - lex.pos;
		if (i > 6)
			errmsg(bounds, lex.pos, lex.lim);
		else if (i > 2) {
			int FORLIM = lex.lim - lex.pos - 3;
			for (i = 0; i <= FORLIM; i++)
				expr.offset = expr.offset * 256 +
					line[lex.pos + i];
		}
		nextlex();
	} else if ((lex.typ == id) && (next.typ == bpar)) {

		/*  do a named (unary) function  */
		op = lex;
		nextlex();   /* skip operator name */
		par = lex;
		nextlex();   /* skip opening paren */
		expr.base = abssym;
		expr.offset = 1;   /* default to ambiguous */
		exprdef = true;   /* default to defined */

		if (poolcmp(funcdf, op.pos, op.lim)) {
			if (lex.typ == id)   /* skip operand */
				symbol = lookup(lex.pos, lex.lim);
			else {
				symbol = 0;
				errmsg(idexp, lex.pos, lex.lim);
			}
			nextlex();
			if (symbol != 0)
				expr.offset =
			-(((1L<<((long)setyet)) & symtab[symbol - 1].use) != 0);
		} else if (poolcmp(funcfw, op.pos, op.lim)) {
			if (lex.typ == id)   /* skip operand */
				symbol = lookup(lex.pos, lex.lim);
			else {
				symbol = 0;
				errmsg(idexp, lex.pos, lex.lim);
			}
			nextlex();
			if (symbol != 0)
				expr.offset =
			-(((1L<<((long)usedyet)) & symtab[symbol - 1].use)!=0 &&
			  ((1L<<((long)setyet)) & symtab[symbol - 1].use)==0);
		} else if (poolcmp(functy, op.pos, op.lim)) {
			expression();
			expr.offset = expr.base;
			expr.base = abssym;
		} else if (poolcmp(funcab, op.pos, op.lim)) {
			expression();
			expr.base = abssym;
		} else if (poolcmp(funcrl, op.pos, op.lim)) {
			expression();
			if (expr.base == abssym) {
				expr.base = relsym;
			} else {
				errmsg(badrel, exprpos, exprlim);
			}
		} else if (poolcmp(funcle, op.pos, op.lim)) {
			if (lex.typ == epar)
				expr.offset = 0;
			else {
				expr.offset = lex.pos;
				if (lex.typ == bpar)
					skipbal();
				while ((next.typ != eol)&&(next.typ != epar)) {
					nextlex();
					if (lex.typ == bpar)
					  skipbal();
				}
				expr.offset = lex.lim - expr.offset;
				nextlex();
			}
		} else {
			errmsg(unfunc, op.pos, op.lim);
			while ((lex.typ != epar) && (lex.typ != eol)) nextlex();
		}
		if (lex.typ == epar) {
			exprpos = op.pos;
			exprlim = lex.lim;
			/* skip end paren */
			nextlex();
		} else {
			errmsg(unbal, par.pos, par.lim);
		}

	} else if (lex.typ == id) {
		/*  ah, just an ordinary identifier..  */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0) {   /* read over identifier */
			WITH = &symtab[symbol - 1];
			WITH->use |= 1L << usedyet;
			if (((1L << def) & WITH->use) != 0 ||
			   ((1L << lab) & WITH->use) != 0)
			{
				expr = WITH->val;
				exprdef = true;
				exprundef = (((1L << setyet) & WITH->use) == 0);
			} else {
				errmsg(undef, lex.pos, lex.lim);
				expr.offset = 0;
				expr.base = abssym;
				exprdef = false;
			}
		} else {   /* if */
			expr.offset = 0;   /* no err on full table */
			expr.base = abssym;
			exprdef = true;   /* pretend it's defined */
		}
		nextlex();

	} else if (lex.typ == dot) {
		expr = loc;
		exprdef = true;   /* read over dot */
		nextlex();

	} else if (lex.typ == bpar) {
		par = lex;
		nextlex();
		expression();
		if (lex.typ == epar) {
			exprpos = par.pos;
			exprlim = lex.lim;
			nextlex();
		} else
			errmsg(unbal, par.pos, par.lim);

	} else {
		/* got something else */
		errmsg(notval, lex.pos, lex.lim);
		expr.offset = 0;
		expr.base = abssym;
		exprdef = false;
		if (((1L << lex.typ) & ((1L << epar) |
		 (1L << eol) | (1L << comma))) == 0)
		/* read over whatever it is */
			nextlex();
	}

	exprundef = (!exprdef || exprundef);
}

static void term() {
	/* parse terms of an expression of the form
			    <term> ::= [ <unary> ] <value>
		 return the value of the term in expr */
	/* unary operator */
	lexeme op;

	if (((1L << lex.typ) & ((1L << plus) |
	 (1L << minus) | (1L << notsym))) == 0)
	{  /* unary */
		value_();
		return;
	}
	op = lex;   /* read over unary operator */
	nextlex();   /* get value to be modified */
	value_();
	exprpos = op.pos;
	switch (op.typ) {   /* case */

	case plus:
		/* blank case */
		break;

	case minus:
		if (expr.base == abssym)
			expr.offset = (~expr.offset) + 1;
		else
			errmsg(badrel, op.pos, op.lim);
		break;

	case notsym:
		if (expr.base == abssym)
			expr.offset = ~expr.offset;
		else
			errmsg(badrel, op.pos, op.lim);
		break;
	}

	/* no unary operator */
}



/* inside smal32.onepass, procedures to parse expressions */

static void expression() {
	/* parse expressions of the form
	    <expression> ::= <term> ! <expression> <binop> <term>
	   return the value of the expression in expr */
	value acc;        /* the accumulator */
	boolean accdef;   /* is the accumulator defined */
	boolean accundef; /* does acc. have fwd ref(s)? */
	lexeme op;        /* what operator was found */
	inbufptr expos;   /* position of start of expression */

	/* get leading term */
	term();
	acc = expr;
	accdef = exprdef;
	accundef = exprundef;
	expos = exprpos;   /* save pos for error handlers */
	while ( (lex.typ == plus) ||
		(lex.typ == minus) ||
		(lex.typ == gt) ||
		(lex.typ == lt) ||
		(lex.typ == eq) ||
		(lex.typ == andsym) ||
		(lex.typ == orsym)
	) {   /* while loop processing terms */
		op = lex;   /* skip over operator */
		nextlex();
		if ( (op.typ == lt) && (lex.typ == eq) ) { /* <= */
			op.lim = lex.lim; /* so error msgs underline both */
			op.typ = le;
			nextlex();
		} else if ( (op.typ == lt) && (lex.typ == lt) ) { /* << */
			op.lim = lex.lim; /* so error msgs underline both */
			op.typ = sl;
			nextlex();
		} else if ( (op.typ == gt) && (lex.typ == eq) ) { /* >= */
			op.lim = lex.lim; /* so error msgs underline both */
			op.typ = ge;
			nextlex();
		} else if ( (op.typ == gt) && (lex.typ == gt) ) { /* >> */
			op.lim = lex.lim;
			nextlex();
			if (lex.typ == gt) { /* >>> */
				op.lim = lex.lim;
				op.typ = sru;
				nextlex();
			} else {
				op.typ = sr;
			}
		}
		/* get following term */
		term();
		if (!exprdef) {
			accdef = false;
			continue;
		}
		switch (op.typ) {   /* case */

		case plus:   /* plus */
			acc.offset = acc.offset + expr.offset;
			if (acc.base == abssym)
				acc.base = expr.base;
			else if (expr.base != abssym) {
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
			}
			break;

		case minus:   /* minus */
			acc.offset = acc.offset + (~expr.offset) + 1;
			if (acc.base == expr.base)
				acc.base = abssym;
			else if (expr.base != abssym) {
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
			}
			break;

		case gt:
		case ge:
		case lt:
		case le:
		case eq:
			if (acc.base == expr.base) {
				int32_t a = *((int32_t *)&acc.offset);
				int32_t b = *((int32_t *)&expr.offset);
				switch (op.typ) {   /* case */

				case gt:
					acc.offset = (uint32_t)(-(a > b));
					break;

				case ge:
					acc.offset = (uint32_t)(-(a >= b));
					break;

				case lt:
					acc.offset = (uint32_t)(-(a < b));
					break;

				case le:
					acc.offset = (uint32_t)(-(a <= b));
					break;

				case eq:
					acc.offset = (uint32_t)(-(a == b));
					break;
				}
				acc.base = abssym;
			} else {   /* gt,lt,eq */
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
				acc.offset = 1;   /* neither true nor false */
			}
			break;

		case andsym:
		case orsym:
			if (acc.base == abssym && expr.base == abssym) {
				switch (op.typ) {   /* case */

				case andsym:
					acc.offset = acc.offset & expr.offset;
					break;

				case orsym:
					acc.offset = acc.offset | expr.offset;
					break;
				}
			} else   /* andsym,orsym */
				errmsg(badrel, op.pos, op.lim);
			break;

		case sl:
		case sr:
		case sru:
			if (acc.base == abssym && expr.base == abssym) {
				if (expr.offset > 32)
					expr.offset = 32;
				while (expr.offset > 0) {
				    switch (op.typ) {

				    case sl:
					acc.offset = acc.offset << 1;
					break;

				    case sr: /* signed shift */
					acc.offset = (uint32_t)
						(((int32_t)acc.offset) >> 1);
					break;

				    case sru: /* unsigned shift */
					acc.offset = acc.offset >> 1;
					break;
				    }
				    expr.offset--;
				}
			} else {
				errmsg(badrel, op.pos, op.lim);
			}  /* shift operators */
			break;
		}
	}
	expr = acc;
	exprdef = accdef;
	exprundef = (exprundef || accundef || !exprdef);
	exprpos = expos;
}

static void expresbal() {
	/* evaluate expressions, assuring balanced parens */
	expression();
	while (lex.typ == epar) {
		errmsg(unbal, lex.pos, lex.lim);
		nextlex();
	}
}

static void boundval( uint32_t *v, int32_t min, int32_t max ) {
	/* check to see v is between min and max */
	if ((((int32_t)*v) < min) || (((int32_t)*v) > max)) {
		errmsg( bounds, exprpos, exprlim );
		*v = (uint32_t)min;
	}
}

static boolean predicate() {   /* evaluate expression */
	/* evaluate predicates for if and elseif directives */
	boolean Result;

	expresbal();
	Result = false;   /* default */
	if (expr.base == abssym) {
		if (expr.offset == -1)
			return true;
	} else
		errmsg(badrel, exprpos, exprlim);
	return Result;
}


/* inside smal32.onepass, processing of key syntactic elements */

static void labeldef() {
	/* parse label definition; define label and handle multiples */
	symptr symbol;

	/* assume that (lex.typ = id) and (next.typ = colon) */
	symbol = lookup(lex.pos, lex.lim);
	if (symbol > 0) {  /* the symbol is in the table */
		association *WITH = &symtab[symbol - 1];
		if (((1L << setyet) & WITH->use) != 0) {
			errmsg(muldef, lex.pos, lex.lim);
		} else if (((1L << lab) & WITH->use) != 0) {
			if (WITH->val.offset != loc.offset ||
			    WITH->val.base != loc.base) {
				errmsg(phase, lex.pos, lex.lim);
			}
		} else {
			WITH->val = loc;
		}
		WITH->use |= (1L << lab) | (1L << setyet);
	}
	nextlex();   /* read over id */
	nextlex();   /* read over colon */
}

static void definition() {
	/* parse and process definition of form <id> = <expression> */
	symptr symbol;
	association *WITH;

	symbol = lookup(lex.pos, lex.lim);
	if (((1L << lab) & symtab[symbol - 1].use) != 0)
		errmsg(muldef, lex.pos, lex.lim);
	nextlex();   /* read over id */
	nextlex();   /* read over eq */
	expresbal();
	if (symbol <= 0)
		return;
	WITH = &symtab[symbol - 1];
	WITH->use |= 1L << setyet;
	if (exprdef) {
		WITH->use |= 1L << def;
		WITH->val = expr;
	}
}

static void origin() {
	/* parse and process definitions of the form . = <expression> */
	nextlex();   /* skip dot */
	nextlex();   /* skip eq */
	expresbal();
	loc = expr;
}

static void opcode() {
	/*  parse opcode field, including detection of data width
		specifying suffices.  Resultant info is returned via globals
		optype, opval, opsubt, opsufi, opsufi2, opsuff and opsufcc. */
	opptr i;

	oppos = lex.pos;
	oplim = lex.lim;
	optype = operr;

	i = oplookup(oppos, oplim);
	if (i != 0) {
		if (optab[i - 1].id != 0) {
			_REC_optab *WITH = &optab[i - 1];
			optype = WITH->typ;
			opval = WITH->val;
			opsubt = WITH->subtyp;
		}
	}

	nextlex();
}


static void getop() {
	/* skip and ignore labels on a line, return the opcode globally
	   suppress any error messages encountered in parsing the line
	   used in scanning macro bodies and conditional assembly */
	startup();

	while (lex.typ == id && next.typ == colon) {   /* skip id */
		nextlex();   /* skip colon */
		nextlex();
	}
	if (lex.typ == id) {
		if (next.typ == eq)
			optype = operr;
		else
			opcode();
	} else
		optype = operr;
	erset = 0;
}


/* inside smal32.onepass, processing of external symbol linkages */

static void internl() {
	/* parse and process internal symbol definitions */
	symptr symbol;

	if (lex.typ == id) {   /* read over internal symbol name */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0) {
			association *WITH = &symtab[symbol - 1];
			WITH->use |= 1L << intdef;
			if ((WITH->use & ((1L << def) | (1L << lab))) == 0)
			/* if with */
			  errmsg(undef, lex.pos, lex.lim);
		}
	} else
		errmsg(idexp, lex.pos, lex.lim);
	nextlex();
}

static void makeext(symptr symbol) {
	/* make or verify that the current symbol is external */
	/* used only from externl and comdef */

	association *WITH = &symtab[symbol - 1];
	if (((1L << setyet) & WITH->use) != 0)
		errmsg(muldef, lex.pos, lex.lim);
	else if (((1L << usedyet) & WITH->use) != 0)
		errmsg(fwdref, lex.pos, lex.lim);
	else if (((1L << lab) & WITH->use) != 0) {
		if (WITH->val.base != WITH->id || WITH->val.offset != 0)
			errmsg(muldef, lex.pos, lex.lim);
	} else {
		WITH->val.base = WITH->id;
		WITH->val.offset = 0;
	}
	WITH->use |= (1L << lab) | (1L << setyet);   /* with */

	/* symbol previously unused */
}

static void externl() {
	/* parse and process external symbol declarations */
	symptr symbol;

	if (lex.typ == id) {   /* read over external symbol name */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0)
			makeext(symbol);
	} else
		errmsg(idexp, lex.pos, lex.lim);
	nextlex();
}

static void comdef() {
	/* parse and process common declarations */
	symptr symbol;

	if (lex.typ != id) { /* common name missing */
		errmsg(idexp, lex.pos, lex.lim);
		return;
	}
	symbol = lookup(lex.pos, lex.lim);
	if (symbol > 0) {
		association *WITH = &symtab[symbol - 1];
		makeext(symbol);
		nextlex();    /* read over common name */
		getcomma();   /* get common size or maximum location */
		expresbal();
		if (expr.base != abssym && expr.base != WITH->id) {
			errmsg(badrel, exprpos, exprlim);
			return;
		}
		expr.offset += expr.offset & 1; /* halfword align size */
		expr.offset += expr.offset & 2; /* word align size */
		if (!allowlist)
			return;
		fputs("IF\\DEF(S", obj);
		writesym(obj, WITH->id);
		fputs(")\n S", obj);
		writesym(obj, WITH->id);
		fputs("=#", obj);
		writehex(obj, expr.offset, 4L);
		fputs("\nENDIF\nIF\\DEF(R", obj);
		writesym(obj, WITH->id);
		fputs(")\n R", obj);
		writesym(obj, WITH->id);
		fputs("=C\n C=C+S", obj);
		writesym(obj, WITH->id);
		fputs("\n CT=.\n .=C\n .=CT\nENDIF\n",obj);
		return;
	}
	nextlex(); /* skip over name */
	getcomma();
	expresbal(); /* skip over size */
	/* symbol table full */
}


/* inside smal32.onepass, processing of text insertions */

static void insert_() {
	/* process use directive (push one input file on stack) */
	int i;
	inbufptr pos;
	int startpos;

	if (lex.typ != quote) {
		errmsg(quoexp, lex.pos, lex.lim);
		nextlex();
		return;
	}
	if (getlevel >= maxgetl) {
		errmsg(maxuse, lex.pos, lex.lim);
		nextlex();
		return;
	}

	/* put together the file name */
	pos = lex.pos + 1;
	startpos = 1;
	if (line[pos - 1] != pathbrk) {  /* relative path */
		memcpy(infile[getlevel], infile[getlevel - 1],
			sizeof(filename));
		for (i = 1; i < filelen; i++) {
			if (infile[getlevel][i - 1] == pathbrk)
			startpos = i + 1;
		}
	}
	for (i = startpos - 1; i < filelen; i++) {
		if (pos < lex.lim - 1) {
			infile[getlevel][i] = line[pos - 1];
			pos++;
		} else
			infile[getlevel][i] = 0;
	}

	/* try to open the file */
	if (inp[getlevel] != NULL) {
		inp[getlevel] = freopen(infile[getlevel], "r", inp[getlevel]);
	} else {
		inp[getlevel] = fopen(infile[getlevel], "r");
	}

	/* if couldn't open, and if relative path, try default use directory */
	if ((inp[getlevel] == NULL)
	&&  (line[pos - 1] != pathbrk)
	&&  (usedirectory != NULL)) {
		strncpy(infile[getlevel], usedirectory, sizeof(filename));
		strncat(infile[getlevel], "/", sizeof(filename));
		pos = lex.pos + 1;
		startpos = strlen(infile[getlevel]) + 1;
		for (i = startpos - 1; i < filelen; i++) {
			if (pos < lex.lim - 1) {
				infile[getlevel][i] = line[pos - 1];
				pos++;
			} else
				infile[getlevel][i] = 0;
		}
		inp[getlevel] = fopen(infile[getlevel], "r");
	}

	if (inp[getlevel] == NULL) {  /* see if it worked */
		errmsg(baduse, lex.pos, lex.lim);
	} else {
		/* if it worked, save current source */
		pushget();
		getlevel++;
		gettext = 0; /* set source to file, not macro */
		actcnt = 0;  /* set to no macro parameters */
	}
	nextlex();
}

/* static variables for macdef: */
static poolref parmtab[parmlim];
static int parms;
static poolref _oldsp;

static void getpar() {
	/* parse one formal parameter of form:
			  <param> := <id>  !  ( <id> )  !  = <id>
		 corresponding to by name, by list of names, and by value */
	/* the parameter identifier */
	lexeme _parm;
	/* the position of the begin paren */
	lexeme par;
	/* the type of the formal parameter */
	char typ = ' ';

	if (parms < parmlim) {   /* read over parameter */
		if (lex.typ == id) {  /* name parameter */
			_parm = lex;
			typ = 'a';
		} else if (lex.typ == eq) {
			nextlex();
			if (lex.typ == id) {
				typ = '=';
				_parm = lex;
			} else {
				_parm.typ = eol;
				errmsg(idexp, lex.pos, lex.lim);
			}
		} else if (lex.typ == bpar) {
			par = lex;   /* hold onto position for errors */
			/* skip over paren */
			nextlex();
			if (lex.typ == id) {
				typ = '(';
				_parm = lex;
				nextlex(); /* skip over identifier */
				if (lex.typ != epar)
					errmsg(unbal, par.pos, par.lim);
			} else {
				_parm.typ = eol;
				errmsg(idexp, lex.pos, lex.lim);
			}
		} else {
			_parm.typ = eol;
			errmsg(idexp, lex.pos, lex.lim);
		}
		if (_parm.typ == id) {  /* have a good param */
			parms++;
			if (firstpass) {
				parmtab[parms-1] =
					pushtext(_parm.pos, _parm.lim);
				putch(typ);
			}
		}
	} else {
		errmsg(parovf, lex.pos, lex.lim);
	}
	nextlex();
}


static parm lookup_(inbufptr pos, inbufptr lim) {
	/* lookup candidate formal parameter name */
	parm Result, i;

	Result = 0;
	i = 0;
	while (i < parms) {
		i++;
		if (poolcmp(parmtab[i - 1], pos, lim)) {
			Result = i;
			i = parms;
		}
	}
	return Result;
}

static void getbody() {
	/* parse macro body of form: <body> ::= <linesequence> endmac */
	int nest;          /* counter to find right endmac when nested */
	inbufptr pos, lim; /* progress pointers for parameter scan */
	parm parmnum;      /* identity of current parameter */

	nest = 1;
	do {
		if (listing) listline();
		getop();
		if (optype == opmac)
			nest++;
		else if (optype == opendm)
			nest--;
		else if (optype == opend)
			nest = 0;
		if (nest > 0 && firstpass) {  /* save text */
			pos = 1;
			while (pos <= length) {
				while (!(charclass[line[pos - 1]] & isalpha)
				       && (pos <= length)
				) {
					/* copy non identifier text */
					if (line[pos - 1] == '\'') {
						/* squeeze out quote marks */
						pos++;
						/* assert line[length+1]=';' */
						while (line[pos - 1] == '\'') {
						    putch('\'');
						    pos++;
						}
					} else {
						/* pass through simple text */
						putch(line[pos - 1]);
						pos++;
					}
				}
				if (pos > length) break;
				/* found an identifier */
				lim = pos;
				while (charclass[line[lim - 1]] & isalphanum)
					lim++;
				/* length of identifier known */
				parmnum = lookup_(pos, lim);
				if (parmnum > 0) {
					/* identifier is a macro formal parm */
					putch(pooldel);
					putch(parmnum + '0');
				} else {
					/* identifier is just text */
					for (pos--; pos <= lim - 2; pos++)
						putch(line[pos]);
				}
				pos = lim;
			}
			putch(pooldel);
			putch(',');
		}
	} while (nest >= 1);
	poolsp = _oldsp; /* pop formal parameter table from stack */
	if (optype == opend) {  /* found end of file, not endm */
		errmsg(misemc, 0, 0);
		popget();
	}
	if (firstpass) {
		putch(pooldel);   /* two pooldel's in a row end a macro */
		putch(pooldel);
	}
}

static void macdef() {
	/* process macro definitions of the form:
	   macro <name> [ <param> [ , <param> ]* ] <body>          */
	opptr m;

	parms = 0;
	_oldsp = poolsp;   /* mark stack top, allowing temporary use */
	if (lex.typ == id) {  /* have macro name */
		m = oplookup(lex.pos, lex.lim);
		if (m <= 0) {  /* no room in table */
			opfull = true;
		} else if (firstpass) {
			if (optab[m - 1].id == 0) {  /* first definition */
			    if (poolfit(lex.pos, lex.lim)) {
				optab[m - 1].id = putpool_(lex.pos, lex.lim);
				optab[m - 1].typ = opmcall;
				optab[m - 1].val = poolpos + 1;
			    }
			} else {  /* redefinition */
				optab[m - 1].typ = opmcall;
				optab[m - 1].val = poolpos + 1;
			}
		}
		nextlex();   /* skip over macro name */
		if (lex.typ != eol) {
			getpar();
			while (lex.typ != eol) {
				getcomma();
				getpar();
			}
		}
		if (firstpass)   /* mark end of parmtypes */
			putch(pooldel);

	} else {  /* missing macro name */
		errmsg(idexp, lex.pos, lex.lim);   /* skip over junk */
		nextlex();
	}
	if (lex.typ != eol && erset == 0)
		errmsg(unproc, lex.pos, lex.lim);
	getbody();
}

/* static variables for maccall: */
static poolref _poolpos;

static void getpar_() {
	/* parse a parameter of the form:
	    <param> ::= [ <lexeme> ]*
		     !  <expression>
		     !  ( [ <lexeme> ]* ) [ : <expr> [ : <expr> ] ]  */
	char typ;     /* indicates expected parameter type */
	inbufptr pos; /* location of parameter */
	inbufptr lim; /* position info for begin paren */
	lexeme par;

	typ = strpool[_poolpos - relsym];
	_poolpos++;
	actcnt++;
	if (((1L << lex.typ) &
	    ((1L << comma) | (1L << eol))) != 0)
	{  /* parameter missing */
		actparm[actcnt - 1] = 0;
		return;
	}
	if (typ == 'a') {
		pos = lex.pos;
		lim = lex.lim;
		while (((1L << lex.typ) &
		       ((1L << eol) | (1L << comma))) == 0) {
			if (lex.typ == bpar)
				skipbal();
			else if (lex.typ == epar)
				errmsg(unbal, lex.pos, lex.lim);
			lim = lex.lim;
			nextlex();
		}
		actparm[actcnt - 1] = pushtext(pos, lim);
		return;
	}
	if (typ == '=') {
		expresbal();
		/* recall that text is pushed backwards */
		pushitxt(expr.offset); /* end with decimal offset */
		if (expr.base != abssym) {
			/* add in correction for relocatable symbols */
			pushchar('+');
			if (expr.base == relsym) {
				/* actual param is REL(0)+offset */
				pushchar(')');
				pushchar('0');
				pushchar('(');
				pushchar('L');
				pushchar('E');
				pushchar('R');
				actparm[actcnt - 1] = poolsp + 1;
			} else {
				/* actual param is external+offset */
				short int i = expr.base - relsym;
				short int j;
				for (j = i; strpool[j] != pooldel; j++);
				for (j--; j >= i; j--) {
					pushchar(strpool[j]);
				}
			}
		}
		actparm[actcnt - 1] = poolsp + 1;
		return;
	}
	if (typ != '(')
		return;
	if (lex.typ != bpar) {
		errmsg(parexp, lex.pos, lex.lim);
		nextlex();
		return;
	}
	par = lex;   /* skip paren at list head */
	nextlex();
	pos = lex.pos;
	lim = pos;   /* default for empty string */
	while (((1L << lex.typ) &
	       ((1L << eol) | (1L << epar))) == 0) {
		if (lex.typ == bpar)
			skipbal();
		lim = lex.lim;
		nextlex();
	}
	if (lex.typ == eol)
		errmsg(unbal, par.pos, par.lim);
	else
		nextlex();
	if (lex.typ == colon) {  /* substring */
		nextlex();
		if (lex.typ != colon) {   /* get start */
			expresbal();
			if (expr.base != abssym)
				errmsg(badrel, exprpos, exprlim);
			if (expr.offset > 1) {
				if (expr.offset > lim - pos)
					pos = lim;
				else
					pos += expr.offset - 1;
			}
		}
		if (lex.typ == colon) {
			nextlex();   /* get length */
			expresbal();
			if (expr.base != abssym)
				errmsg(badrel, exprpos, exprlim);
			if (expr.offset < lim - pos) {
				if (expr.offset < 1)
					lim = pos;
				else
					lim = pos + expr.offset;
			}
		}
	}
	if (pos >= lim)
		actparm[actcnt - 1] = 0;
	else
		actparm[actcnt - 1] = pushtext(pos, lim);
}

static void maccall(poolref poolpos_) {
	/* call macro who's text is stored at poolpos in string pool;
		 form of call is:
		 <name> [ <param> [ , <param> ]* ]      */

	/* save previous macro expansion status block */
	_poolpos = poolpos_;
	if (poolfull) {
		/* stop bad calls */
	} else {
		pushget();
		actcnt = 0;
		while (strpool[_poolpos-relsym] != pooldel && lex.typ != eol) {
			getpar_();
			if (lex.typ != eol) getcomma();
		}
		if (lex.typ != eol)
			errmsg(parovf, lex.pos, lex.lim);
		while (strpool[_poolpos - relsym] != pooldel)
			_poolpos++;
		gettext = _poolpos + 1;   /* set to read macro text from pool */
	}
}


/* inside smal32.onepass, processing of conditional directives */

static void findend() {
	/* skip over lines until a line with endif or end opcode found */
	int nest;

	nest = 0;
	do {   /* read and check following lines */
		/* first list previous line */
		if (listing) listline();

		getop();
		if (optype == opif)
			nest++;
		else if (optype == opendif)
			nest--;
	} while (optype != opend && nest >= 0);
	if (optype == opend) {
		errmsg(miseif, 0, 0);
		popget();
	}
}

static void findelse() {
	/* skip lines until one with an else, elseif <true>, or end found */
	do {   /* read over then parts (allowing multiple elseif's) */
		if (lex.typ != eol && erset == 0)
			errmsg(unproc, lex.pos, lex.lim);
		do {
			/* list line to be skipped */
			if (listing) listline();
			getop();
			while (optype == opif) {
				findend();
				/* list end */
				if (listing) listline();
				if (optype == opend)
					optype = opendif;
					/* don't complain twice */
				else
					getop();
			}
		} while (((1L << optype) &
			 ((1L << opend) | (1L << opendif) |
			  (1L << opelse) | (1L << opelseif))) == 0);
		if (optype == opelseif) {
			if (predicate())
				optype = opelse;
		}
	} while (((1L << optype) & ((1L << opend) |
		  (1L << opendif) | (1L << opelse))) == 0);
	if (optype == opend) {
		errmsg(miseif, 0, 0);
		popget();
	}
}


/* inside smal32, main assembly procedure for one pass through source */

static void onepass() {
	/*  perform one assembly pass.  produce output if listing=true  */
	/* max val of loc.offset when loc.base = relsym */
	uint32_t maxrel;
	/* starting address, if specified */
	value startloc;
	/* detects multiple start dirs */
	boolean seenstart;

	poolsp = poolsize;
	gettext = 0;
	getlevel = 0;
	oldsp = 0;
	actcnt = 0;   /* put a dummy level on the stack to allow clean end */
	pushget();
	getlevel = 1;   /* setup for reading at normal source level */
	listlevel = 1;   /* setup so it will list only main level source */
	lineonpg = linesper;   /* force leading page-eject */
	codelen = 0;    /* no code yet accumulated for listing */
	sbttllen = 0;	/* no subtitle on first page-eject */
	lineno = 0;	/* no lines have been read */
	listlen = -1;	/* there is no line to list */
	errorcount = 0;
	loc.offset = 0;
	loc.base = relsym;
	maxrel = 0;
	objloc = loc;
	seenstart = false;   /* setup symbol table for pass */
	clearuse();

	while (getlevel >= 1) {   /* process one line per iteration */
		listing = (listlevel > 0 && allowlist);
		startup(); /* setup for processing a line */

		while (lex.typ == id && next.typ == colon) {
			labeldef();
		}

		/* now know that if lex.typ = id, then next.typ <> colon */
		if (lex.typ == id) {
			if (next.typ == eq)  /* process definitions */
				definition();
			else {   /* if */
				opcode();
/* ---------------------------- */
	switch (optype) { /* opcode class */

	case operr:
		errmsg(baddir, oppos, oplim);
		break;

	case opb:  /* 8 bit byte */
		expresbal();
		boundval(&expr.offset, -128L, 255L);
		putobj(1L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			boundval(&expr.offset, -128L, 255L);
			putobj(1L, expr.offset, expr.base);
		}
		break;

	case oph:  /* 16 bit half-word */
		expresbal();
		boundval(&expr.offset, -32768L, 65535L);
		putobj(2L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			boundval(&expr.offset, -32768L, 65535L);
			putobj(2L, expr.offset, expr.base);
		}
		break;

	case opt:  /* 24 bit three-quarter-word */
		expresbal();
		boundval(&expr.offset, -8388608L, 16777215L);
		putobj(3L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			boundval(&expr.offset, -8388608L, 16777215L);
			putobj(3L, expr.offset, expr.base);
		}
		break;

	case opw:  /* 32 bit word */
		expresbal();
		putobj(4L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			putobj(4L, expr.offset, expr.base);
		}
		break;

	case opascii: /* ascii string */
		if (lex.typ != quote) {
			errmsg(quoexp, lex.pos, lex.lim);
		} else {
			putascii(lex.pos + 1, lex.lim - 1);
		}
		nextlex();
		while (lex.typ != eol) {
			getcomma();
			if (lex.typ == quote) {
				putascii(lex.pos + 1, lex.lim - 1);
				nextlex();
			} else {
				expresbal();
				boundval(&expr.offset, -128L, 255L);
				putobj(1L, expr.offset, expr.base);
			}
		}
		break;
		
	case opif:
		if (!predicate())
			findelse();
		break;

	case opelse:
	case opelseif:
		findend();
		break;

	case opendif:
		/* blank case */
		break;

	case opint:  /* internal definition */
		internl();
		while (lex.typ == comma) {
			nextlex();
			internl();
		}
		break;

	case opext:  /* external definition */
		externl();
		while (lex.typ == comma) {
			nextlex();
			externl();
		}
		break;

	case opcomon:   /* common definition */
		comdef();
		break;

	case opmac:   /* process macro definition */
		macdef();
		break;

	case opendm:
		errmsg(baddir, oppos, oplim);
		break;

	case opuse:   /* insert text from alt file */
		insert_();
		break;

	case oplist:  /* control listing */
		expresbal();
		if (expr.base != abssym)
			errmsg(badrel, exprpos, exprlim);
		else {
			listlevel += expr.offset;
			if (expr.offset < 0 && erset == 0)
			  listing = (listlevel > 0 && allowlist);
		}
		break;

	case operror: /* force listing as error msg */
		lex.typ = eol;  /* ignore rest of this line */
		errmsg(erropr, 0, 0);
		break;

	case opttl:  /* title directive */
		settitle(titlebuf, &titlelen, lex.pos, length);
		lex.typ = eol;
		break;

	case opsbttl:
		if (firstpass) listline();
		if (allowlist) {
			settitle(sbttlbuf, &sbttllen, lex.pos, length);
		}
		if (listing) {
			flushlist();
			newpage();
		}
		lex.typ = eol;
		break;

	case opeject:
		if (listing)
			lineonpg = linesper - 1;
		break;

	case opstart:
		if (!seenstart) {  /* set start addr */
			seenstart = true;
			expresbal();
			startloc = expr;
		} else  /* multiple start addr's */
			errmsg(mulstt, oppos, oplim);
		break;

	case opend:
		popget();
		break;

	case opmcall:   /* call the indicated macro */
		maccall((int)opval);
		break;
	}
/* ---------------------------- */
			}
		} else if (lex.typ == dot && next.typ == eq)
			origin();
		else if (lex.typ != eol) {
			errmsg(baddir, lex.pos, lex.lim);
			nextlex(); /* skip over junk to avoid extra errors */
		}
		if (lex.typ != eol && erset == 0)
			errmsg(unproc, lex.pos, lex.lim);
		if (listing)
			listline();
		if (loc.base == relsym) {
			if (loc.offset > maxrel) maxrel = loc.offset;
		}
	}
	flushlist(); /* deal with any partially unlisted line */

	if (!allowlist) return;

	/* make sure relocatable size is aligned on 32 bit boundary */
	maxrel += maxrel & 1;  /* halfword align */
	maxrel += maxrel & 2;  /* word align */
	/* make sure object code ends at maxrel */
	if (loc.base != relsym || loc.offset != maxrel) {
		fputs(".=", obj);
		genval( 4L, maxrel, relsym);
	}
	/* put starting address out */
	if (seenstart) {
		putc('S', obj);
		genval( 4L, startloc.offset, startloc.base);
	}
	if (!permitlisting) return;

	/* note error count at end of listing */
	if (errorcount == 0) {
		fputs("                    no errors\n", o);
		return;
	} else {
		fputs("    ", o);
		if (errorcount == 1)
			fputs(" 1 error", o);
		else {
			fputs("    ", o);
			writedec(o, errorcount, 1);
			fputs(" errors", o);
		}
		fputs(" in this assembly\n", o);
	}
}


/* inside smal32, procedures to do file setup and takedown */

static void getfiles(int argc, char *argv[]) {
	/*  get file name from command line, store it in global "infile"
	    suffix it with ".l" for "outfile", ".o" for "objfile"
	    and ".d" for "dumpfile" */
	int dot, i;

	infile[0][0] = 0;

	/* setup defaults for command line options */
	permitlisting = true;
	symtabdump = false;
	linesper = defaultlinesper;
	usedirectory = NULL;

	/* process command line options */
	for (i = 1; i < argc; i++) { /* for each argument */
		if (argv[i][0] == '-') { /* command line options */
			if ((argv[i][1] == 'L') && (argv[i][2] == '\0')) {
				permitlisting = false;
			} else if ((argv[i][1] == 'D')&&(argv[i][2] == '\0')) {
				symtabdump = true; 
			} else if ((argv[i][1] == 'P')&&(argv[i][2] == '\0')) {
				i++;
				if (i < argc) {
					linesper = atol( argv[i] );
					if (linesper <= 5) {
						fputs( "** bad -P option **\n",
							stderr);
						exit(EXIT_FAILURE);
					}
				} else {
					fputs( "** missing -P argument **\n",
						stderr);
					exit(EXIT_FAILURE);
				}
			} else if ((argv[i][1] == 'U')&&(argv[i][2] == '\0')) {
				i++;
				if (i < argc) {
					usedirectory = argv[i];
				} else {
					fputs( "** missing -U argument **\n",
						stderr);
					exit(EXIT_FAILURE);
				}
			} else {
				fputs( "** invalid command line argument: ",
					stderr);
				fputs( argv[i], stderr );
				fputs( " ** \n"
					"   for " SMALversion "\n"
					"   valid options are\n"
					"   -L      turns off listing\n"
					"   -D      ask for symbol table dump\n"
					"   -P nnn  set listing page size\n"
					"   -U nnn  set default use directory\n"
					, stderr );
				exit(EXIT_FAILURE);
			}
		} else {
			int j;
			if (infile[0][0] != 0) {
				fputs("** multiple input files **\n", stderr);
				exit(EXIT_FAILURE);
			}
			for (j = 0; argv[i][j] != 0; j++) {
				if (j < filelen-3) infile[0][j] = argv[i][j];
			}
			while (j < filelen) {
				infile[0][j] = 0;
				j++;
			}
			infile[0][filelen-1] = 0;
		}
	}
	if (infile[0][0] == 0) {
		fputs("** no input file specified **\n", stderr);
		exit(EXIT_FAILURE);
	}

	dot = 0;
	titlelen = 0;
	for (i = 0; i < filelen; i++) {
		char ch = infile[0][i];
		titlebuf[i] = ch;
		outfile[i] = ch;
		objfile[i] = ch;
		dumpfile[i] = ch;
		if (ch == '.') {
			dot = i;
		}
		if (i < (ttlbuflen-2)) {
			titlebuf[i] = ch;
			if (ch != 0) titlelen = i;
		}
		if ((ch == 0) && (dot == 0)) { /* no suffix on file name */
			dot = i;
		}
	}

	outfile[dot] = '.';
	outfile[dot + 1] = 'l';
	outfile[dot + 2] = 0;
	objfile[dot] = '.';
	objfile[dot + 1] = 'o';
	objfile[dot + 2] = 0;
	dumpfile[dot] = '.';
	dumpfile[dot + 1] = 'd';
	dumpfile[dot + 2] = 0;
}


/* inside smal32 */

int main(int argc, char *argv[]) {
	{ /* setup to classify characters */
		int i;
		for (i = 0; i < 256; i++) charclass[i] = 0;
	}
	classchar( "abcdefghijklmnopqrstuvwxyz", islower | isvalid );
	classchar( "ABCDEFGHIJKLMNOPQRSTUVWXYZ_", isupper | isvalid );
	classchar( "0123456789", isdigit | isvalid );
	classchar( ":.,=><+-\\@&!()[]|~", ispunc | isvalid );
	classchar( "'\"", isquote | isvalid );
	classchar( " ;#", isvalid );

	obj = NULL;
	dmp = NULL;
	o = NULL;
	{
		int i;
		for (i = 0; i < maxgetl; i++) {
			inp[i] = NULL;
		}
	}
	getfiles(argc, argv);

	inp[0] = fopen(infile[0], "r");
	if (inp[0] == NULL) {
		fputs("** cannot open input file **\n", stderr);
		exit(EXIT_FAILURE);
	}
	if (permitlisting) {
		o = fopen(outfile, "w");
		if (o == NULL) {
			fputs("** cannot open object file **\n", stderr);
			exit(EXIT_FAILURE);
		}
	}
	obj = fopen(objfile, "w");
	if (obj == NULL) {
		fputs("** cannot open listing file **\n", stderr);
		exit(EXIT_FAILURE);
	}
	clearsym();
	opinit();
	lineonpg = 1;
	pagenum = 0;
	getdatetime();

	firstpass = true;
	allowlist = false;
	onepass();

	if (poolfull) {
		fputs("** string pool overflowed on pass 1 **\n", stderr);
		exit(EXIT_FAILURE);
	}
	inp[0] = freopen(infile[0], "r", inp[0]);
	if (inp[0] == NULL) {
		fputs("** cannot reopen input file **\n", stderr);
		exit(EXIT_FAILURE);
	}
	fputs("R=.\n", obj);   /* firstpass */
	
	firstpass = false;
	allowlist = true;
	onepass();
	objsufx();

	putchar('\n');
	fputs("  ", stderr);
	if (errorcount == 0) {
		fputs(" no errors\n", stderr);
	} else if (errorcount == 1) {
		fputs(" 1 error\n", stderr);
	} else {
		writedec( stderr, errorcount, 1 );
		fputs(" errors\n", stderr);
	}
	if (symfull) {
		fputs("** symbol table overflowed **\n", stderr);
		errorcount = errorcount + 1;
	}
	if (poolfull) {
		fputs("** string pool overflowed **\n", stderr);
		errorcount = errorcount + 1;
	}
	if (opfull) {
		fputs("** macro name table full **\n", stderr);
		errorcount = errorcount + 1;
	}

	if (symtabdump) {
		dmp = fopen(dumpfile, "w");
		if (dmp == NULL) {
			fputs("** cannot open dump file **\n", stderr);
		} else {
			symdump();
		}
	}

	/* now close all files */
	{
		int i;
		for (i = 0; i < maxgetl; i++) {
			if (inp[i] != NULL)
				fclose(inp[i]);
		}
	}

	if (o != NULL) fclose(o);
	if (obj != NULL) fclose(obj);
	if (dmp != NULL) fclose(dmp);
	if (errorcount == 0) {
		exit(EXIT_SUCCESS);
	} else {
		exit(EXIT_FAILURE);
	}
}
