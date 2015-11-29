// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    *current_func,
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    st;

// tokens and classes (operators last and in precedence order)
enum {
    Num = 128, Fun, Sys, Glo, Arg, Loc, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum {
    STR ,GLO ,LOC ,ARG ,IMM ,JMP ,CALL,BZ  ,BNZ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
    OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,LABL,CMMT
};

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
    char *pp;

    while ((tk = *p)) {
        ++p;
        if (tk == '\n') ++line;
        else if (tk == '#') {
            while (*p != 0 && *p != '\n') ++p;
        }
        else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
            pp = p - 1;
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
                tk = tk * 147 + *p++;
            tk = (tk << 6) + (p - pp);
            id = sym;
            while (id[Tk]) {
                if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
                id = id + Idsz;
            }
            id[Name] = (int)pp;
            id[Hash] = tk;
            tk = id[Tk] = Id;
            return;
        }
        else if (tk >= '0' && tk <= '9') {
            if ((ival = tk - '0')) { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
            else if (*p == 'x' || *p == 'X') {
                while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
            }
            else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
            tk = Num;
            return;
        }
        else if (tk == '/') {
            if (*p == '/') {
                ++p;
                while (*p != 0 && *p != '\n') ++p;
            }
            else {
                tk = Div;
                return;
            }
        }
        else if (tk == '\'' || tk == '"') {
            pp = data;
            while (*p != 0 && *p != tk) {
                if ((ival = *p++) == '\\') {
                    if ((ival = *p++) == 'n') ival = '\n';
                }
                if (tk == '"') *data++ = ival;
            }
            ++p;
            if (tk == '"') ival = (int)pp; else tk = Num;
            return;
        }
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
}

void expr(int lev)
{
    int t, *d;

    if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
    else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
    else if (tk == '"') {
        *++e = STR; *++e = ival; next();
        while (tk == '"') next();
        data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
    }
    else if (tk == Sizeof) {
        next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
        ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
        while (tk == Mul) { next(); ty = ty + PTR; }
        if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
        *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
        ty = INT;
    }
    else if (tk == Id) {
        d = id; next();
        if (tk == '(') {
            next();
            t = 0;
            while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
            next();
            if (d[Class] == Fun) { *++e = CALL; *++e = t; *++e = (int)d; }
            else { printf("%d: bad function call\n", line); exit(-1); }
            ty = d[Type];
        }
        else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
        else {
            if (d[Class] == Loc) { *++e = LOC; *++e = d[Val]; }
            else if (d[Class] == Arg) { *++e = ARG; *++e = d[Val]; }
            else if (d[Class] == Glo) { *++e = GLO; *++e = (int)d; }
            else { printf("%d: undefined variable\n", line); exit(-1); }
            *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
        }
    }
    else if (tk == '(') {
        next();
        if (tk == Int || tk == Char) {
            t = (tk == Int) ? INT : CHAR; next();
            while (tk == Mul) { next(); t = t + PTR; }
            if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
            expr(Inc);
            ty = t;
        }
        else {
            expr(Assign);
            if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        }
    }
    else if (tk == Mul) {
        next(); expr(Inc);
        if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
        *++e = (ty == CHAR) ? LC : LI;
    }
    else if (tk == And) {
        next(); expr(Inc);
        if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
        ty = ty + PTR;
    }
    else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
    else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
    else if (tk == Add) { next(); expr(Inc); ty = INT; }
    else if (tk == Sub) {
        next(); *++e = IMM;
        if (tk == Num) { *++e = -ival; next(); } else { *++e = 0; *++e = PSH; expr(Inc); *++e = SUB; }
        ty = INT;
    }
    else if (tk == Inc || tk == Dec) {
        t = tk; next(); expr(Inc);
        if (*e == LC) { *e = PSH; *++e = LC; }
        else if (*e == LI) { *e = PSH; *++e = LI; }
        else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
        *++e = PSH;
        *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
    }
    else { printf("%d: bad expression\n", line); exit(-1); }

    while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
        t = ty;
        if (tk == Assign) {
            next();
            if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
            expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
        }
        else if (tk == Cond) {
            next();
            *++e = BZ; d = ++e;
            expr(Assign);
            if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
            *d = (int)(e + 3); *++e = JMP; d = ++e; *++e = LABL;
            expr(Cond);
            *(int*)(*d = (int)++e) = LABL;
        }
        else if (tk == Lor) {
            next();
            *++e = BNZ;
            d = ++e;
            expr(Lan);
            *(int*)(*d = (int)++e) = LABL;
            ty = INT;
        }
        else if (tk == Lan) {
            next();
            *++e = BZ;
            d = ++e;
            expr(Or);
            *(int*)(*d = (int)++e) = LABL;
            ty = INT;
        }
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
            if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = 2; *++e = SHL;  }
            *++e = ADD;
        }
        else if (tk == Sub) {
            next(); *++e = PSH; expr(Mul);
            if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
            else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = 2; *++e = SHL; *++e = SUB; }
            else *++e = SUB;
        }
        else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
        else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
        else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
        else if (tk == Inc || tk == Dec) {
            if (*e == LC) { *e = PSH; *++e = LC; }
            else if (*e == LI) { *e = PSH; *++e = LI; }
            else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? SUB : ADD;
            next();
        }
        else if (tk == Brak) {
            next(); *++e = PSH; expr(Assign);
            if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
            if (t > PTR) { *++e = PSH; *++e = IMM; *++e = 2; *++e = SHL;  }
            else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
            *++e = ADD;
            *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
        }
        else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
    }
}

void
codegen(int *e, int *le)
{
    int i, lst;

    st = -1;
    lst = 0;    // last in stack
    while (e != le) {
        if (*e == STR)      { printf("    addi $v0, $gp, s%u\n", *++e); lst = 0; }
        else if (*e == GLO) {
            ++e;
            lst = 0;
            if (*(e + 1) == LI) { printf("    lw   $v0, %.*s($gp)\n", ((int*)(*e))[Hash] & 0x3F, (char*)((int*)(*e))[Name]); ++e; }
            else if (*(e + 1) == LC) { printf("    lb $v0, %.*s($gp)\n", ((int*)(*e))[Hash] & 0x3F, (char*)((int*)(*e))[Name]); ++e; }
            else if (*(e + 1) == PSH) {
                printf("    addi $t%d, $gp, %.*s\n", ++st, ((int*)(*e))[Hash] & 0x3F, (char*)((int*)(*e))[Name]);
                ++e; lst = 1;
            }
            else { printf("    addi $v0, $gp, %.*s\n", ((int*)(*e))[Hash] & 0x3F, (char*)((int*)(*e))[Name]); }
        }
        else if (*e == LOC) {
            lst = 0;
            if (*(e + 2) == LI) { printf("    lw   $v0, -%d($fp)\n", (*++e) << 2); ++e; }
            else if (*(e + 2) == LC) { printf("    lb $v0, -%d($fp)\n", (*++e) << 2); ++e; }
            else if (*(e + 2) == PSH) { printf("    addi $t%d, $fp, -%d\n", ++st, (*++e) << 2); ++e; lst = 1; }
            else { printf("    addi $v0, $fp, -%d\n", (*++e) << 2); }
        }
        else if (*e == ARG) {
            lst = 0;
            if (*(e + 2) == LI) { printf("    lw $v0, %d($fp)\n", (*++e + 2) << 2); ++e; }
            else if (*(e + 2) == LC) { printf("    lb $v0, %d($fp)\n", (*++e + 2) << 2); ++e; }
            else if (*(e + 2) == PSH) { printf("    addi $t%d, $fp, %d\n", ++st, (*++e + 2) << 2); ++e; lst = 1; }
            else { printf("    addi $v0, $fp, %d\n", (*++e + 2) << 2); }
        }
        else if (*e == IMM) {
            ++e;
            if (*e <= 32767 && *e >= -65535) {
                lst = 0;
                if (*(e + 1) == LI || *(e + 1) == LC) { printf("    %s   $v0, %d($gp)\n", *(e + 1) == LI ? "lw" : "lb", *e); ++e; }
                else if (*(e + 1) == PSH) { printf("    addi $t%d, $zero, %d\n", ++st, *e); ++e; lst = 1; }
                else if (*(e + 1) == OR)  { printf("    ori  $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == XOR) { printf("    xori $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == AND) { printf("    andi $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == NE)  { printf("    addi $v0, $t%d, %d\n", st--, -*e); ++e; }
                else if (*(e + 1) == SHL) { printf("    sll  $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == SHR) { printf("    srl  $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == ADD) { printf("    addi $v0, $t%d, %d\n", st--, *e); ++e; }
                else if (*(e + 1) == SUB) { printf("    addi $v0, $t%d, %d\n", st--, -*e); ++e; }
                else printf("    addi $v0, $zero, %d\n", *e);
            }
            else { printf("Imm too large: %d\n", *e); exit(-1); }
        }
        else if (*e == JMP) {
            // address indepent code
            printf("    beq  $zero, $zero, _%.*s_%u\n", current_func[Hash] & 0x3F, (char*)current_func[Name], *++e); 
        }
        else if (*e == CALL) {
            ++e; i = 0; lst = 0;
            printf("    addi $sp, $sp, -%d\n", (st + 1) << 2);
            while (i <= st - *e) { printf("    sw   $t%d, %d($sp)\n", i, (st - i) << 2); ++i; }  // save temp stack
            while (i <= st) {
                printf("    sw   $t%d, %d($sp)\n", (st - *e) + (st - i) + 1, (st - i) << 2); ++i;
            }

            printf("    jal  %.*s\n", ((int*)(*(e + 1)))[Hash] & 0x3F, (char*)((int*)(*(e + 1)))[Name]); 

            i = 0;
            while (i <= st - *e) { printf("    lw $t%d, %d($sp)\n", i, (st - i) << 2); ++i; }
            printf("    addi $sp, $sp, %d\n", (st + 1) << 2);
            st = st - *e;

            ++e;
        }
        else if (*e == BZ)  {
            if (lst) { printf("    beq  $t%d, $zero, _%.*s_%u\n", st, current_func[Hash] & 0x3F, (char*)current_func[Name], *++e); }
            else { printf("    beq  $v0, $zero, _%.*s_%u\n", current_func[Hash] & 0x3F, (char*)current_func[Name], *++e); }
        }
        else if (*e == BNZ) {
            if (lst) { printf("    bne  $t%d, $zero, _%.*s_%u\n", st, current_func[Hash] & 0x3F, (char*)current_func[Name], *++e); }
            else { printf("    bne  $v0, $zero, _%.*s_%u\n", current_func[Hash] & 0x3F, (char*)current_func[Name], *++e); }
        }
        else if (*e == LEV) {
            if (e + 1 != le) {  // at the end of function
                printf("    j    _%.*s_end\n", current_func[Hash] & 0x3F, (char*)current_func[Name]); 
            }
        }
        else if (*e == LI)  { if (lst) printf("    lw   $v0, 0($t%d)\n", st); else printf("    lw   $v0, 0($v0)\n"); lst = 0; }
        else if (*e == LC)  { if (lst) printf("    lb   $v0, 0($t%d)\n", st); else printf("    lb   $v0, 0($v0)\n"); lst = 0; }
        else if (*e == SI)  { lst = 0; printf("    sw   $v0, 0($t%d)\n", st--); }
        else if (*e == SC)  { lst = 0; printf("    sb   $v0, 0($t%d)\n", st--); }
        else if (*e == PSH) { printf("    addi $t%d, $v0, 0\n", ++st); }
        else if (*e == OR)  { lst = 0; printf("    or   $v0, $t%d, $v0\n", st--); }
        else if (*e == XOR) { lst = 0; printf("    xor  $v0, $t%d, $v0\n", st--); }
        else if (*e == AND) { lst = 0; printf("    and  $v0, $t%d, $v0\n", st--); }
        else if (*e == EQ)  {
            lst = 0;
            if (*(e + 1) == BNZ) {
                e = e + 2;
                printf("    beq  $v0, $t%d, _%.*s_%u\n", st--, current_func[Hash] & 0x3F, (char*)current_func[Name], *e);
            }
            else if (*(e + 1) == BZ) {
                e = e + 2;
                printf("    bne  $v0, $t%d, _%.*s_%u\n", st--, current_func[Hash] & 0x3F, (char*)current_func[Name], *e);
            }
            else {
                printf(
                        "    beq  $v0, $t%d, 2\n"
                        "    addi $v0, $zero, 0\n"
                        "    beq  $zero, $zero, 1\n"
                        "    addi $v0, $zero, 1\n", st--
                    );
            }
        }
        else if (*e == NE)  { printf("    sub  $v0, $t%d, $v0\n", st--); }
        else if (*e == LT)  { printf("    slt  $v0, $t%d, $v0\n", st--); }
        else if (*e == LE)  { printf("    slt  $v0, $v0, $t%d\n    addi $v0, $v0, -1\n", st--); }
        else if (*e == GT)  { printf("    slt  $v0, $v0, $t%d\n", st--); }
        else if (*e == GE)  { printf("    slt  $v0, $t%d, $v0\n    addi $v0, $v0, -1\n", st--); }
        else if (*e == SHL) { printf("    sllv $v0, $t%d, $v0\n", st--); }
        else if (*e == SHR) { printf("    srlv $v0, $t%d, $v0\n", st--); }
        else if (*e == ADD) { printf("    add  $v0, $t%d, $v0\n", st--); }
        else if (*e == SUB) { printf("    sub  $v0, $t%d, $v0\n", st--); }
        else if (*e == MUL || *e == DIV || *e == MOD) {
            if (!lst) { printf("    addi $t%d, $v0, 0\n", ++st); }
            i = 0;
            printf("    addi $sp, $sp, -%d\n", (st + 1) << 2);
            while (i <= st - 2) { printf("    sw   $t%d, %d($sp)\n", i, (st - i) << 2); ++i; }  // save temp stack
            while (i <= st) {
                printf("    sw   $t%d, %d($sp)\n", st + st - i - 1, (st - i) << 2); ++i;
            }
            if      (*e == MUL) { printf("    jal  mul\n"); }
            else if (*e == DIV) { printf("    jal  div\n"); }
            else if (*e == MOD) { printf("    jal  mod\n"); }
            i = 0;
            while (i <= st - 2) { printf("    lw   $t%d, %d($sp)\n", i, (st - i) << 2); ++i; }
            printf("    addi $sp, $sp, %d\n", (st + 1) << 2);
            st = st - 2;
        }
        else if (*e == LABL){ printf("_%.*s_%u:\n", current_func[Hash] & 0x3F, (char*)current_func[Name], (int)e); }
        else if (*e == CMMT){ printf("## line %d\n", *++e); }
        else { printf("Unknown inst: %d\n", *e); exit(-1); }
        ++e;
    }
}

void stmt()
{
    int *a, *b;

    if (tk == If) {
        *++e = CMMT; *++e = line;
        next();
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        if (tk == Else) {
            *b = (int)(e + 3); *++e = JMP; b = ++e; *++e = LABL;
            next();
            stmt();
        }
        *(int*)(*b = (int)++e) = LABL;
    }
    else if (tk == While) {
        *++e = CMMT; *++e = line;
        next();
        *(int*)(a = ++e) = LABL;
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        *++e = JMP; *++e = (int)a;
        *(int*)(*b = (int)++e) = LABL;
    }
    else if (tk == Return) {
        *++e = CMMT; *++e = line;
        next();
        if (tk != ';') expr(Assign);
        *++e = LEV;
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
    else if (tk == '{') {
        next();
        while (tk != '}') stmt();
        next();
    }
    else if (tk == ';') {
        next();
    }
    else {
        *++e = CMMT; *++e = line;
        expr(Assign);
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
}

int main(int argc, char **argv)
{
    int fd, bt, ty, poolsz;
    int i; // temps
    int *le;
    int *te;
    char *td, *t;

    --argc; ++argv;
    if (argc < 1) { printf("usage: c5 file ...\n"); return -1; }

    poolsz = 256*1024; // arbitrary size
    if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
    if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
    if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

    memset(sym,  0, poolsz);
    memset(e,    0, poolsz);
    memset(data, 0, poolsz);

    te = e;
    td = data;

    p = "char else enum if int return sizeof while void ";
    i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
    next(); id[Tk] = Char; // handle void type

    if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }

    while (argc--) {
        p = lp;
        if ((fd = open(*argv++, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }
        if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
        p[i] = 0;
        close(fd);

        // parse declarations
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
                        if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
                        next();
                        if (tk == Assign) {
                            next();
                            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
                            i = ival;
                            next();
                        }
                        id[Class] = Num; id[Type] = INT; id[Val] = i++;
                        if (tk == ',') next();
                    }
                    next();
                }
            }
            while (tk != ';' && tk != '}') {
                ty = bt;
                while (tk == Mul) { next(); ty = ty + PTR; }
                if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
                if (id[Class]) { 
                    printf("%d: duplicate global definition: %.*s\n", line, id[Hash] & 0x3F, (char*)id[Name]);
                    return -1; 
                }
                next();
                id[Type] = ty;
                if (tk == '(') { // function
                    id[Class] = Fun;
                    id[Val] = (int)(e + 1);
                    current_func = id;
                    next(); i = 0;
                    while (tk != ')') {
                        ty = INT;
                        if (tk == Int) next();
                        else if (tk == Char) { next(); ty = CHAR; }
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
                        if (id[Class] == Loc || id[Class] == Arg) { printf("%d: duplicate parameter definition\n", line); return -1; }
                        id[HClass] = id[Class]; id[Class] = Arg;
                        id[HType]  = id[Type];  id[Type] = ty;
                        id[HVal]   = id[Val];   id[Val] = i++;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                    if (tk == '{') {
                        i = 0;
                        next();
                        while (tk == Int || tk == Char) {
                            bt = (tk == Int) ? INT : CHAR;
                            next();
                            while (tk != ';') {
                                ty = bt;
                                while (tk == Mul) { next(); ty = ty + PTR; }
                                if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
                                if (id[Class] == Loc || id[Class] == Arg) { printf("%d: duplicate local definition\n", line); return -1; }
                                id[HClass] = id[Class]; id[Class] = Loc;
                                id[HType]  = id[Type];  id[Type] = ty;
                                id[HVal]   = id[Val];   id[Val] = i++;
                                next();
                                if (tk == ',') next();
                            }
                            next();
                        }
                        le = e;
                        while (tk != '}') stmt();
                        printf(
                                "\n%.*s:\n"
                                "    addi $sp, $sp, -4\n"
                                "    sw   $fp, 0($sp)\n"
                                "    addi $sp, $sp, -4\n"
                                "    sw   $ra, 0($sp)\n"
                                "    addi $fp, $sp, 0\n"
                                "\n",
                                current_func[Hash] & 0x3F, (char*)current_func[Name]
                            );
                        if (i) printf("    addi $sp, $sp, -%d\n", i << 2);
                        codegen(le + 1, e + 1);
                        printf(
                                "\n"
                                "_%.*s_end:\n",
                                current_func[Hash] & 0x3F, (char*)current_func[Name]
                            );
                        if (i) printf("    addi $sp, $sp, -%d\n", i << 2);
                        printf(
                                "    lw   $ra, 0($sp)\n"
                                "    addi $sp, $sp, 4\n"
                                "    lw   $fp, 0($sp)\n"
                                "    addi $sp, $sp, 4\n"
                                "    jr   $ra\n"
                                "\n"
                            );
                    }
                    else if (tk != ';') { printf("%d: bad function decl\n", line); exit(-1); }
                    id = sym; // unwind symbol table locals
                    while (id[Tk]) {
                        if (id[Class] == Loc || id[Class] == Arg) {
                            id[Class] = id[HClass];
                            id[Type] = id[HType];
                            id[Val] = id[HVal];
                        }
                        id = id + Idsz;
                    }
                }
                else {
                    id[Class] = Glo;
                    id[Val] = (int)data;
                    printf("\n%.*s:\n    dd 0\n", id[Hash] & 0x3F, (char*)id[Name]);
                }
                if (tk == ',') next();
            }
            next();
        }
    }

    t = td;
    while (t < data) {
        printf("s%u:\n", (int)t);
        printf("    string \"");
        while (*t) {
            if (*t == '\n')         { printf("\\n"); }
            else if (*t == '"')     { printf("\\\""); }
            else if (*t == '\'')    { printf("\\\'"); }
            else if (*t == '\\')    { printf("\\\\"); }
            else                    { printf("%c", *t); }
            ++t;
        }
        printf("\"\n\n");
        t = (char*)((int)t + sizeof(int) & -sizeof(int));
    }

    free(sym);
    free(te);
    free(td);
}
