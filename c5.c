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
     *data,   // data/bss pointer
     *file;

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    *current_func,
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug,    // print executed instructions
    st;

// tokens and classes (operators last and in precedence order)
enum {
    Num = 128, Fun, Sys, Glo, Loc, Arg, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { 
    LEA ,LOC ,ARG ,GLO ,STR ,IMM ,JMP ,LABL,LI  ,LC  ,
    OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,DUP ,CLR ,SAVE,LOAD,RST ,PST ,LST ,
    JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,SI  ,SC  ,PSH ,RET ,CMT ,
    OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,MUL ,DIV ,MOD
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
        if (tk == '\n') {
            if (src) {
                printf("%d: %.*s", line, p - lp, lp);
                lp = p;
                while (le < e) {
                    printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                            "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                            "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,"[*++le * 5]);
                    if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
                }
            }
            ++line;
        }
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
    int *p_label_to_jsr,
        *p_label_to_j_to_jsr,
        *p_pos_for_label_of_last_arg,
        *prev_arg;

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
            *++e = PST;
            *++e = JMP;
            p_pos_for_label_of_last_arg = ++e;
            *(p_label_to_j_to_jsr = ++e) = LABL;
            *++e = JMP;
            p_label_to_jsr = ++e;
            prev_arg = 0;
            while (tk != ')') {
                *(int*)((*p_pos_for_label_of_last_arg) = (int)++e) = LABL;
                expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); 
                *++e = JMP;
                if (prev_arg) { *++e = (int)prev_arg; } else { *++e = (int)p_label_to_j_to_jsr; }
                prev_arg = (int*)*p_pos_for_label_of_last_arg;
            }
            next();
            *(int*)(*p_label_to_jsr = (int)++e) = LABL;
            *++e = JSR; *++e = (int)d;
            *++e = ADJ; *++e = t;
            *++e = LST;
            ty = d[Type];
        }
        else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
        else {
            if (d[Class] == Loc) { *++e = LOC; *++e = d[Val]; }
            else if (d[Class] == Arg) { *++e = ARG; *++e = d[Val]; }
            else { *++e = GLO; *++e = (int)d; } // Treat all unknown to external global variable
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
    else if (tk == '!') { next(); expr(Inc); *++e = IMM; *++e = 0; *++e = EQ; }
    else if (tk == '~') { next(); expr(Inc); *++e = IMM; *++e = 0; *++e = XOR; }
    else if (tk == Add) { next(); expr(Inc); ty = INT; }
    else if (tk == Sub) {
        next();
        *++e = IMM;
        if (tk == Num) {
            *++e = -ival;
            next(); 
        } 
        else {
            *++e = 0;
            expr(Inc);
            *++e = SUB; 
        }
        ty = INT;
    }
    else if (tk == Inc || tk == Dec) {
        t = tk; next(); expr(Inc);
        if (*e == LC) { *e = DUP; *++e = LC; }
        else if (*e == LI) { *e = DUP; *++e = LI; }
        else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
        *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
    }
    else { printf("%d: bad expression\n", line); exit(-1); }

    while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
        t = ty;
        if (tk == Assign) {
            next();
            if (*e == LC || *e == LI) --e; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
            expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
        }
        else if (tk == Cond) {
            next();
            *++e = BZ; d = ++e;
            expr(Assign);
            if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
            *d = (int)(e + 3); *++e = JMP; d = ++e; *++e = LABL;
            expr(Cond);
            *(int*)(*d = (int)(++e)) = LABL;
        }
        else if (tk == Lor) {
            next();
            *++e = DUP;
            *++e = BNZ;
            d = ++e;
            *++e = CLR;
            expr(Lan);
            *(int*)(*d = (int)++e) = LABL;
            ty = INT; 
        }
        else if (tk == Lan) {
            next();
            *++e = DUP;
            *++e = BZ;
            d = ++e;
            *++e = CLR;
            expr(Or);
            *(int*)(*d = (int)++e) = LABL;
            ty = INT; 
        }
        else if (tk == Or)  { next(); expr(Xor); *++e = OR;  ty = INT; }
        else if (tk == Xor) { next(); expr(And); *++e = XOR; ty = INT; }
        else if (tk == And) { next(); expr(Eq);  *++e = AND; ty = INT; }
        else if (tk == Eq)  { next(); expr(Lt);  *++e = EQ;  ty = INT; }
        else if (tk == Ne)  { next(); expr(Lt);  *++e = NE;  ty = INT; }
        else if (tk == Lt)  { next(); expr(Shl); *++e = LT;  ty = INT; }
        else if (tk == Gt)  { next(); expr(Shl); *++e = GT;  ty = INT; }
        else if (tk == Le)  { next(); expr(Shl); *++e = LE;  ty = INT; }
        else if (tk == Ge)  { next(); expr(Shl); *++e = GE;  ty = INT; }
        else if (tk == Shl) { next(); expr(Add); *++e = SHL; ty = INT; }
        else if (tk == Shr) { next(); expr(Add); *++e = SHR; ty = INT; }
        else if (tk == Add) {
            next(); expr(Mul);
            if ((ty = t) > PTR) { *++e = IMM; *++e = 2; *++e = SHL;  }
            *++e = ADD;
        }
        else if (tk == Sub) {
            next(); expr(Mul);
            if (t > PTR && t == ty) { *++e = SUB; *++e = IMM; *++e = 2; *++e = SHR; ty = INT; }
            else if ((ty = t) > PTR) { *++e = IMM; *++e = 2; *++e = SHL; *++e = SUB; }
            else *++e = SUB;
        }
        else if (tk == Mul) {
            next();
            expr(Inc);
            *++e = PST;
            *++e = PSH;
            *++e = PSH;
            *++e = JSR;
            *++e = MUL;
            *++e = ADJ;
            *++e = 2;
            *++e = LST;
            *++e = ADJ; // to restore first two stack item
            *++e = 2;
        }
        else if (tk == Div) {
            next();
            expr(Inc);
            *++e = PST;
            *++e = PSH;
            *++e = PSH;
            *++e = JSR;
            *++e = DIV;
            *++e = ADJ;
            *++e = 2;
            *++e = LST;
            *++e = ADJ; // to retore first two stack item
            *++e = 2;
        }
        else if (tk == Mod) {
            next();
            expr(Inc);
            *++e = PST;
            *++e = PSH;
            *++e = PSH;
            *++e = JSR;
            *++e = MOD;
            *++e = ADJ;
            *++e = 2;
            *++e = LST;
            *++e = ADJ; // to restore first two stack item
            *++e = 2;
        }
        else if (tk == Inc || tk == Dec) {
            if (*e == LC) { *e = DUP; *++e = LC; }
            else if (*e == LI) { *e = DUP; *++e = LI; }
            else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
            *++e = SAVE; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = LOAD;
            next();
        }
        else if (tk == Brak) {
            next(); expr(Assign);
            if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
            if (t > PTR) { *++e = IMM; *++e = 2; *++e = SHL;  }
            else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
            *++e = ADD;
            *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
        }
        else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
    }
}

void codegen(int *e, int *end)
{
    int i;

    while (e <= end) {
        if (*e == LOC) { printf("    addi $t%d, $fp, -%d\n", ++st, (*++e) << 2); }
        else if (*e == ARG) { printf("    addi $t%d, $fp, %d\n", ++st, ((*++e) + 2) << 2); }
        else if (*e == IMM) {
            ++e;
            if (*e > 65535) {
                ++st;
                printf("    lui $t%d, %d\n", st, *e >> 16);
                printf("    addiu $t%d, $t%d, %d\n", st, st, *e & 0xFF);
            }
            else {
                printf("    addi $t%d, $zero, %d\n", ++st, *e);
            }
        }
        else if (*e == JMP) { printf("    j l%u\n", *++e); }
        else if (*e == BZ)  { printf("    beq $t%d, $zero, l%u\n", st--, *++e); }
        else if (*e == BNZ) { printf("    bne $t%d, $zero, l%u\n", st--, *++e); }
        else if (*e == LABL) { printf("l%u:\n", (int)e); }
        else if (*e == LI) { printf("    lw $t%d, 0($t%d)\n", st, st); }
        else if (*e == LC) { printf("    lb $t%d, 0($t%d)\n", st, st); }
        else if (*e == GLO) { ++e; printf("    addi $t%d, $gp, %.*s\n", ++st, ((int*)*e)[Hash] & 0x3F, (char*)((int*)*e)[Name]); }
        else if (*e == STR) { printf("    addi $t%d, $gp, s%u\n", ++st, *++e); }
        else if (*e == PSH) {
            printf("    addi $sp, $sp, -4\n");
            printf("    sw $t%d, 0($sp)\n", st--);
        }
        else if (*e == EQ) {
            if (*(e + 1) == BNZ) {
                e = e + 2;
                printf("    beq $t%d, $t%d, l%u\n", st, st - 1, (int)e);
                st = st - 2;
            }
            else if (*(e + 1) == BZ) {
                e = e + 2;
                printf("    bne $t%d, $t%d, l%u\n", st, st - 1, (int)e);
                st = st - 2;
            }
            else {
                printf("    beq $t%d, $t%d, 2\n", st, st - 1);
                printf("    addi $t%d, $zero, 0\n", st - 1);
                printf("    beq $zero, $zero, 1\n");
                printf("    addi $t%d, $zero, 1\n", st - 1);
                --st;
            }
        }
        else if (*e == OR)  { printf("    or  $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == XOR) { printf("    xor $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == AND) { printf("    and $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == NE)  { printf("    sub $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == SHL) { printf("    sllv $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == SHR) { printf("    srlv $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == ADD) { printf("    add $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == SUB) { printf("    sub $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == SI)  {
            printf("    sw  $t%d, 0($t%d)\n", st, st - 1);
            printf("    addi $t%d, $t%d, 0\n", st - 1, st);
            --st; 
        }
        else if (*e == SC)  {
            printf("    sb  $t%d, 0($t%d)\n", st, st - 1);
            printf("    addi $t%d, $t%d, 0\n", st - 1, st);
            --st; 
        }
        else if (*e == LT)  { printf("    slt $t%d, $t%d, $t%d\n", st - 1, st - 1, st); --st; }
        else if (*e == GT)  { printf("    slt $t%d, $t%d, $t%d\n", st - 1, st, st - 1); --st; }
        else if (*e == LE)  {
            printf("    slt $t%d, $t%d, $t%d\n", st - 1, st, st - 1);
            printf("    addi $t%d, $t%d, -1\n", st - 1, st - 1);
            --st;
        }
        else if (*e == GE)  {
            printf("    slt $t%d, $t%d, $t%d\n", st - 1, st - 1, st);
            printf("    addi $t%d, $t%d, -1\n", st - 1, st - 1);
            --st;
        }
        else if (*e == DUP) { printf("    addi $t%d, $t%d, 0\n", st + 1, st); ++st; }
        else if (*e == CLR) { --st; }
        else if (*e == JSR) {
            ++e;
            if (*e == MUL) { printf("    jal mul\n"); }
            else if (*e == DIV) { printf("    jal div\n"); }
            else if (*e == MOD) { printf("    jal mod\n"); }
            else { printf("    jal %.*s\n", ((int*)*e)[Hash] & 0x3F, (char*)(((int*)*e)[Name])); }

            printf("    addi $t%d, $v0, 0\n", ++st);
        }
        else if (*e == ADJ)  { printf("    addi $sp, $sp, %d\n", (*++e) << 2); }
        else if (*e == PST) {
            if (st >= 0) {
                printf("    addi $sp, $sp, -%d\n", (st + 1) << 2);
                i = 0;
                while (i <= st) {
                    printf("    sw $t%d, %d($sp)\n", i, i << 2);
                    ++i;
                }
            }
        }
        else if (*e == LST) {
            if (st >= 1) {
                i = st - 1; // by pass the return value
                while (i >= 0) {
                    printf("    lw $t%d, %d($sp)\n", i, i << 2);
                    --i;
                }
                printf("    addi $sp, $sp, %d\n", st << 2);
            }
        }
        else if (*e == SAVE) { printf("    addi $at, $t%d, 0\n", st); }
        else if (*e == LOAD) { printf("    addi $t%d, $at, 0\n", ++st); }
        else if (*e == LEV) { printf("    j _%.*s_end\n", current_func[Hash] & 0x3F, (char*)current_func[Name]); }
        else if (*e == RET) { printf("    addi $v0, $t%d, 0\n", st--); }
        else if (*e == RST) { st = -1; }
        else if (*e == CMT) { printf("\n## %s:%d\n", file, *++e); }
        else { printf("Unknown inst %d\n", *e); exit(-1); }

        ++e;
    }
}

void stmt()
{
    int *a, *b;
    if (tk == If) {
        *++e = CMT;
        *++e = line;

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
        *(int*)(*b = (int)(++e)) = LABL;
    }
    else if (tk == While) {
        *++e = CMT;
        *++e = line;

        next();
        *(a = ++e) = LABL;
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        *++e = JMP; *++e = (int)a;
        *(int*)(*b = (int)(++e)) = LABL;
    }
    else if (tk == Return) {
        *++e = CMT;
        *++e = line;

        next();
        if (tk != ';') {
            expr(Assign);
            *++e = RET;
        }
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
        *++e = CMT;
        *++e = line;

        expr(Assign);
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
    *++e = RST;
}

int main(int argc, char **argv)
{
    int fd, bt, ty, poolsz, *idmain;
    int i; // temps
    int *se;

    int *s_sym, *s_e;
    char *s_data, *td;

    --argc; ++argv;
    if (argc < 1) { printf("usage: c5 file ...\n"); return -1; }

    poolsz = 256*1024; // arbitrary size
    if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
    if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
    if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

    memset(sym,  0, poolsz);
    memset(e,    0, poolsz);
    memset(data, 0, poolsz);

    s_sym = sym;
    s_e = e;
    s_data = data;

    p = "char else enum if int return sizeof while "
        "open read close printf malloc memset memcmp exit mul div mod void main";
    i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
    i = OPEN; while (i <= MOD) { next(); id[Class] = Fun; id[Type] = INT; id[Val] = i++; } // add library to symbol table
    next(); id[Tk] = Char; // handle void type
    next(); idmain = id; // keep track of main


    while (argc--) {
        if ((fd = open((file = *argv++), 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }
        if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
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
                if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
                next();
                id[Type] = ty;
                if (tk == '(') { // function
                    id[Class] = Fun;
                    current_func = id;
                    id[Val] = (int)(e + 1);
                    next(); i = 0;
                    while (tk != ')') {
                        ty = INT;
                        if (tk == Int) next();
                        else if (tk == Char) { next(); ty = CHAR; }
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
                        if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
                        id[HClass] = id[Class]; id[Class] = Arg;
                        id[HType]  = id[Type];  id[Type] = ty;
                        id[HVal]   = id[Val];   id[Val] = i++;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                    if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
                    loc = ++i;
                    i = 0;
                    next();
                    while (tk == Int || tk == Char) {
                        bt = (tk == Int) ? INT : CHAR;
                        next();
                        while (tk != ';') {
                            ty = bt;
                            while (tk == Mul) { next(); ty = ty + PTR; }
                            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
                            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
                            id[HClass] = id[Class]; id[Class] = Loc;
                            id[HType]  = id[Type];  id[Type] = ty;
                            id[HVal]   = id[Val];   id[Val] = ++i;
                            next();
                            if (tk == ',') next();
                        }
                        next();
                    }
                    se = e + 1;
                    printf("%.*s:\n", current_func[Hash] & 0x3F, (char*)current_func[Name]);
                    printf(
                            "    addi $sp, $sp, -4\n"
                            "    sw $fp, 0($sp)\n"
                            "    addi $sp, $sp, -4\n"
                            "    sw $ra, 0($sp)\n"
                            "    addi $fp, $sp, 0\n"
                            "    addi $sp, $sp, -%d\n"
                            "\n",
                            i << 2
                        );
                    while (tk != '}') stmt();
                    st = -1;
                    codegen(se, e);
                    printf(
                            "\n"
                            "_%.*s_end:\n"
                            "    addi $sp, $sp, %d\n"
                            "    lw $ra, 0($sp)\n"
                            "    addi $sp, $sp, 4\n"
                            "    lw $fp, 0($sp)\n"
                            "    addi $sp, $sp, 4\n"
                            "    jr $ra\n"
                            , current_func[Hash] & 0x3F, (char*)current_func[Name], i << 2
                        );
                    // *++e = LEV;
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
                    id[Class] = Glo;
                    printf(
                            "%.*s:\n"
                            "    dd 0\n\n",
                            id[Hash] & 0x3F, (char*)id[Name]
                        );
                }
                if (tk == ',') next();
            }
            next();
        }
    }

    printf("\n");

    // string literals
    td = s_data;
    while (td < data) {
        printf("s%u:\n", (int)td);
        printf("    string \"");
        while (*td) {
            if (*td == '\n') { printf("\\n"); }
            else if (*td == '"') { printf("\\\""); }
            else if (*td == '\'') { printf("\\\'"); }
            else if (*td == '\\') { printf("\\\\"); }
            else { printf("%c", *td); }
            ++td;
        }
        printf("\"\n\n");
        td = (char*)((int)td + sizeof(int) & -sizeof(int));
    }

    free(s_sym);
    free(s_e);
    free(s_data);
}
