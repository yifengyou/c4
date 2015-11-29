#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

enum {
    Tk,
    Hash,
    Name,
    Value,
    IdSize
};

enum {
    _ = 255,
    ADD  ,ADDU ,SUB  ,SUBU ,AND  ,OR   ,XOR  ,NOR  ,SLT  ,SLTU ,SLL  ,SRL  ,SRA  ,SLLV ,SRLV ,SRAV ,JR   ,JALR ,SCALL,ERET ,MFCO ,MTCO ,
    ADDI ,ADDIU,ANDI ,ORI  ,XORI ,LUI  ,LW   ,LH   ,LB   ,SW   ,SH   ,SB   ,SLTI ,SLTIU,
    BEQ  ,BNE  ,J    ,JAL  ,
    DB   ,DW   ,DD   ,STR  ,
    _ZERO,_AT  ,_V0  ,_V1  ,_A0  ,_A1  ,_A2  ,_A3  ,_T0  ,_T1  ,_T2  ,_T3  ,_T4  ,_T5  ,_T6  ,_T7  ,
    _S0  ,_S1  ,_S2  ,_S3  ,_S4  ,_S5  ,_S6  ,_S7  ,_T8  ,_T9  ,_K0  ,_K1  ,_GP  ,_SP  ,_FP  ,_RA
};

enum { Id = 128, Reg, Imm, Labl };

char *p,
     *buf,
     *file,
     *output;

int  *sym,
     *id,
     *e,
     tk,
     ival,
     line
     ;

void
next()
{
    char *pp;
    int sign;

    while ((tk = *p)) {
        ++p;
        if (tk == '\n') ++line;
        else if (tk == '#') {
            while (*p != 0 && *p != '\n') ++p;
        }
        else if (tk == '$') {
            if (*p >= '0' && *p <= '9') {
                ival = 0;
                while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0';
                tk = Reg;
            }
            else {
                pp = p; tk = 0;
                while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
                    tk = tk * 147 + *p++;
                tk = (tk << 6) + (p - pp);
                id = sym;
                while (id[Tk]) {
                    if (tk == id[Hash] && !memcmp((char*)id[Name], pp, p - pp) && id[Tk] >= _ZERO && id[Tk] <= _RA) {
                        tk = Reg;
                        ival = id[Tk] - _ZERO;
                        return;
                    }
                    id = id + IdSize;
                }
                printf("%s:%d: bad register `%.*s'\n", file, line, p - pp, pp);
                exit(-1);
            }
        }
        else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
            pp = p - 1;
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9')|| *p == '_')
                tk = tk * 147 + *p++;
            tk = (tk << 6) + (p - pp);
            id = sym;
            while (id[Tk]) {
                if (tk == id[Hash] && !memcmp((char*)id[Name], pp, p - pp)) {
                    tk = Id;
                    ival = id[Value];
                    return;
                }
                id = id + IdSize;
            }
            id[Name] = (int)pp;
            id[Hash] = tk;
            tk = id[Tk] = Id;
            ival = id[Value];
            return;
        }
        else if (tk == '-' || (tk >= '0' && tk <= '9')) {
            if ((sign = (tk == '-'))) tk = *p++;
            if ((ival = tk - '0')) { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
            else if (*p == 'x' || *p == 'X') {
                while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
            }
            else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
            if (sign) ival = -ival;
            tk = Imm;
            return;
        }
        else if (tk == '\'' || tk == '"') {
            pp = buf;
            while (*p != 0 && *p != tk) {
                if ((ival = *p++) == '\\') {
                    if ((ival = *p++) == 'n') ival = '\n';
                }
                if (tk == '"') *pp++ = ival;
            }
            ++p;
            if (tk == '"') ival = pp - buf; else tk = Imm;
            return;
        }
        else if ( tk == ',' || tk == '(' || tk == ')' || tk == ':' ) return;
        else if ( tk != ' ' && tk != '\n' && tk != '\t' && tk != '\r' && tk != '\b') {
            printf("%s:%d: bad token '%c'(%d)\n", file, line, tk, tk);
            exit(-1);
        }
    }
}

int
main(int argc, char **argv)
{
    int fd, poolsz, i, *lsym, *d, *le;
    char *tp;

    int offset;

    --argc; ++argv;

    while (argc && **argv == '-') {
        if ((*argv)[1] == 'o') {
            if (! --argc) { printf("no output file\n"); exit(-1); }
            output = *++argv;
            --argc; ++argv;
        }
        else { printf("unknown argument `%s'\n", *argv); exit(-1); }
    }

    if (!output) { printf("no output file\n"); exit(-1); }
    if (!argc) { printf("usage: as -o output file ...\n"); exit(-1); }

    poolsz = 256 * 1024; // arbitrary size

    if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); exit(-1); }
    if (!(buf = malloc(poolsz))) { printf("could not malloc(%d) buffer area\n", poolsz); exit(-1); }
    if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) exec area\n", poolsz); exit(-1); }

    memset(sym, 0, poolsz);
    memset(buf, 0, poolsz);
    memset(e, 0, poolsz);

    p = "add addu sub subu and or xor nor slt sltu sll srl sra sllv srlv srav jr jalr scall eret mfco mtco "
        "addi addiu andi ori xori lui lw lh lb sw sh sb slti sltiu "
        "beq bne j jal "
        "db dw dd string "
        "zero at v0 v1 a0 a1 a2 a3 t0 t1 t2 t3 t4 t5 t6 t7 "
        "s0 s1 s2 s3 s4 s5 s6 s7 t8 t9 k0 k1 gp sp fp ra ";
    i = ADD;
    while (i <= _RA) { next(); id[Tk] = i++; }

    if (!(tp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }

    offset = 0;
    lsym = sym;
    while (argc--) {
        if ((fd = open(file = *argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }
        if ((i = read(fd, p, poolsz - 1)) <= 0) { printf("read() returned %d\n", i); return -1; }
        close(fd);

        line = 1;
        next();
        while (tk) {
            if (tk == Id) {
                if ((id[Tk] >= ADD && id[Tk] <= SLTU) || (id[Tk] >= SLLV && id[Tk] <= SRAV)) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (
                        (id[Tk] >= SLL && id[Tk] <= SRA) || 
                        (id[Tk] >= ADDI && id[Tk] <= XORI) || 
                        (id[Tk] >= SLTI && id[Tk] <= SLTIU) ||
                        (id[Tk] >= BEQ && id[Tk] <= BNE)
                ) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Id && tk != Imm) { printf("%s:%d expect imm\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] >= JR && id[Tk] <= JALR) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] == SCALL) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] == ERET) {
                    offset = offset + 4;
                }
                else if ((id[Tk] >= MFCO && id[Tk] <= MTCO) || id[Tk] == LUI) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Imm) { printf("%s:%d expect imm\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] >= LW && id[Tk] <= SB) {
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ',') { printf("%s:%d expect `,'", file, line); exit(-1); }
                    next(); if (tk != Id && tk != Imm) { printf("%s:%d expect offset\n", file, line); exit(-1); }
                    next(); if (tk != '(') { printf("%s:%d expect `('\n", file, line); exit(-1); }
                    next(); if (tk != Reg) { printf("%s:%d expect register\n", file, line); exit(-1); }
                    next(); if (tk != ')') { printf("%s:%d expect `)'\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] >= J && id[Tk] <= JAL) {
                    next(); if (tk != Id && tk != Imm) { printf("%s:%d expect address\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] == DB || id[Tk] == DW || id[Tk] == DD) {
                    next(); if (tk != Id && tk != Imm) { printf("%s:%d expect imm\n", file, line); exit(-1); }
                    offset = offset + 4;
                }
                else if (id[Tk] == STR) {
                    next(); if (tk != '"') { printf("%s:%d expect string\n", file, line); exit(-1); }
                    offset = (offset - 4 + ival) & -sizeof(int);
                    offset = offset + 4;
                }
                else {
                    d = id;
                    if (d[Tk] == Labl) { printf("%s:%d duplicate label `%.*s'\n", file, line, d[Hash] & 0x3F, (char*)d[Name]); }
                    next();
                    if (tk != ':') { printf("%s:%d bad label\n", file, line); exit(-1); }
                    d[Tk] = Labl;
                    d[Value] = offset;
                }
            }
            else { printf("%s:%d bad inst\n", file, line); exit(-1); }

            next();
        }

        ++argv;
    }

    p = tp;
    next();
    while (tk) {
        if (tk == Id && id[Tk] != STR && id[Tk] != Labl) {
            i = 0;
            if (
                    (id[Tk] >= ADD && id[Tk] <= SLTU)  ||
                    (id[Tk] >= SLLV && id[Tk] <= SRAV)
            ) {
                if      (id[Tk] == ADD)     i = i | 0x20;
                else if (id[Tk] == ADDU)    i = i | 0x21;
                else if (id[Tk] == SUB)     i = i | 0x22;
                else if (id[Tk] == SUBU)    i = i | 0x23;
                else if (id[Tk] == AND)     i = i | 0x24;
                else if (id[Tk] == OR)      i = i | 0x25;
                else if (id[Tk] == XOR)     i = i | 0x26;
                else if (id[Tk] == NOR)     i = i | 0x27;
                else if (id[Tk] == SLT)     i = i | 0x2A;
                else if (id[Tk] == SLTU)    i = i | 0x2B;
                else if (id[Tk] == SLLV)    i = i | 0x04;
                else if (id[Tk] == SRLV)    i = i | 0x06;
                else if (id[Tk] == SRAV)    i = i | 0x07;
                next(); i = i | ((ival & 0x1F) << 11);  next();
                next(); i = i | ((ival & 0x1F) << 21);  next();
                next(); i = i | ((ival & 0x1F) << 16);
            }
            else if (
                    id[Tk] >= SLL && id[Tk] <= SRA
            ) {
                if      (id[Tk] == SLL)     i = i | 0x00;
                else if (id[Tk] == SRL)     i = i | 0x02;
                else if (id[Tk] == SRA)     i = i | 0x03;
                next(); i = i | ((ival & 0x1F) << 11);  next();
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next(); i = i | ((ival & 0x1F) << 6);
            }
            else if (
                    id[Tk] == JR    ||
                    id[Tk] == JALR
            ) {
                if      (id[Tk] == JR)      i = i | 0x08;
                else if (id[Tk] == JALR)    i = i | 0x19;
                next(); i = i | ((ival & 0x1F) << 21);
            }
            else if (
                    id[Tk] == SCALL
            ) {
                next(); i = i | ((ival & 0x1F) << 16);
                i = i | 0x0C;
            }
            else if (
                    id[Tk] == ERET
            ) {
                i = (0x0A << 26) | (0x12);
            }
            else if (
                    id[Tk] == MFCO || id[Tk] == MTCO
            ) {
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next(); i = i | ((ival & 0x1F) << 11);
                i = i | (0x10 << 26);
            }
            else if (
                    (id[Tk] >= ADDI && id[Tk] <= XORI) ||
                    (id[Tk] >= SLTI && id[Tk] <= SLTIU)
            ) {
                if      (id[Tk] == ADDI)    i = i | (0x08 << 26);
                else if (id[Tk] == ADDIU)   i = i | (0x09 << 26);
                else if (id[Tk] == ANDI)    i = i | (0x0C << 26);
                else if (id[Tk] == ORI)     i = i | (0x0D << 26);
                else if (id[Tk] == XORI)    i = i | (0x0E << 26);
                else if (id[Tk] == SLTI)    i = i | (0x0A << 26);
                else if (id[Tk] == SLTIU)   i = i | (0x0B << 26);
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next(); i = i | ((ival & 0x1F) << 21);  next();
                next(); 
                i = i | (ival & ((1 << 16) - 1));
            }
            else if (
                    id[Tk] == LUI
            ) {
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next(); i = i | (ival & ((i << 16) - 1));
                i = i | (0x0F << 26);
            }
            else if (
                    id[Tk] >= LW && id[Tk] <= SB
            ) {
                if      (id[Tk] == LW)      i = i | (0x23 << 26);
                else if (id[Tk] == LH)      i = i | (0x21 << 26);
                else if (id[Tk] == LB)      i = i | (0x20 << 26);
                else if (id[Tk] == SW)      i = i | (0x2B << 26);
                else if (id[Tk] == SH)      i = i | (0x29 << 26);
                else if (id[Tk] == SB)      i = i | (0x28 << 26);
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next();
                i = i | (ival & ((1 << 16) - 1));
                next();
                next(); i = i | ((ival & 0x1F) << 21);  next();
            }
            else if (
                    id[Tk] >= BEQ && id[Tk] <= BNE
            ) {
                if      (id[Tk] == BEQ)     i = i | (0x04 << 26);
                else if (id[Tk] == BNE)     i = i | (0x05 << 26);
                next(); i = i | ((ival & 0x1F) << 16);  next();
                next(); i = i | ((ival & 0x1F) << 21);  next();
                next(); 
                if (id[Tk] == Labl) ival = (ival - ((int)e - (int)le) - 4) >> 2;
                i = i | (ival & ((1 << 16) - 1));
            }
            else if (
                    id[Tk] == J || id[Tk] == JAL
            ) {
                i = ((id[Tk] == J ? 0x02 : 3) << 26);
                next();
                if (id[Tk] == Labl) ival = (ival) >> 2;
                i = i | (ival << 6 >> 6);
            }
            else if (
                    id[Tk] >= DB && id[Tk] <= DW
            ) {
                next();
                i = ival;
            }
            *e++ = i;
        }
        else if (tk == Id && id[Tk] == STR) {
            next();
            memcpy((char*)e, buf, ival);
            e = (int*)((int)e + ival + sizeof(int) & -sizeof(int));
        }
        else if (tk == Id && id[Tk] == Labl) {
            next();
        }
        next();
    }

    if ((fd = open(output,
                    O_CREAT | O_WRONLY,
                    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)) < 0) { printf("open returned %d\n", fd); exit(-1); }

    write(fd, le, (int)e - (int)le);
    close(fd);

    free(sym);
    free(buf);
    free(le);

    return 0;
}
