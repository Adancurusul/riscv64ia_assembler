#include <stdio.h>
#include <string.h>
#include "asm2bin.h"

#define DEBUG 1
#if DEBUG
#define DEBUGPRINT(...) printf(__VA_ARGS__)
#endif

#define TRANS_RD(RD) RD
#define TRANS_RS(RS) RS
#define TRANS_CSR(CSR) CSR
unsigned int a = 0x800 >> 11 & 0b1111;
#define R_TYPE(FUNCT7, RS2, RS1, FUNCT3, RD, OPCODE) (0x00) | (OPCODE) | ((TRANS_RD(RD)) << 7) | ((FUNCT3) << 12) | ((TRANS_RS(RS1)) << 15) | ((TRANS_RS(RS2)) << 20) | ((FUNCT7) << 25)
#define I_TYPE(IMM12, RS1, FUNCT3, RD, OPCODE) (0x00) | (OPCODE) | ((TRANS_RD(RD)) << 7) | ((FUNCT3) << 12) | ((TRANS_RS(RS1)) << 15) | ((IMM12) << 20)
#define S_TYPE(IMM12, RS2, RS1, FUNCT3, OPCODE) (0x00)|(OPCODE)|(IMM12&0xf)<<7|FUNCT3<<12|TRANS_RS(RS1)<<15|TRANS_RS(RS2)<<20|((IMM12>>4)<<25
#define B_TYPE(IMM12, RS2, RS1, FUNCT3, OPCODE) (0x00) | (OPCODE) | ((IMM12 >> 10) & 0x01) << 7 | (IMM12 & 0xf) << 8 | FUNCT3 << 12 | TRANS_RS(RS1) << 15 | TRANS_RS(RS2) << 20 | (IMM12 >> 4 & 0x3f) << 25 | (IMM12 >> 11) << 31
#define U_TYPE(IMM20, RD, OPCODE) (0x00) | (OPCODE) | ((TRANS_RD(RD)) << 7) | ((IMM20) << 12)
#define J_TYPE(IMM20, RD, OPCODE) (0x00) | (POCODE) | ((TRANS_RD(RD)) << 7) | ((((IMM20) >> 11) & 0xff) << 12) | ((((IMM20) >> 10) & 0x01) << 20) | (((IMM20)&0x3ff) << 21) | ((((IMM20) >> 19) & 0x01) << 31)

#define CSR_TYPE(CSR, RS1, FUNCT3, RD, OPCODE) (0x00) | (OPCODE) | ((TRANS_RD(RD)) << 7) | ((FUNCT3) << 12) | ((TRANS_RS(RS1)) << 15) | ((TRANS_CSR(CSR)) << 20)
#define CSRI_TYPE(CSR, IMM5, FUNCT3, RD, OPCODE) (0x00) | (OPCODE) | ((TRANS_RD(RD)) << 7) | ((FUNCT3) << 12) | ((IMM5) << 15) | ((TRANS_CSR(CSR)) << 20)

#define LUI(RD, IMM20) U_TYPE((IMM20), (RD), 0b0110111)
#define AUIPC(RD, IMM20) U_TYPE((IMM20), (RD), 0b0010111)
#define ADD(RD, RS1, RS2) R_TYPE(0b0000000, (RS2), (RS1), 0b000, (RD), 0b0110011)
#define ADDI(RD, RS1, IMM12) I_TYPE((IMM12), (RS1), 0b000, (RD), 0b0010011)
#define SLTI(RD, RS1, IMM12) I_TYPE((IMM12), (RS1), 0b010, (RD), 0b0010011)
#define SLTIU(RD, RS1, IMM12) I_TYPE((IMM12), (RS1), 0b011, (RD), 0b0010011)
#define ANDI(RD, RS1, IMM12) I_TYPE((IMM12), (RS1), 0b111, (RD), 0b0010011)
#define ORI(RD, RS1, IMM12) I_TYPE((IMM12), (RS1), 0b110, RD, 0b0010011)
#define XORI(RD, RS1, IMM12) I_TYPE(IMM12, RS1, 0b100, RD, 0b0010011)
#define SLLI(RD, RS1, IMM12) I_TYPE(IMM12 & 0x3f, RS1, 0b001, RD, 0b0010011)
#define SRLI(RD, RS1, IMM12) I_TYPE(IMM12 & 0x3f, RS1, 0b101, RD, 0b0010011)
#define SRAI(RD, RS1, IMM12) I_TYPE((IMM12 & 0x3f) | 0x400, RS1, 0b101, RD, 0b0010011)
#define JAL(RD, LABLE) J_TYPE(LABLE, RD, 0b1101111)
#define JLAR(RD, RS1, IMM12) I_TYPE(IMM2, RS1, 0b000, RD, 0b1100111)
#define BEQ(RS1, RS2, LABLE) B_TYPE(LABLE, RS2, RS1, 0b000, 0b1100011)
//#define BEQZ(RS1,RS2,LABLE)
#define BNE(RS1, RS2, LABLE) B_TYPE(LABEL, RS2, RS1, 0b001, 0b1100011)
#define BLT(RS1, RS2, LABLE) B_TYPE(LABEL, RS2, RS1, 0b100, 0b1100011)
#define BLTU(RS1, RS2, LABLE) B_TYPE(LABEL, RS2, RS1, 0b110, 0b1100011)
#define BGE(RS1, RS2, LABLE) B_TYPE(LABEL, RS2, RS1, 0b101, 0b1100011)
#define BGEU(RS1, RS2, LABLE) B_TYPE(LABEL, RS2, RS1, 0b111, 0b1100011)
#define SB(RS2, RS1, OFFSET) S_TYPE(OFFSET, RS2, RS1, 0b000, 0b0100011)
#define SH(RS2, RS1, OFFSET) S_TYPE(OFFSET, RS2, RS1, 0b001, 0b0100011)
#define SW(RS2, RS1, OFFSET) S_TYPE(OFFSET, RS2, RS1, 0b010, 0b0100011)
#define SD(RS2, RS1, OFFSET) S_TYPE(OFFSET, RS2, RS1, 0b011, 0b0100011)
#define LB(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b000, RD, 0b0000011)
#define LBU(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b100, RD, 0b0000011)
#define LH(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b001, RD, 0b0000011)
#define LHU(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b101, RD, 0b0000011)
#define LW(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b010, RD, 0b0000011)
#define LWU(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b110, RD, 0b0000011)
#define LD(RD, RS1, OFFSET) I_TYPE(OFFSET, RS1, 0b011, RD, 0b0000011)
#define ADDIW(RD, RS1, IMM12) I_TYPE(IMM12, RS1, 0b000, RD, 0b0011011)
#define SLLIW(RD, RS1, IMM12) I_TYPE(IMM12 & 0x3f, RS1, 0b001, RD, 0b0011011)
#define SRLIW(RD, RS1, IMM12) I_TYPE(IMM12 & 0x3f, RS1, 0b101, RD, 0b0011011)
#define SRAIW(RD, RS1, IMM12) I_TYPE((IMM12 & 0x3f) | 0x400, RS1, 0b101, RD, 0b0011011)
#define ADDW(RD, RS1, RS2) R_TYPE(0b0000000, RS2, RS1, 0b000, RD, 0b0111011)
#define SUBW(RD, RS1, IMM12) R_TYPE(0b0100000, RS2, RS1, 0b000, RD, 0b0111011)
#define SLLW(RD, RS1, IMM12) R_TYPE(0b0000000, RS2, RS1, 0b001, RD, 0b0111011)
#define SRLW(RD, RS1, IMM12) R_TYPE(0b0000000, RS2, RS1, 0b101, RD, 0b0111011)
#define SRAW(RD, RS1, IMM12) R_TYPE(0b0100000, RS2, RS1, 0b101, RD, 0b0111011)
#define FENCE() 0b00000000000000000000000000001111
#define FENCE_I() 0b00000000000000000001000000001111
#define AND(RD, RS1, RS2) R_TYPE(0b0000000, RS2, RS1, 0b111, RD, 0b0110011)
#define AMOSWAP_W(RD, RS2, RS1) R_TYPE(0b0000100, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOSWAP_D(RD, RS2, RS1) R_TYPE(0b0000100, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOADD_W(RD, RS2, RS1) R_TYPE(0b0000000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOADD_D(RD, RS2, RS1) R_TYPE(0b0000000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMAND_W(RD, RS2, RS1) R_TYPE(0b0110000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMAND_D(RD, RS2, RS1) R_TYPE(0b0110000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOOR_W(RD, RS2, RS1) R_TYPE(0b0100000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOOR_D(RD, RS2, RS1) R_TYPE(0b0100000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOXOR_W(RD, RS2, RS1) R_TYPE(0b0010000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOXOR_D(RD, RS2, RS1) R_TYPE(0b0010000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOMAX_W(RD, RS2, RS1) R_TYPE(0b1010000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOMAX_D(RD, RS2, RS1) R_TYPE(0b1010000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOMIN_W(RD, RS2, RS1) R_TYPE(0b1000000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOMIN_D(RD, RS2, RS1) R_TYPE(0b1000000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOMINU_W(RD, RS2, RS1) R_TYPE(0b1100000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOMINU_D(RD, RS2, RS1) R_TYPE(0b1100000, RS2, RS1, 0b011, RD, 0b0101111)
#define AMOMAXU_W(RD, RS2, RS1) R_TYPE(0b1110000, RS2, RS1, 0b010, RD, 0b0101111)
#define AMOMAXU_D(RD, RS2, RS1) R_TYPE(0b1110000, RS2, RS1, 0b011, RD, 0b0101111)
#define LR_W(RD, RS1, RS2) R_TYPE(0b0001000, 0b00000, RS1, 0b010, RD, 0b0101111)
#define LR_D(RD, RS1, RS2) R_TYPE(0b0001000, 0b00000, RS1, 0b011, RD, 0b0101111)
#define SC_W(RD, RS1, RS2) R_TYPE(0b0001100, RS2, RS1, 0b010, RD, 0b0101111)
#define SC_D(RD, RS1, RS2) R_TYPE(0b0001100, RS2, RS1, 0b011, RD, 0b0101111)

#define ECALL() 0b00000000000000000000000001110011
#define EBREAK() 0b00000000000100000000000001110011
#define CSRRW(RD, CSR, RS1) CSR_TYPE(CSR, RS1, 0b001, RD, 0b1110011)
#define CSRRS(RD, CSR, RS1) CSR_TYPE(CSR, RS1, 0b010, RD, 0b1110011)
#define CSRRC(RD, CSR, RS1) CSR_TYPE(CSR, RS1, 0b011, RD, 0b1110011)
#define CSRRWI(RD, CSR, IMM5) CSRI_TYPE(CSR, IMM5, 0b101, RD, 0b1110011)
#define CSRRSI(RD, CSR, IMM5) CSRI_TYPE(CSR, IMM5, 0b110, RD, 0b1110011) ///
#define CSRRCI(RD, CSR, IMM5) CSRI_TYPE(CSR, IMM5, 0b111, RD, 0b1110011)
#define MRET() 0b00110000001000000000000001110011
#define SRET() 0b00010000001000000000000001110011

unsigned int transform_function[] = {};
CORE_DATA immnow;
CORE_DATA renow;
//static INSTRUCTIONS instruction_now = 0;

typedef struct
{
    char *opcode_str;
    INSTRUCTIONS name;
} INSTRUCTIONS_LIST;
typedef struct
{
    char *core_data_str;
    CORE_DATA name;
} CORE_DATA_LIST;

const INSTRUCTIONS_LIST keywords[] = {
    {"LUI", LUI}, {"AUIPC", AUIPC}, {"ADDI", ADDI}, {"SLTI", SLTI}, {"SLTIU", SLTIU}, {"ANDI", ANDI}, {"ORI", ORI}, {"SLLI", SLLI}, {"SRLI", SRLI}, {"SRAI", SRAI}, {"ADD", ADD}, {"SUB", SUB}, {"SLT", SLT}, {"SLTIU", SLTIU}, {"AND", AND}, {"OR", OR}, {"XOR", XOR}, {"SLL", SLL}, {"SRL", SRA}, {"JAL", JAL}, {"JALR", JALR}, {"BEQ", BEQ}, {"BNE", BNE}, {"BLT", BLT}, {"BLTU", BLTU}, {"BGE", BGE}, {"BGEU", BGEU}, {"SB", SB}, {"SH", SH}, {"SW", SW}, {"SD", SD}, {"LB", LB}, {"LBU", LBU}, {"LH", LH}, {"LHU", LHU}, {"LWU", LWU}, {"LD", LD}, {"ADDIW", ADDIW}, {"SLLIW", SLLIW}, {"SRLIW", SRLIW}, {"SRAIW", SRAIW}, {"ADDW", ADDW}, {"SUBW", SUBW}, {"SLLW", SLLW}, {"SRLW", SRLW}, {"SRAW", SRAW}, {"FENCE", FENCE}, {"FENCE.I", FENCE_I}, {"ECALL", ECALL}, {"EBREAK", EBREAK}, {"CSRRW", CSRRW}, {"CSRRS", CSRRS}, {"CSRRC", CSRRC}, {"CSRRWI", CSRRWI}, {"CSRRSI", CSRRSI}, {"CSRRCI", CSRRCI}, {"MRET", MRET}, {"SRET", SRET}, {"AMOADD.D", AMOADD_D}, {"AMOADD.W", AMOADD_W}, {"AMOMAX.D", AMOMAX_D}, {"AMOMAX.W", AMOMAX_W}, {"MRET", MRET}, {"SRET", SRET}, {"LR.W", LR_W}, {"LR.D", LR_D}, {"SC.W", SC_W}, {"SC.D", SC_D}, {NULL, NOTHING}

};

const CORE_DATA_LIST cores[] = {
    {".text", TEXT_BEGIN}, {NULL, NOTHING}};

#define CHANGE_LOWER 'A' - 'a'
#define isdigit(c) ((c) >= '0' && (c) <= '9')
static char const *program_ptr, *ptr, *nextptr, *startptr;
static unsigned char linenum = 0;

static INSTRUCTIONS instruction_now = 0;
static CORE_DATA token_now;
static unsigned int r1_now, r2_now, rs1_now, rs2_now, rd_now, imm_now;
static unsigned int RD, IMM12, IMM20, RS1, RS2;
static int B_TYPE_OFFSET;
//linenum 用于计算地址，发现为操作符时linenum加一
unsigned binary_now;
int error_line;
static CORE_DATA if_one_char()
{
    switch (*ptr)
    {
    case '\n':
        return NEXTLINE;
    case ',':
        return COMMA;
    case ';':
        return SEMICOLON;
    case '(':
        return LEFTBRACKET;
    case ')':
        return RIGHTBRACKET;
    default:
        return 0;
    }
}

static CORE_DATA get_next_token()
{
    INSTRUCTIONS_LIST const *ins_keys;
    CORE_DATA_LIST const *core_d;
    int i;

    if (*ptr == 0)
    {
        return END;
    }
    if (*ptr == ';')
    {
        while (!(*ptr == '\n'))
        {
            nextptr = ptr + 1;
        }
        return NEXTLINE;
    }
    else if (if_one_char())
    {
        nextptr = ptr + 1;
        return if_one_char();
    }
    else if (*ptr == 'x')
    {
        startptr = ptr;
        for (i = 1; i < 4; ++i)
        {
            if (!isdigit(ptr[i]))
            {
                nextptr = ptr + i;
                return REGISTER;
            }
        }
    }
    else if (isdigit(*ptr))
    {
        CORE_DATA state = NUM10;

        for (i = 0; i < 10; ++i)
        {
            if (!isdigit(ptr[i]) && ptr[i])
            {
                if (ptr[i] == 'x' && i == 1)
                {
                    state = NUM16;
                }
                if (i > 0)
                {
                    nextptr = ptr + i;
                    DEBUGPRINT("get an imm");
                    return state;
                }
            }
        }
    }
    else
    {
        for (ins_keys = keywords; ins_keys->opcode_str != NULL; ++ins_keys)
        {
            if (strncmp(ptr, ins_keys->opcode_str, strlen(ins_keys->opcode_str)) == 0)
            {
                nextptr = ptr + strlen(ins_keys->opcode_str);
                instruction_now = ins_keys->name;
                DEBUGPRINT("get opcode: %s \n", ins_keys->opcode_str);
                return INSTRUCTION_WORD;
            }
        }
        for (core_d = cores; core_d->core_data_str != NULL; ++core_d)
        {
            if (strncmp(ptr, core_d->core_data_str, strlen(core_d->core_data_str)) == 0)
            {
                nextptr = ptr + strlen(core_d->core_data_str);

                return core_d->name;
            }
        }
    }
}

void search_init(const char *program)
{
    ptr = program;
    token_now = get_next_token();
}
CORE_DATA search_token()
{
    return token_now;
}
INSTRUCTIONS search_instruction()
{
    return instruction_now;
}

int search_finished()
{
    return *ptr == 0 || token_now == END;
}

void search_next()
{
    if (search_finished())
    {
        return;
    }
    ptr = nextptr;
    while (*ptr == ' ')
    {
        ++ptr;
    }
    token_now = get_next_token();
    return;
}

void transform_init(char pro[])
{
    register int length = strlen(pro);
    for (int i = 0; i < length; i++)
    {
        if (pro[i] >= 'A' && pro[i] <= 'Z')
        {
            pro[i] -= CHANGE_LOWER;
            //MY_PRINT("%c",pro[i]);
            //while (1){

            //}
        }
    }
    char *program = pro;
    program_ptr = program;
    search_init(program);
}

static void accept_token(int token)
{
    if (token != search_token())
    {
        DEBUGPRINT("wrong token");
    }
    search_next();
}
void do_transform()
{
    if (search_finished())
    {
        DEBUGPRINT("nothing here!");
        return;
    }

    accept_token(TEXT_BEGIN);
    accept_token(NEXTLINE);

    handler();
}
static void r_type_handler()
{
    RD = atoi(ptr + 1);
    accept_token(REGISTER);

    DEBUGPRINT("RD NOW: %d\n", RD);
    accept_token(COMMA);
    RS1 = atoi(ptr + 1);
    accept_token(REGISTER);

    DEBUGPRINT("RS1 NOW: %d\n", RS1);
    accept_token(COMMA);
    renow = search_token();
    if (renow == REGISTER)
    {
        RS2 = atoi(ptr + 1);
        accept_token(REGISTER);
    }
    else if (renow == LEFTBRACKET)
    {
        accept_token(LEFTBRACKET);
        RS2 = atoi(ptr + 1);
        accept_token(REGISTER);
        accept_token(RIGHTBRACKET);
    }
    DEBUGPRINT("RS2 NOW: %d\n", RS2);

    accept_token(NEXTLINE);
    linenum++;
    switch (instruction_now)
    {
    case ADD:
        binary_now = ADD(RD, RS1, RS2);
        break;
    case ADDW:
        binary_now = ADDW(RD, RS1, RS2);
        break;
    case SUBW:
        binary_now = SUBW(RD, RS1, RS2);
        break;
    case SLLW:
        binary_now = SLLW(RD, RS1, RS2);
        break;
    case SRLW:
        binary_now = SRLW(RD, RS1, RS2);
        break;
    case SRAW:
        binary_now = SRAW(RD, RS1, RS2);
        break;
    case AND:
        binary_now = AND(RD, RS1, RS2);
        break;
    case AMOADD_D:
        binary_now = AMOADD_D(RD, RS1, RS2);
        break;
    case AMOADD_W:
        binary_now = AMOADD_W(RD, RS1, RS2);
        break;
    case AMOMAX_D:
        binary_now = AMOMAX_D(RD, RS1, RS2);
        break;
    case AMOMAX_W:
        binary_now = AMOMAX_W(RD, RS1, RS2);
        break;
    case AMOMIN_D:
        binary_now = AMOMIN_D(RD, RS1, RS2);
        break;
    case AMOMIN_W:
        binary_now = AMOMIN_W(RD, RS1, RS2);
        break;
    case AMOMAXU_D:
        binary_now = AMOMAXU_D(RD, RS1, RS2);
        break;
    case AMOMAXU_W:
        binary_now = AMOMAXU_W(RD, RS1, RS2);
        break;
    case AMOMINU_D:
        binary_now = AMOMINU_D(RD, RS1, RS2);
        break;
    case AMOMINU_W:
        binary_now = AMOMINU_W(RD, RS1, RS2);
        break;
    case AMOOR_D:
        binary_now = AMOOR_D(RD, RS1, RS2);
        break;
    case AMOOR_W:
        binary_now = AMOOR_W(RD, RS1, RS2);
        break;
    case AMOSWAP_D:
        binary_now = AMOSWAP_D(RD, RS1, RS2);
        break;
    case AMOSWAP_W:
        binary_now = AMOSWAP_W(RD, RS1, RS2);
        break;
    case AMOXOR_D:
        binary_now = AMOXOR_D(RD, RS1, RS2);
        break;
    case AMOXOR_W:
        binary_now = AMOXOR_W(RD, RS1, RS2);
        break;
    case LR_D:
        binary_now = LR_D(RD, RS1, RS2);
        break;
    case LR_W:
        binary_now = LR_W(RD, RS1, RS2);
        break;
    case SC_D:
        binary_now = SC_D(RD, RS1, RS2);
        break;
    case SC_W:
        binary_now = SC_W(RD, RS1, RS2);
        break;
    default:
        DEBUGPRINT("nothing\n");
        break;
    };
}
static void u_type_handler()
{
    RD = atoi(ptr + 1);
    accept_token(REGISTER);

    DEBUGPRINT("RD NOW: %d\n", RD);
    accept_token(COMMA);
    immnow = search_token();
    if (immnow == NUM16)
    {
        IMM20 = htoi(ptr);
    }
    else if (immnow == NUM10)
    {
        IMM20 = atoi(ptr);
    }
    accept_token(NEXTLINE);
    DEBUGPRINT("IMM20\n");
    linenum++;

    switch (instruction_now)
    {
    case LUI:
        binary_now = LUI(RD, IMM20);
        break;
    case AUIPC:
        binary_now = AUIPC(RD, IMM20);
        break;
    default:
        DEBUGPRINT("NOTHING\n");
        break;
    }
}
static void i_type_handler()
{
    RD = atoi(ptr + 1);
    DEBUGPRINT("RD NOW: %d\n", RD);
    accept_token(REGISTER);
    accept_token(COMMA);
    immnow = search_token();
    if (immnow == REGISTER)
    {
        RS1 = atoi(ptr + 1);
        accept_token(REGISTER);
        accept_token(COMMA);
        immnow = search_token();
        if (immnow == NUM16)
        {
            IMM12 = htoi(ptr);
            accept_token(NUM16);
        }
        else if (immnow == NUM10)
        {
            IMM12 = atoi(ptr);

            accept_token(NUM10);
        }
        else
        {
            DEBUGPRINT("no num found\n");
        }
        accept_token(NEXTLINE);
    }
    else if (immnow == NUM16)
    {
        IMM12 = htoi(ptr + 1);
        accept_token(NUM16);
        accept_token(LEFTBRACKET);
        RS1=atoi(ptr+1);
        accept_token(REGISTER);
        accept_token(RIGHTBRACKET);
        accept_token(NEXTLINE);
    }
    else if (immnow == NUM10)
    {
        IMM12 = atoi(ptr + 1);
        accept_token(NUM16);
        accept_token(LEFTBRACKET);
        RS1=atoi(ptr+1);
        accept_token(REGISTER);
        accept_token(RIGHTBRACKET);
        accept_token(NEXTLINE);
    }
    else{
        DEBUGPRINT("nothing found\n");
        
    }
    linenum++;


    switch (instruction_now)
    {
        case ADDI:
        binary_now = ADDI(RD,RS1,IMM12);
        break;
        case SLTI:
        binary_now = SLTI(RD,RS1,IMM12);
        break;
        case SLTIU:
        binary_now = SLTIU(RD,RS1,IMM12);
        break;
        case ANDI:
        binary_now = ANDI(RD,RS1,IMM12);
        break;
        case ORI:
        binary_now = ORI(RD,RS1,IMM12);
        break;
        case SLLI:
        binary_now = SLLI(RD,RS1,IMM12);
        break;
        case SRLI:
        binary_now = SRLI(RD,RS1,IMM12);
        break;
        case SRAI:
        binary_now = SRAI(RD,RS1,IMM12);
        break;
        case LB:
        binary_now = LB(RD,RS1,IMM12);
        break;
        case LBU:
        binary_now = LBU(RD,RS1,IMM12);
        break;
        case LH:
        binary_now = LH(RD,RS1,IMM12);
        break;
        case LHU:
        binary_now = LHU(RD,RS1,IMM12);
        break;
        case LW:
        binary_now = LW(RD,RS1,IMM12);
        break;
        case LWU:
        binary_now = LWU(RD,RS1,IMM12);
        break;
        case LD:
        binary_now = LD(RD,RS1,IMM12);
        break;
        case ADDIW:
        binary_now = ADDIW(RD,RS1,IMM12);
        break;
        case SLLIW:
        binary_now = SLLIW(RD,RS1,IMM12);
        break;
        case SRLIW:
        binary_now = SRLIW(RD,RS1,IMM12);
        break;
        case SRAIW:
        binary_now = SRAIW(RD,RS1,IMM12);
        break;
    
    default:
    DEBUGPRINT("nothing i_type\n");
        break;
    }





}
static void b_type_handler()
{
    if(get_ge<0){
        
    }
}
static void s_type_handler()
{
}
static void handler()
{
    register INSTRUCTIONS instruction = search_instruction();

    for (int i = 0; r_type_instructions[i] != NOTHING; i++)
    {
        if (instruction == r_type_instructions[i])
        {
            r_type_handler();
            return;
        }
    }
    for (int i = 0; u_type_instructions[i] != NOTHING; i++)
    {
        if (instruction == u_type_instructions[i])
        {
            u_type_handler();
            return;
        }
    }
    for (int i = 0; i_type_instructions[i] != NOTHING; i++)
    {
        if (instruction == i_type_instructions[i])
        {
            i_type_handler();
            return;
        }
    }
    for (int i = 0; b_type_instructions[i] != NOTHING; i++)
    {
        if (instruction == b_type_instructions[i])
        {
            b_type_handler();
            return;
        }
    }
    for (int i = 0; s_type_instructions[i] != NOTHING; i++)
    {
        if (instruction == r_type_instructions[i])
        {
            s_type_handler();
            return;
        }
    }
}
