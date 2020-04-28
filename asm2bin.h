unsigned int bin_list[];




typedef enum{
LUI=0,AUIPC,
ADDI,SLTI,SLTIU,ANDI,ORI,XORI,SLLI,SRLI,SRAI,
ADD,SUB,SLT,SLTIU,AND,OR,XOR,SLL,SRL,SRA,
JAL,JALR,BEQ,BNE,BLT,BLTU,BGE,BGEU,
SB,SH,SW,SD,LB,LBU,LH,LHU,LW,LWU,LD,
ADDIW,SLLIW,SRLIW,SRAIW,ADDW,SUBW,SLLW,SRLW,SRAW,
FENCE,FENCE_I,LR_D,LR_W,SC_D,SC_W,
ECALL,EBREAK,CSRRW,CSRRS,CSRRC,CSRRWI,CSRRSI,CSRRCI,MRET,SRET,
AMOADD_D,AMOADD_W,AMOMAX_D,AMOMAX_W,AMOMAXU_D,AMOMAXU_W,AMOMIN_D,AMOMIN_W,AMOMINU_D,AMOMINU_W,AMOOR_D,AMOOR_W,AMOSWAP_D,AMOSWAP_W
,AMOXOR_D,AMOXOR_W,NOTHING
}INSTRUCTIONS;
typedef  enum{
    INSTRUCTION_WORD=100,
    COMMA,
    SEMICOLON,
    NEXTLINE,
    REGISTER,
    NUM16,
    NUM10,
     LEFTBRACKET,
    RIGHTBRACKET,
    END,
    TEXT_BEGIN,
}CORE_DATA;


const INSTRUCTIONS r_type_instructions[]={
ADD,ADDW,SUBW,SLLW,SRLW,SRAW,AND,AMOADD_D,AMOADD_W,AMOMAX_D,AMOMAX_W,AMOMAXU_D,AMOMAXU_W,AMOMIN_D,AMOMIN_W,AMOMINU_D,AMOMINU_W,AMOOR_D,AMOOR_W,AMOSWAP_D,AMOSWAP_W
,AMOXOR_D,AMOXOR_W,LR_D,LR_W,SC_D,SC_W,NOTHING
};



int tolower(int c)  
{  
    if (c >= 'A' && c <= 'Z')  
    {  
        return c + 'a' - 'A';  
    }  
    else  
    {  
        return c;  
    }  
}

int htoi(char s[])  
{  
    int i = 0;  
    int n = 0;  
    if (s[0] == '0' && (s[1]=='x' || s[1]=='X'))  
    {  
        i = 2;  
    }  
    else  
    {  
        i = 0;  
    }  
    for (; (s[i] >= '0' && s[i] <= '9') || (s[i] >= 'a' && s[i] <= 'z') || (s[i] >='A' && s[i] <= 'Z');++i)  
    {  
        if (tolower(s[i]) > '9')  
        {  
            n = 16 * n + (10 + tolower(s[i]) - 'a');  
        }  
        else  
        {  
            n = 16 * n + (tolower(s[i]) - '0');  
        }  
    }  
    return n;  
} 
int atoi(const char *src)
{
    //assert(NULL != src);
    int _num = 0;
    int _sign = 0;
    while ('0' == *src || ' ' == *src || '\n' == *src || '-' == *src || '+' == *src) //如果有空,空格或者换行跳过去
    {
        if (*src == '-')
            _sign = 1;

        src++;
    }

    while (*src >= '0' && *src <= '9')
    {
        _num = _num * 10 + *src - '0';
        src++;
    }

    if (_sign == 1)
        return -_num;
    else
        return _num;
}


const INSTRUCTIONS u_type_instructions[]={
    LUI,AUIPC,
};

const INSTRUCTIONS i_type_instructions[]={
ADDI,SLTI,SLTIU,ANDI,ORI,XORI,SLLI,SRLI,SRAI,LB,LBU,LH,LHU,LW,LWU,LD,
ADDIW,SLLIW,SRLIW,SRAIW
};
const INSTRUCTIONS b_type_instructions[]={
    BEQ,BNE,BLT,BLTU,BGE,BGEU,
};
const INSTRUCTIONS s_type_instructions[]={
    SB,SH,SW,SD,
}