#include "Packer.h"

namespace asmjs
{

void
Out::code(ExprWithImm e, uint8_t imm)
{
    assert(imm < ImmLimit);
    u8(PackOpWithImm(e.raw_code(), imm));
}

void
Out::code(StmtWithImm s, uint8_t imm)
{
    assert(imm < ImmLimit);
    u8(PackOpWithImm(uint8_t(s), imm));
}

void
Out::code(VarTypesWithImm l, uint8_t imm)
{
    assert(imm < ImmLimit);
    u8(PackOpWithImm(uint8_t(l), imm));
}

void
Out::imm_u32(uint32_t u32)
{
    if (u32)
    {
        for (; true; u32 >>= 7)
        {
            if (u32 < 0x80)
            {
                os_.put(u32);
                return;
            }
            os_.put(0x80 | (u32 & 0x7f));
        }
    }
    else
    {
        os_.put(0);
    }
}

void
Out::imm_s32(int32_t s32)
{
    if (s32)
    {
        for (; true; s32 >>= 7)
        {
            if (-64 <= s32 && s32 < 64)
            {
                os_.put(s32 & 0x7f);
                return;
            }
            os_.put(0x80 | (s32 & 0x7f));
        }
    }
    else
    {
        os_.put(0);
    }
}

void
Out::c_str(const char* p)
{
    do
    {
        os_.put(*p);
    }
    while (*p++);
}

} // namespace asmjs
