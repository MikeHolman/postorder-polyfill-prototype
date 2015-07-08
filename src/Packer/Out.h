#pragma once

namespace asmjs
{
    template <class To, class From>
    To
        union_cast(From from)
    {
        static_assert(sizeof(To) == sizeof(From), "Sizes should match");
        union {
            From from;
            To to;
        } u = { from };
        return u.to;
    }

    class Out
    {
    private:
        std::ostream& os_;
        template <class T> void u8(T t) { os_.put(uint8_t(t)); }
    public:
        Out(std::ostream& os) : os_(os)
        {
        }
        template <class T> void fixed_width(T);
        void code(Stmt s) { u8(s); }
        void code(SwitchCase c) { u8(c); }
        void code(I32 i) { assert(i < I32::Bad); u8(i); }
        void code(F32 f) { assert(f < F32::Bad); u8(f); }
        void code(F64 f) { assert(f < F64::Bad); u8(f); }
        void code(Void v) { assert(v < Void::Bad); u8(v); }
        void code(Expr e) { u8(e.raw_code()); }
        void code(ExportFormat f) { u8(f); }
        void code(Type t) { u8(t); }
        void code(RType t) { u8(t); }
        void code(VarTypes t) { u8(t); }
        void code(ExprWithImm, uint8_t);
        void code(StmtWithImm, uint8_t);
        void code(VarTypesWithImm, uint8_t);
        void imm_u32(uint32_t u32);
        void imm_s32(int32_t s32);
        void c_str(const char*);

    };
    template<class T>
    void Out::fixed_width(T)
    {
        union {
            T t;
            uint8_t arr[sizeof(T)];
        } u;
        for (auto u8 : u.arr)
            os_.put(u8);
    }
}
