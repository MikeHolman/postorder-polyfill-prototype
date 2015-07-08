#pragma once

namespace asmjs
{
    class NumLit
    {
        AsmJSType asmjs_type_;
        union {
            uint32_t u32_;
            float f32_;
            double f64_;
        } u;

        static bool is_non_float_lit(const AstNode& n);

    public:
        static bool is(Module& m, const AstNode& n);
        explicit NumLit(Module& m, const AstNode& n);
        explicit NumLit(double f64) : asmjs_type_(AsmJSType::Double) { u.f64_ = f64; }

        AsmJSType asmjs_type() const { return asmjs_type_; }
        Type type() const { return asmjs_type_.type(); }

        uint32_t uint32() const { assert(asmjs_type_.is_int()); return u.u32_; }
        int32_t int32() const { assert(asmjs_type_.is_int()); return u.u32_; }
        float float32() const { assert(asmjs_type_.is_float()); return u.f32_; }
        double float64() const { assert(asmjs_type_.is_double()); return u.f64_; }
    };
}
