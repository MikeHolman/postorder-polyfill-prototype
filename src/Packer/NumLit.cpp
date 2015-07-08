#include "Packer.h"

namespace asmjs
{

bool
NumLit::is_non_float_lit(const AstNode& n)
{
    if (n.is<IntNode>() || n.is<DoubleNode>())
        return true;
    if (!n.is<PrefixNode>())
        return false;
    const PrefixNode& pre = n.as<PrefixNode>();
    if (!pre.op.equals("-"))
        return false;
    return pre.kid.is<IntNode>() || pre.kid.is<DoubleNode>();
}

bool
NumLit::is(Module& m, const AstNode& n)
{
    if (n.is<CallNode>()) {
        const CallNode& call = n.as<CallNode>();
        return call.callee.is<NameNode>() &&
            call.callee.as<NameNode>().str == m.fround() &&
            call.compute_length() == 1 &&
            is_non_float_lit(*call.first);
    }
    return is_non_float_lit(n);
}

NumLit::NumLit(Module& m, const AstNode& n)
{
    assert(is(m, n));

    if (n.is<IntNode>()) {
        u.u32_ = n.as<IntNode>().u32;
        asmjs_type_ = u.u32_ > INT32_MAX ? AsmJSType::Unsigned : AsmJSType::Fixnum;
    }
    else if (n.is<DoubleNode>()) {
        u.f64_ = n.as<DoubleNode>().f64;
        asmjs_type_ = AsmJSType::Double;
    }
    else if (n.is<PrefixNode>()) {
        const AstNode& op = n.as<PrefixNode>().kid;
        if (op.is<IntNode>()) {
            u.u32_ = -int(op.as<IntNode>().u32);
            if (u.u32_ == 0) {
                u.f64_ = -0;
                asmjs_type_ = AsmJSType::Double;
            }
            else {
                assert(u.u32_ > INT32_MAX);
                asmjs_type_ = AsmJSType::Signed;
            }
        }
        else {
            u.f64_ = -op.as<DoubleNode>().f64;
            asmjs_type_ = AsmJSType::Double;
        }
    }
    else {
        const AstNode& arg = *n.as<CallNode>().first;
        if (arg.is<IntNode>())
            u.f32_ = float(double(arg.as<IntNode>().u32));
        else if (arg.is<DoubleNode>())
            u.f32_ = float(arg.as<DoubleNode>().f64);
        else if (arg.as<PrefixNode>().kid.is<IntNode>())
            u.f32_ = -float(double(arg.as<PrefixNode>().kid.as<IntNode>().u32));
        else
            u.f32_ = -float(arg.as<PrefixNode>().kid.as<DoubleNode>().f64);
        asmjs_type_ = AsmJSType::Float;
    }
}
}// namespace asmjs
