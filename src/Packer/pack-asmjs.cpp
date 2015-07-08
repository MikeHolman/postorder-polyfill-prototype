// vim: set ts=2 sw=2 tw=99 et:

#include "Packer.h"

using namespace std;
using cashew::IString;

namespace asmjs
{

    // =================================================================================================
    // Analysis (first) pass

void
analyze_heap_ctor(Module& m, const VarNameNode& var)
{
    const CallNode& call = var.init.as<NewNode>().call;
    assert(call.first == call.last);
    assert(call.first->as<NameNode>().str == m.buffer());

    DotNode& dot = call.callee.as<DotNode>();
    assert(dot.base.as<NameNode>().str == m.stdlib());

    if (dot.name.equals("Int8Array"))
        m.add_heap_view(var.name, HeapView(0, Signed));
    else if (dot.name.equals("Uint8Array"))
        m.add_heap_view(var.name, HeapView(0, Unsigned));
    else if (dot.name.equals("Int16Array"))
        m.add_heap_view(var.name, HeapView(1, Signed));
    else if (dot.name.equals("Uint16Array"))
        m.add_heap_view(var.name, HeapView(1, Unsigned));
    else if (dot.name.equals("Int32Array"))
        m.add_heap_view(var.name, HeapView(2, Signed));
    else if (dot.name.equals("Uint32Array"))
        m.add_heap_view(var.name, HeapView(2, Unsigned));
    else if (dot.name.equals("Float32Array"))
        m.add_heap_view(var.name, HeapView(Type::F32, 2));
    else if (dot.name.equals("Float64Array"))
        m.add_heap_view(var.name, HeapView(Type::F64, 3));
    else
        abort();
}

void
analyze_import(Module& m, IString name, DotNode& dot)
{
    if (dot.base.is<DotNode>()) {
        assert(dot.base.as<DotNode>().base.as<NameNode>().str == m.stdlib());
        assert(dot.base.as<DotNode>().name.equals("Math"));
        if (dot.name.equals("imul"))
            m.add_stdlib_func(name, I32::Mul);
        else if (dot.name.equals("clz32"))
            m.add_stdlib_func(name, I32::Clz);
        else if (dot.name.equals("fround"))
            m.add_fround(name);
        else if (dot.name.equals("min"))
            m.add_stdlib_func(name, PreTypeSignCode::Min);
        else if (dot.name.equals("max"))
            m.add_stdlib_func(name, PreTypeSignCode::Max);
        else if (dot.name.equals("abs"))
            m.add_stdlib_func(name, PreTypeCode::Abs);
        else if (dot.name.equals("ceil"))
            m.add_stdlib_func(name, PreTypeCode::Ceil);
        else if (dot.name.equals("floor"))
            m.add_stdlib_func(name, PreTypeCode::Floor);
        else if (dot.name.equals("sqrt"))
            m.add_stdlib_func(name, PreTypeCode::Sqrt);
        else if (dot.name.equals("cos"))
            m.add_stdlib_func(name, F64::Cos);
        else if (dot.name.equals("sin"))
            m.add_stdlib_func(name, F64::Sin);
        else if (dot.name.equals("tan"))
            m.add_stdlib_func(name, F64::Tan);
        else if (dot.name.equals("acos"))
            m.add_stdlib_func(name, F64::ACos);
        else if (dot.name.equals("asin"))
            m.add_stdlib_func(name, F64::ASin);
        else if (dot.name.equals("atan"))
            m.add_stdlib_func(name, F64::ATan);
        else if (dot.name.equals("atan2"))
            m.add_stdlib_func(name, F64::ATan2);
        else if (dot.name.equals("exp"))
            m.add_stdlib_func(name, F64::Exp);
        else if (dot.name.equals("log"))
            m.add_stdlib_func(name, F64::Ln);
        else if (dot.name.equals("pow"))
            m.add_stdlib_func(name, F64::Pow);
        else
            unreachable<void>();
    }
    else if (dot.base.as<NameNode>().str == m.stdlib()) {
        if (dot.name.equals("NaN"))
            m.add_stdlib_double(name, NAN);
        else if (dot.name.equals("Infinity"))
            m.add_stdlib_double(name, INFINITY);
        else
            unreachable<void>();
    }
    else if (dot.base.as<NameNode>().str == m.foreign()) {
        m.add_func_import(name, dot.name);
    }
    else {
        unreachable<void>();
    }
}

AstNode*
analyze_global_definitions(Module& m, AstNode* stmt)
{
    vector<IString> i32_zero;
    vector<IString> f32_zero;
    vector<IString> f64_zero;
    vector<pair<IString, IString>> i32_import;
    vector<pair<IString, IString>> f32_import;
    vector<pair<IString, IString>> f64_import;

    for (; stmt && stmt->is<VarNode>(); stmt = stmt->next) {
        for (VarNameNode* var = stmt->as<VarNode>().first; var; var = var->next) {
            if (NumLit::is(m, var->init)) {
                NumLit lit(m, var->init);
                switch (lit.type()) {
                case Type::I32:
                    assert(lit.uint32() == 0);
                    i32_zero.push_back(var->name);
                    break;
                case Type::F32:
                    assert(lit.float32() == 0);
                    f32_zero.push_back(var->name);
                    break;
                case Type::F64:
                    assert(lit.float64() == 0);
                    f64_zero.push_back(var->name);
                    break;
                }
            }
            else {
                switch (var->init.which) {
                case AstNode::New:
                    analyze_heap_ctor(m, *var);
                    break;
                case AstNode::Dot:
                    analyze_import(m, var->name, var->init.as<DotNode>());
                    break;
                case AstNode::Prefix: {
                    auto& dot = var->init.as<PrefixNode>().kid.as<DotNode>();
                    assert(dot.base.as<NameNode>().str == m.foreign());
                    f64_import.emplace_back(var->name, dot.name);
                    break;
                }
                case AstNode::Binary: {
                    auto& binary = var->init.as<BinaryNode>();
                    assert(binary.op.equals("|"));
                    assert(binary.rhs.as<IntNode>().u32 == 0);
                    auto& dot = binary.lhs.as<DotNode>();
                    assert(dot.base.as<NameNode>().str == m.foreign());
                    i32_import.emplace_back(var->name, dot.name);
                    break;
                }
                case AstNode::Call: {
                    auto& call = var->init.as<CallNode>();
                    assert(call.callee.as<NameNode>().str == m.fround());
                    assert(call.compute_length() == 1);
                    auto& dot = call.first->as<DotNode>();
                    assert(dot.base.as<NameNode>().str == m.foreign());
                    f32_import.emplace_back(var->name, dot.name);
                    break;
                }
                default:
                    unreachable<void>();
                }
            }
        }
    }

    m.set_globals(i32_zero, f32_zero, f64_zero, i32_import, f32_import, f64_import);
    return stmt;
}

Type
extract_arg_type(Module& m, const ArgNode& arg, const AstNode& stmt)
{
    const BinaryNode& assign = stmt.as<BinaryNode>();
    assert(assign.op.equals("="));
    assert(assign.lhs.as<NameNode>().str == arg.name);

    AstNode& coercion = assign.rhs;

    if (coercion.is<BinaryNode>()) {
        assert(coercion.as<BinaryNode>().op.equals("|"));
        assert(coercion.as<BinaryNode>().rhs.as<IntNode>().u32 == 0);
        assert(coercion.as<BinaryNode>().lhs.as<NameNode>().str == arg.name);
        return Type::I32;
    }

    if (coercion.is<CallNode>()) {
        assert(coercion.as<CallNode>().callee.as<NameNode>().str == m.fround());
        assert(coercion.as<CallNode>().compute_length() == 1);
        assert(coercion.as<CallNode>().first->as<NameNode>().str == arg.name);
        return Type::F32;
    }

    assert(coercion.as<PrefixNode>().op.equals("+"));
    assert(coercion.as<PrefixNode>().kid.as<NameNode>().str == arg.name);
    return Type::F64;
}

Type
extract_var_init(Module& m, const AstNode& init)
{
    if (init.is<NameNode>())
        return m.global(init.as<NameNode>().str).type;

    NumLit lit(m, init);

    assert((lit.type() == Type::I32 && lit.uint32() == 0) ||
        (lit.type() == Type::F32 && lit.float32() == 0) ||
        (lit.type() == Type::F64 && lit.float64() == 0));

    return lit.type();
}

AsmJSType analyze_expr(Module&, Function&, AstNode&);
RType analyze_call(Module& m, Function& f, CallNode& call, RType ret_type, AstNode* coercion = nullptr);

AsmJSType
analyze_num_lit(Module& m, NumLit lit, unsigned lshift = 0)
{
    m.add_lit(lit, lshift);
    return lit.asmjs_type();
}

AsmJSType
analyze_stdlib_call(Module& m, Function& f, CallNode& call)
{
    IString callee = call.callee.as<NameNode>().str;

    assert(call.expr.is_bad());

    if (callee == m.fround() && call.compute_length() == 1 && call.first->is<CallNode>()) {
        call.kind = CallNode::Fround;
        analyze_call(m, f, call.first->as<CallNode>(), RType::F32, &call);
        return AsmJSType::Float;
    }

    vector<AsmJSType> args;
    for (AstNode* arg = call.first; arg; arg = arg->next)
        args.push_back(analyze_expr(m, f, *arg));

    StdLibFunc stdlib = m.stdlib_func(callee);
    switch (stdlib.which()) {
    case StdLibFunc::Mono:
        call.kind = CallNode::FixedArityBuiltin;
        call.expr = stdlib.mono();
        switch (stdlib.mono().type()) {
        case RType::I32:
            switch (stdlib.mono().i32()) {
            case I32::Mul: return AsmJSType::Signed;
            case I32::Clz: return AsmJSType::Fixnum;
            default: unreachable<void>();
            }
        case RType::F64:
            return AsmJSType::Double;
        default:
            unreachable<void>();
        }
    case StdLibFunc::Poly:
        switch (stdlib.poly()) {
        case PreTypeCode::Ceil:
        case PreTypeCode::Floor:
        case PreTypeCode::Sqrt:
            assert(args.size() == 1);
            assert(args[0].is_double() || args[0].is_float());
            call.kind = CallNode::FixedArityBuiltin;
            call.expr = specialize(stdlib.poly(), args[0].type());
            return args[0].is_float() ? AsmJSType::Float : AsmJSType::Double;
        case PreTypeCode::Abs:
            assert(args.size() == 1);
            assert(args[0].is_signed() || args[0].is_float() || args[0].is_double());
            call.kind = CallNode::FixedArityBuiltin;
            call.expr = specialize(stdlib.poly(), args[0].type());
            return type_switch(args[0].type(), AsmJSType::Unsigned, AsmJSType::Float, AsmJSType::Double);
        default:
            unreachable<void>();
        }
    case StdLibFunc::SignedPoly:
        switch (stdlib.signed_poly()) {
        case PreTypeSignCode::Min:
        case PreTypeSignCode::Max:
            assert(args.size() >= 1);
            call.kind = CallNode::NaryBuiltin;
            for (AsmJSType t : args)
                assert(t.type() == args[0].type() && (t.is_signed() || t.is_double()));
            if (args[0].is_double()) {
                call.expr = specialize(stdlib.signed_poly(), Type::F64);
                return AsmJSType::Double;
            }
            else {
                call.expr = specialize(stdlib.signed_poly(), Signed);
                return AsmJSType::Signed;
            }
        case PreTypeSignCode::ToF32:
            assert(args.size() == 1);
            call.kind = CallNode::Fround;
            if (args[0].is_signed())
                call.expr = specialize(PreTypeSignCode::ToF32, Signed);
            else if (args[0].is_unsigned())
                call.expr = specialize(PreTypeSignCode::ToF32, Unsigned);
            else
                call.expr = specialize(PreTypeSignCode::ToF32, args[0].type());
            return AsmJSType::Float;
        default:
            unreachable<void>();
        }
    }

    return unreachable<AsmJSType>();
}

void
set_coercion_node_expr(AstNode* coercion, Expr expr)
{
    if (coercion->is<PrefixNode>())
        coercion->as<PrefixNode>().expr = expr;
    else
        coercion->as<CallNode>().expr = expr;
}

RType
analyze_coerced_stdlib_call(Module& m, Function& f, CallNode& call, RType ret_type, AstNode* coercion)
{
    AsmJSType t;
    if (NumLit::is(m, call))
        t = analyze_num_lit(m, NumLit(m, call));
    else
        t = analyze_stdlib_call(m, f, call);

    switch (ret_type) {
    case RType::I32:
        assert(t.is_int());
        break;
    case RType::F32:
        if (t.is_signed())
            set_coercion_node_expr(coercion, F32::FromS32);
        else if (t.is_unsigned())
            set_coercion_node_expr(coercion, F32::FromU32);
        else if (t.is_double())
            set_coercion_node_expr(coercion, F32::FromF64);
        else
            assert(t.is_float());
        break;
    case RType::F64:
        if (t.is_signed())
            set_coercion_node_expr(coercion, F64::FromS32);
        else if (t.is_unsigned())
            set_coercion_node_expr(coercion, F64::FromU32);
        else if (t.is_float())
            set_coercion_node_expr(coercion, F64::FromF32);
        else
            assert(t.is_double());
        break;
    case RType::Void:
        ret_type = t.rtype();
    }

    return ret_type;
}

void
analyze_call_import(Module& m, Function& f, CallNode& call, RType ret_type)
{
    call.kind = CallNode::Import;
    call.expr = type_switch(ret_type, I32::CallImp, F32::Bad, F64::CallImp, Void::CallImp);
    call.stmt = Stmt::CallImp;

    Signature sig(ret_type);
    for (AstNode* arg = call.first; arg; arg = arg->next)
        sig.args.push_back(analyze_expr(m, f, *arg).type());

    call.import_preindex = m.add_import_sig(call.callee.as<NameNode>().str, move(sig));
}

void
analyze_call_internal(Module& m, Function& f, CallNode& call, RType ret_type)
{
    assert(call.callee.is<NameNode>());

    call.kind = CallNode::Internal;
    call.expr = type_switch(ret_type, I32::CallInt, F32::CallInt, F64::CallInt, Void::CallInt);
    call.stmt = Stmt::CallInt;

    for (AstNode* arg = call.first; arg; arg = arg->next)
        analyze_expr(m, f, *arg);
}

void
analyze_call_indirect(Module& m, Function& f, CallNode& call, RType ret_type)
{
    call.kind = CallNode::Indirect;
    call.expr = type_switch(ret_type, I32::CallInd, F32::CallInd, F64::CallInd, Void::CallInd);
    call.stmt = Stmt::CallInd;

    assert(call.callee.is<IndexNode>());
    IndexNode& index = call.callee.as<IndexNode>();
    assert(index.array.is<NameNode>());
    assert(index.index->is<BinaryNode>());
    BinaryNode& mask = index.index->as<BinaryNode>();
    assert(mask.op.equals("&"));
    assert(mask.rhs.is<IntNode>());
    analyze_expr(m, f, mask.lhs);
    for (AstNode* arg = call.first; arg; arg = arg->next)
        analyze_expr(m, f, *arg);
}

RType
analyze_call(Module& m, Function& f, CallNode& call, RType ret_type, AstNode* coercion)
{
    if (call.callee.which == AstNode::Name) {
        IString name = call.callee.as<NameNode>().str;
        if (m.is_stdlib_func(name))
            ret_type = analyze_coerced_stdlib_call(m, f, call, ret_type, coercion);
        else if (m.is_import_func(name))
            analyze_call_import(m, f, call, ret_type);
        else
            analyze_call_internal(m, f, call, ret_type);
    }
    else if (call.callee.which == AstNode::Index) {
        analyze_call_indirect(m, f, call, ret_type);
    }
    else {
        unreachable<void>();
    }
    return ret_type;
}


AsmJSType
analyze_to_number(Module& m, Function& f, PrefixNode& prefix)
{
    assert(prefix.expr.is_bad());

    if (is_double_coerced_call(prefix)) {
        analyze_call(m, f, prefix.kid.as<CallNode>(), RType::F64, &prefix);
        return AsmJSType::Double;
    }

    AsmJSType t = analyze_expr(m, f, prefix.kid);
    if (t.is_signed())
        prefix.expr = F64::FromS32;
    else if (t.is_unsigned())
        prefix.expr = F64::FromU32;
    else if (t.is_float())
        prefix.expr = F64::FromF32;
    else
        assert(t.is_double());
    return AsmJSType::Double;
}

AsmJSType
analyze_negate(Module& m, Function& f, PrefixNode& prefix)
{
    AsmJSType t = analyze_expr(m, f, prefix.kid);
    prefix.expr = specialize(PreTypeCode::Neg, t.type());
    return t;
}

AsmJSType
analyze_bitnot(Module& m, Function& f, PrefixNode& prefix)
{
    if (prefix.kid.is<PrefixNode>() && prefix.kid.as<PrefixNode>().op.equals("~")) {
        AsmJSType t = analyze_expr(m, f, prefix.kid.as<PrefixNode>().kid);
        assert(t.is_float() || t.is_double());
        if (t.is_float())
            prefix.expr = I32::FromF32;
        else
            prefix.expr = I32::FromF64;
        return AsmJSType::Signed;
    }

    AsmJSType t = analyze_expr(m, f, prefix.kid);
    assert(t.is_int());
    prefix.expr = I32::BitNot;
    return AsmJSType::Signed;
}

AsmJSType
analyze_not(Module& m, Function& f, PrefixNode& prefix)
{
    AsmJSType t = analyze_expr(m, f, prefix.kid);
    assert(t.is_int());
    prefix.expr = I32::LogicNot;
    return AsmJSType::Int;
}

AsmJSType
analyze_name(Module& m, Function& f, NameNode& name)
{
    if (f.is_local_name(name.str)) {
        name.index = f.local_index(name.str);
        switch (f.local_type(name.index)) {
        case Type::I32: name.expr = I32::GetLoc; name.expr_with_imm = I32WithImm::GetLoc; return AsmJSType::Int;
        case Type::F32: name.expr = F32::GetLoc; name.expr_with_imm = F32WithImm::GetLoc; return AsmJSType::Float;
        case Type::F64: name.expr = F64::GetLoc; name.expr_with_imm = F64WithImm::GetLoc; return AsmJSType::Double;
        }
        return unreachable<AsmJSType>();
    }

    if (m.is_stdlib_double(name.str))
        return analyze_num_lit(m, NumLit(m.stdlib_double(name.str)));

    Global g = m.global(name.str);
    name.index = g.index;
    switch (g.type) {
    case Type::I32: name.expr = I32::GetGlo; return AsmJSType::Int;
    case Type::F32: name.expr = F32::GetGlo; return AsmJSType::Float;
    case Type::F64: name.expr = F64::GetGlo; return AsmJSType::Double;
    }
    return unreachable<AsmJSType>();
}

AsmJSType
analyze_prefix(Module& m, Function& f, PrefixNode& prefix)
{
    if (prefix.op.equals("~"))
        return analyze_bitnot(m, f, prefix);

    assert(strlen(prefix.op.c_str()) == 1);
    switch (prefix.op.c_str()[0]) {
    case '+': return analyze_to_number(m, f, prefix);
    case '-': return analyze_negate(m, f, prefix);
    case '!': return analyze_not(m, f, prefix);
    default:;
    }
    return unreachable<AsmJSType>();
}

AsmJSType
analyze_comma(Module& m, Function& f, BinaryNode& binary)
{
    RType lhs;
    if (binary.lhs.is<CallNode>())
        lhs = analyze_call(m, f, binary.lhs.as<CallNode>(), RType::Void);
    else
        lhs = analyze_expr(m, f, binary.lhs).rtype();
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    binary.expr = specialize(PreTypeCode::Comma, rhs.type());
    binary.kind = BinaryNode::Comma;
    binary.comma_lhs_type = lhs;
    return rhs;
}

HeapView
analyze_index(Module& m, Function& f, IndexNode& index)
{
    HeapView hv = m.heap_view(index.array.as<NameNode>().str);

    AsmJSType t;
    if (index.index->is<IntNode>()) {
        index.constant = true;
        t = analyze_num_lit(m, NumLit(m, *index.index), hv.shift);
    }
    else {
        if (index.index->is<BinaryNode>()) {
            BinaryNode& binary = index.index->as<BinaryNode>();
            if (binary.op.equals(">>") && binary.rhs.is<IntNode>() && binary.rhs.as<IntNode>().u32 == hv.shift)
                index.index = &binary.lhs;
            else
                assert(hv.shift == 0);
        }

        if (index.index->is<BinaryNode>()) {
            BinaryNode& binary = index.index->as<BinaryNode>();
            if (binary.op.equals("+")) {
                if (binary.rhs.is<IntNode>()) {
                    index.offset = binary.rhs.as<IntNode>().u32;
                    index.index = &binary.lhs;
                }
                else if (binary.lhs.is<IntNode>()) {
                    index.offset = binary.lhs.as<IntNode>().u32;
                    index.index = &binary.rhs;
                }
            }
        }

        t = analyze_expr(m, f, *index.index);
    }

    assert(t.is_int());
    return hv;
}

AsmJSType
analyze_load(Module& m, Function& f, IndexNode& index)
{
    HeapView hv = analyze_index(m, f, index);
    switch (hv.type) {
    case Type::I32:
        switch (hv.shift) {
        case 0:
            index.expr = index.offset ? signedness_switch(hv.si, I32::SLoadOff8, I32::ULoadOff8)
                : signedness_switch(hv.si, I32::SLoad8, I32::ULoad8);
            break;
        case 1:
            index.expr = index.offset ? signedness_switch(hv.si, I32::SLoadOff16, I32::ULoadOff16)
                : signedness_switch(hv.si, I32::SLoad16, I32::ULoad16);
            break;
        case 2:
            index.expr = index.offset ? I32::LoadOff32 : I32::Load32;
            break;
        }
        break;
    case Type::F32:
        index.expr = index.offset ? F32::LoadOff : F32::Load;
        break;
    case Type::F64:
        index.expr = index.offset ? F64::LoadOff : F64::Load;
        break;
    }
    return type_switch(hv.type, AsmJSType::Int, AsmJSType::Float, AsmJSType::Double);
}

AsmJSType
analyze_store(Module& m, Function& f, BinaryNode& binary)
{
    IndexNode& index = binary.lhs.as<IndexNode>();
    HeapView hv = analyze_index(m, f, index);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    switch (hv.type) {
    case Type::I32:
        assert(rhs.is_int());
        switch (hv.shift) {
        case 0:
            binary.expr = index.offset ? I32::StoreOff8 : I32::Store8;
            binary.stmt = index.offset ? Stmt::I32StoreOff8 : Stmt::I32Store8;
            break;
        case 1:
            binary.expr = index.offset ? I32::StoreOff16 : I32::Store16;
            binary.stmt = index.offset ? Stmt::I32StoreOff16 : Stmt::I32Store16;
            break;
        case 2:
            binary.expr = index.offset ? I32::StoreOff32 : I32::Store32;
            binary.stmt = index.offset ? Stmt::I32StoreOff32 : Stmt::I32Store32;
            break;
        }
        break;
    case Type::F32:
        assert(rhs.is_float() || rhs.is_double());
        binary.expr = index.offset ? F32::StoreOff : F32::Store;
        binary.stmt = index.offset ? Stmt::F32StoreOff : Stmt::F32Store;
        if (rhs.is_double()) {
            binary.store_rhs_conv = F32::FromF64;
            f.set_needs_f64_temp();
        }
        break;
    case Type::F64:
        assert(rhs.is_float() || rhs.is_double());
        binary.expr = index.offset ? F64::StoreOff : F64::Store;
        binary.stmt = index.offset ? Stmt::F64StoreOff : Stmt::F64Store;
        if (rhs.is_float()) {
            binary.store_rhs_conv = F64::FromF32;
            f.set_needs_f32_temp();
        }
        break;
    }
    binary.kind = BinaryNode::Store;
    return rhs;
}

AsmJSType
analyze_assign(Module& m, Function& f, BinaryNode& binary)
{
    if (binary.lhs.is<IndexNode>())
        return analyze_store(m, f, binary);

    binary.kind = BinaryNode::Assign;

    NameNode& lhs = binary.lhs.as<NameNode>();
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);

    if (f.is_local_name(lhs.str)) {
        lhs.index = f.local_index(lhs.str);
        assert(rhs.type() == f.local_type(lhs.index));
        binary.expr = type_switch(rhs.type(), I32::SetLoc, F32::SetLoc, F64::SetLoc);
        binary.stmt = Stmt::SetLoc;
        binary.stmt_with_imm = StmtWithImm::SetLoc;
        return rhs;
    }

    Global g = m.global(lhs.str);
    lhs.index = g.index;
    assert(rhs.type() == g.type);
    binary.expr = type_switch(rhs.type(), I32::SetGlo, F32::SetGlo, F64::SetGlo);
    binary.stmt = Stmt::SetGlo;
    binary.stmt_with_imm = StmtWithImm::SetGlo;
    return rhs;
}

AsmJSType
analyze_arith(Module& m, Function& f, BinaryNode& binary, PreTypeCode pre)
{
    AsmJSType lhs = analyze_expr(m, f, binary.lhs);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    assert(lhs.type() == rhs.type());
    binary.expr = specialize(pre, lhs.type());
    return lhs.forget_signedness();
}

Expr
specialize(AsmJSType lhs, AsmJSType rhs, PreTypeSignCode pre)
{
    if (lhs.is_int()) {
        if (lhs.is_signed() && rhs.is_signed())
            return specialize(pre, Signed);
        return specialize(pre, Unsigned);
    }
    return specialize(pre, lhs.type());
}

AsmJSType
analyze_div_mod(Module& m, Function& f, BinaryNode& binary, PreTypeSignCode pre)
{
    AsmJSType lhs = analyze_expr(m, f, binary.lhs);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    assert(lhs.type() == rhs.type());
    binary.expr = specialize(lhs, rhs, pre);
    return lhs.forget_signedness();
}

AsmJSType
analyze_eq(Module& m, Function& f, BinaryNode& binary, PreTypeCode pre)
{
    AsmJSType lhs = analyze_expr(m, f, binary.lhs);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    assert(lhs.type() == rhs.type());
    binary.expr = specialize(pre, lhs.type());
    return AsmJSType::Int;
}

AsmJSType
analyze_comp(Module& m, Function& f, BinaryNode& binary, PreTypeSignCode pre)
{
    AsmJSType lhs = analyze_expr(m, f, binary.lhs);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    assert(lhs.type() == rhs.type());
    binary.expr = specialize(lhs, rhs, pre);
    return AsmJSType::Int;
}

bool
is_double_coerced_call(const PrefixNode& prefix)
{
    return prefix.op.equals("+") &&
        prefix.kid.is<CallNode>();
}

bool
is_int_coerced_call(const BinaryNode& binary)
{
    return binary.op.equals("|") &&
        binary.lhs.is<CallNode>() &&
        binary.rhs.is<IntNode>() &&
        binary.rhs.as<IntNode>().u32 == 0;
}

AsmJSType
analyze_bitwise(Module& m, Function& f, BinaryNode& binary, I32 i32)
{
    binary.kind = BinaryNode::Bitwise;

    if (is_int_coerced_call(binary)) {
        analyze_call(m, f, binary.lhs.as<CallNode>(), RType::I32);
        return AsmJSType::Signed;
    }

    AsmJSType lhs = analyze_expr(m, f, binary.lhs);
    AsmJSType rhs = analyze_expr(m, f, binary.rhs);
    assert(lhs.type() == rhs.type());
    assert(lhs.is_int() && rhs.is_int());
    binary.expr = i32;
    return i32 == I32::LogicRsh ? AsmJSType::Unsigned : AsmJSType::Signed;
}

AsmJSType
analyze_binary(Module& m, Function& f, BinaryNode& binary)
{
    switch (binary.op.c_str()[0]) {
    case ',': return analyze_comma(m, f, binary);
    case '+': return analyze_arith(m, f, binary, PreTypeCode::Add);
    case '-': return analyze_arith(m, f, binary, PreTypeCode::Sub);
    case '*': return analyze_arith(m, f, binary, PreTypeCode::Mul);
    case '/': return analyze_div_mod(m, f, binary, PreTypeSignCode::Div);
    case '%': return analyze_div_mod(m, f, binary, PreTypeSignCode::Mod);
    case '|': return analyze_bitwise(m, f, binary, I32::BitOr);
    case '&': return analyze_bitwise(m, f, binary, I32::BitAnd);
    case '^': return analyze_bitwise(m, f, binary, I32::BitXor);
    case '=':
        switch (binary.op.c_str()[1]) {
        case '\0': return analyze_assign(m, f, binary);
        case '=': return analyze_eq(m, f, binary, PreTypeCode::Eq);
        default: unreachable<void>();
        }
    case '!':
        switch (binary.op.c_str()[1]) {
        case '\0': unreachable<void>();
        case '=': return analyze_eq(m, f, binary, PreTypeCode::NEq);
        }
    case '<':
        switch (binary.op.c_str()[1]) {
        case '\0': return analyze_comp(m, f, binary, PreTypeSignCode::LeTh);
        case '=': return analyze_comp(m, f, binary, PreTypeSignCode::LeEq);
        case '<': return analyze_bitwise(m, f, binary, I32::Lsh);
        default: unreachable<void>();
        }
    case '>':
        switch (binary.op.c_str()[1]) {
        case '\0': return analyze_comp(m, f, binary, PreTypeSignCode::GrTh);
        case '=': return analyze_comp(m, f, binary, PreTypeSignCode::GrEq);
        case '>':
            switch (binary.op.c_str()[2]) {
            case '\0': return analyze_bitwise(m, f, binary, I32::ArithRsh);
            case '>': return analyze_bitwise(m, f, binary, I32::LogicRsh);
            default: unreachable<void>();
            }
        }
    default: unreachable<void>();
    }
    return unreachable<AsmJSType>();
}

AsmJSType
analyze_ternary(Module& m, Function& f, TernaryNode& ternary)
{
    AsmJSType cond = analyze_expr(m, f, ternary.cond);
    assert(cond.is_int());
    AsmJSType lhs = analyze_expr(m, f, ternary.lhs);
    AsmJSType rhs = analyze_expr(m, f, ternary.rhs);
    assert(lhs.type() == rhs.type());
    if (lhs.is_int())
        ternary.expr = I32::Cond;
    else if (lhs.is_float())
        ternary.expr = F32::Cond;
    else if (lhs.is_double())
        ternary.expr = F64::Cond;
    else
        unreachable<void>();
    return lhs.forget_signedness();
}

AsmJSType
analyze_expr(Module& m, Function& f, AstNode& expr)
{
    if (NumLit::is(m, expr))
        return analyze_num_lit(m, NumLit(m, expr));

    switch (expr.which) {
    case AstNode::Name: return analyze_name(m, f, expr.as<NameNode>());
    case AstNode::Prefix: return analyze_prefix(m, f, expr.as<PrefixNode>());
    case AstNode::Binary: return analyze_binary(m, f, expr.as<BinaryNode>());
    case AstNode::Ternary: return analyze_ternary(m, f, expr.as<TernaryNode>());
    case AstNode::Index: return analyze_load(m, f, expr.as<IndexNode>());
    case AstNode::Call: return analyze_stdlib_call(m, f, expr.as<CallNode>());
    default: unreachable<void>();
    }
    return unreachable<AsmJSType>();
}

void
analyze_stmt(Module& m, Function& f, AstNode& stmt)
{
    switch (stmt.which) {
    case AstNode::Call:
        analyze_call(m, f, stmt.as<CallNode>(), RType::Void);
        break;
    case AstNode::Prefix:
        analyze_prefix(m, f, stmt.as<PrefixNode>());
        break;
    case AstNode::Binary:
        analyze_binary(m, f, stmt.as<BinaryNode>());
        break;
    case AstNode::Return:
        if (!stmt.as<ReturnNode>().expr)
            f.set_ret_type(RType::Void);
        else
            f.set_ret_type(to_rtype(analyze_expr(m, f, *stmt.as<ReturnNode>().expr).type()));
        break;
    case AstNode::Block:
        for (AstNode* p = stmt.as<BlockNode>().first; p; p = p->next)
            analyze_stmt(m, f, *p);
        break;
    case AstNode::If:
        analyze_expr(m, f, stmt.as<IfNode>().cond);
        analyze_stmt(m, f, stmt.as<IfNode>().if_true);
        if (stmt.as<IfNode>().if_false)
            analyze_stmt(m, f, *stmt.as<IfNode>().if_false);
        break;
    case AstNode::While:
        analyze_expr(m, f, stmt.as<WhileNode>().cond);
        analyze_stmt(m, f, stmt.as<WhileNode>().body);
        break;
    case AstNode::Do:
        analyze_stmt(m, f, stmt.as<DoNode>().body);
        analyze_expr(m, f, stmt.as<DoNode>().cond);
        break;
    case AstNode::Label:
        analyze_stmt(m, f, stmt.as<LabelNode>().stmt);
        break;
    case AstNode::Break:
    case AstNode::Continue:
        break;
    case AstNode::Switch:
        analyze_expr(m, f, stmt.as<SwitchNode>().expr);
        for (CaseNode* c = stmt.as<SwitchNode>().first; c; c = c->next)
            for (CaseStmtNode* s = c->first; s; s = s->next)
                analyze_stmt(m, f, s->stmt);
        break;
    default: unreachable<void>();
    }
}

void
analyze_function_definition(Module& m, const FuncNode& func)
{
    Function& f = m.add_func(func.name);

    AstNode* body = func.List<AstNode>::first;

    for (const ArgNode* arg = func.List<ArgNode>::first; arg; arg = arg->next, body = body->next)
        f.add_arg(arg->name, extract_arg_type(m, *arg, *body));

    vector<IString> i32s, f32s, f64s;
    for (; body && body->is<VarNode>(); body = body->next) {
        for (VarNameNode* var = body->as<VarNode>().first; var; var = var->next) {
            switch (extract_var_init(m, var->init)) {
            case Type::I32: i32s.push_back(var->name); break;
            case Type::F32: f32s.push_back(var->name); break;
            case Type::F64: f64s.push_back(var->name); break;
            }
        }
    }

    f.start_body(body, i32s, f32s, f64s);

    for (; body; body = body->next)
        analyze_stmt(m, f, *body);

    m.finish_func(f);
}

void
analyze_func_ptr_table(Module& m, const VarNode& var)
{
    assert(var.first == var.last);
    VarNameNode& name = *var.first;

    vector<uint32_t> funcs;
    for (AstNode* elem = name.init.as<ArrayNode>().first; elem; elem = elem->next)
        funcs.push_back(m.func_index(elem->as<NameNode>().str));

    m.add_func_ptr_table(name.name, move(funcs));
}

void
analyze_export_stmt(Module& m, const ReturnNode& export_stmt)
{
    if (export_stmt.expr->is<NameNode>()) {
        m.add_default_export(export_stmt.expr->as<NameNode>().str);
    }
    else {
        ObjectNode& export_obj = export_stmt.expr->as<ObjectNode>();
        for (FieldNode* cur_field = export_obj.first; cur_field; cur_field = cur_field->next)
            m.add_record_export(cur_field->key, cur_field->value.as<NameNode>().str);
    }
}

void
analyze_module(Module& m, const FuncNode& module)
{
    const ArgNode* arg1 = module.List<ArgNode>::first;
    m.stdlib(arg1->name);
    const ArgNode* arg2 = arg1->next;
    m.foreign(arg2->name);
    const ArgNode* arg3 = arg2->next;
    m.buffer(arg3->name);
    assert(!arg3->next);

    AstNode* stmt = module.List<AstNode>::first;
    assert(stmt->as<StringNode>().str.equals("use asm"));
    
    stmt = stmt->next;

    stmt = analyze_global_definitions(m, stmt);

    for (; stmt && stmt->is<FuncNode>(); stmt = stmt->next)
        analyze_function_definition(m, stmt->as<FuncNode>());

    for (; stmt && stmt->is<VarNode>(); stmt = stmt->next)
        analyze_func_ptr_table(m, stmt->as<VarNode>());

    analyze_export_stmt(m, stmt->as<ReturnNode>());
    assert(!stmt->next);

    m.finish_analysis();
}

// =================================================================================================
// Write (second) pass

void
write_signature_section(Module& m)
{
    m.write().imm_u32(m.sigs().size());
    for (auto& sig : m.sigs()) {
        m.write().code(sig.ret);
        m.write().imm_u32(sig.args.size());
        for (auto t : sig.args)
            m.write().code(t);
    }
}

void
write_function_import_section(Module& m)
{
    m.write().imm_u32(m.func_imps().size());
    m.write().imm_u32(m.num_func_imp_sigs());
    for (auto& func_imp : m.func_imps()) {
        m.write().c_str(func_imp.external.c_str());
        m.write().imm_u32(func_imp.sigs.size());
        for (auto& func_imp_sig : func_imp.sigs)
            m.write().imm_u32(func_imp_sig.sig_index);
    }
}

void
write_constant_pool_section(Module& m)
{
    m.write().imm_u32(m.i32s().size());
    m.write().imm_u32(m.f32s().size());
    m.write().imm_u32(m.f64s().size());
    for (auto t : m.i32s())
        m.write().imm_u32(t.first);
    for (auto t : m.f32s())
        m.write().fixed_width<uint32_t>(t.first);
    for (auto t : m.f64s())
        m.write().fixed_width<uint64_t>(t.first);
}

void
write_global_section(Module& m)
{
    m.write().imm_u32(m.num_global_i32_zero());
    m.write().imm_u32(m.num_global_f32_zero());
    m.write().imm_u32(m.num_global_f64_zero());
    m.write().imm_u32(m.global_i32_imports().size());
    m.write().imm_u32(m.global_f32_imports().size());
    m.write().imm_u32(m.global_f64_imports().size());
    for (auto i : m.global_i32_imports())
        m.write().c_str(i.c_str());
    for (auto i : m.global_f32_imports())
        m.write().c_str(i.c_str());
    for (auto i : m.global_f64_imports())
        m.write().c_str(i.c_str());
}

void
write_function_declaration_section(Module& m)
{
    m.write().imm_u32(m.funcs().size());
    for (auto& f : m.funcs())
        m.write().imm_u32(f.sig_index());
}

void
write_function_pointer_tables(Module& m)
{
    m.write().imm_u32(m.func_ptr_tables().size());
    for (auto& func_ptr_table : m.func_ptr_tables()) {
        m.write().imm_u32(func_ptr_table.sig_index);
        m.write().imm_u32(func_ptr_table.elems.size());
        for (auto elem : func_ptr_table.elems)
            m.write().imm_u32(elem);
    }
}

enum class Ctx { Expr, Stmt };

void write_expr(Module&, Function&, const AstNode&);
void write_call(Module& m, Function& f, const CallNode& call, Ctx ctx);
void write_stmt(Module&, Function&, const AstNode&);

template <class T, class TWithImm>
void
write_num_lit_pool(Module& m, uint32_t pool_index)
{
    if (pool_index < ImmLimit) {
        m.write().code(TWithImm::LitPool, pool_index);
    }
    else {
        m.write().code(T::LitPool);
        m.write().imm_u32(pool_index);
    }
}

void
write_num_lit(Module& m, Function& f, NumLit lit, unsigned lshift = 0)
{
    uint32_t pool_index;
    if (m.lit_has_pool_index(lit, lshift, &pool_index)) {
        switch (lit.type()) {
        case Type::I32: write_num_lit_pool<I32, I32WithImm>(m, pool_index); break;
        case Type::F32: write_num_lit_pool<F32, F32WithImm>(m, pool_index); break;
        case Type::F64: write_num_lit_pool<F64, F64WithImm>(m, pool_index); break;
        }
    }
    else {
        switch (lit.type()) {
        case Type::I32: {
            uint32_t u32 = lit.uint32() << lshift;
            if (u32 < ImmLimit) {
                m.write().code(I32WithImm::LitImm, u32);
            }
            else {
                m.write().code(I32::LitImm);
                m.write().imm_u32(u32);
            }
            break;
        }
        case Type::F32:
            assert(lshift == 0);
            m.write().code(F32::LitImm);
            m.write().fixed_width<float>(lit.float32());
            break;
        case Type::F64:
            assert(lshift == 0);
            m.write().code(F64::LitImm);
            m.write().fixed_width<double>(lit.float64());
            break;
        }
    }
}

bool
is_num_lit_u32(Module& m, const AstNode& ast, uint32_t u32)
{
    if (!NumLit::is(m, ast))
        return false;
    NumLit lit(m, ast);
    if (lit.type() != Type::I32)
        return false;
    return lit.uint32() == u32;
}

void
write_bitwise(Module& m, Function& f, const BinaryNode& binary, Ctx ctx)
{
    if (is_int_coerced_call(binary)) {
        write_call(m, f, binary.lhs.as<CallNode>(), ctx);
        return;
    }

    assert(ctx == Ctx::Expr);

    bool commutative;
    uint32_t identity = 0;
    switch (binary.expr.i32()) {
    case I32::BitOr: commutative = true; identity = 0; break;
    case I32::BitAnd: commutative = true; identity = UINT32_MAX; break;
    case I32::BitXor: commutative = true; identity = 0; break;
    case I32::Lsh: commutative = false; identity = 0; break;
    case I32::ArithRsh: commutative = false; identity = 0; break;
    case I32::LogicRsh: commutative = false; identity = 0; break;
    default: unreachable<void>();
    }

    if (is_num_lit_u32(m, binary.rhs, identity)) {
        write_expr(m, f, binary.lhs);
        return;
    }

    if (commutative && is_num_lit_u32(m, binary.lhs, identity)) {
        write_expr(m, f, binary.rhs);
        return;
    }

    write_expr(m, f, binary.lhs);
    write_expr(m, f, binary.rhs);
    m.write().code(binary.expr);
}

void
write_index(Module& m, Function& f, const IndexNode& index)
{
    if (index.offset > 0)
    {
        m.write().imm_u32(index.offset);
    }
    if (index.constant)
    {
        write_num_lit(m, f, NumLit(m, *index.index), m.heap_view(index.array.as<NameNode>().str).shift);
    }
    else
    {
        write_expr(m, f, *index.index);
    }
}

void
write_assign(Module& m, Function& f, const BinaryNode& binary, Ctx ctx)
{
    uint32_t index = binary.lhs.as<NameNode>().index;
    m.write().imm_u32(index);
    write_expr(m, f, binary.rhs);
    m.write().code(binary.expr);
}

void
write_store(Module& m, Function& f, const BinaryNode& binary, Ctx ctx)
{
    // The simple case
    if (ctx == Ctx::Stmt || binary.store_rhs_conv.is_bad())
    {
        write_index(m, f, binary.lhs.as<IndexNode>());
        write_expr(m, f, binary.rhs);
        if (ctx == Ctx::Stmt)
        {
            m.write().code(binary.stmt);
        }
        else
        {
            m.write().code(binary.expr);
        }
        if (!binary.store_rhs_conv.is_bad())
        {
            m.write().code(binary.store_rhs_conv);
        }
        return;
    }

    // The complex case: the stored value must be converted, but the result of the SetLoc expression
    // must be pre-conversion. Store the pre-conversion value in a temporary local (that we allocated
    // during the analyze phase), convert and store in the lhs of a Comma, and return the
    // pre-conversion value as the rhs of the Comma.

    Type val_type;
    uint32_t temp_local_index;
    if (binary.store_rhs_conv == F64::FromF32)
    {
        val_type = Type::F32;
        temp_local_index = f.f32_temp();
    }
    else
    {
        assert(binary.store_rhs_conv == F32::FromF64);
        val_type = Type::F64;
        temp_local_index = f.f64_temp();
    }

    // comma lhs
    write_index(m, f, binary.lhs.as<IndexNode>());
    m.write().code(binary.expr);
    m.write().imm_u32(temp_local_index);

    // comma rhs
    write_expr(m, f, binary.rhs);
    m.write().code(binary.store_rhs_conv);
    m.write().code(type_switch(val_type, I32::Bad, F32::SetLoc, F64::SetLoc));

    m.write().imm_u32(temp_local_index);
    m.write().code(type_switch(val_type, I32::Bad, F32::GetLoc, F64::GetLoc));

    // TODO (Mike): understand these semantics
    m.write().code(type_switch(val_type, I32::Bad, F32::Comma, F64::Comma));
    m.write().code(binary.store_rhs_conv.type());
}

void
write_binary(Module& m, Function& f, const BinaryNode& binary, Ctx ctx)
{
    switch (binary.kind) {
    case BinaryNode::Assign:
        write_assign(m, f, binary, ctx);
        break;
    case BinaryNode::Store:
        write_store(m, f, binary, ctx);
        break;
    case BinaryNode::Bitwise:
        write_bitwise(m, f, binary, ctx);
        break;
    case BinaryNode::Comma:
        assert(ctx == Ctx::Expr);
        write_expr(m, f, binary.lhs);
        write_expr(m, f, binary.rhs);
        m.write().code(binary.expr);
        // TODO (Mike): type information is going to be messed up
        m.write().code(binary.comma_lhs_type);
        break;
    case BinaryNode::Generic:
        assert(ctx == Ctx::Expr);
        write_expr(m, f, binary.lhs);
        write_expr(m, f, binary.rhs);
        m.write().code(binary.expr);
        break;
    }
}

void
write_name(Module& m, Function& f, const NameNode& name)
{
    if (!name.expr_with_imm.is_bad() && name.index < ImmLimit)
    {
        m.write().code(name.expr_with_imm, name.index);
    }
    else if (name.expr.is_bad())
    {
        write_num_lit(m, f, NumLit(m.stdlib_double(name.str)));
    }
    else
    {
        m.write().imm_u32(name.index);
        m.write().code(name.expr);
    }
}

void
write_prefix(Module& m, Function& f, const PrefixNode& prefix, Ctx ctx)
{
    // TODO (Mike): what is up with prefixes?
    if (is_double_coerced_call(prefix))
    {
        if (!prefix.expr.is_bad())
        {
            m.write().code(prefix.expr);
        }
        write_call(m, f, prefix.kid.as<CallNode>(), ctx);
        return;
    }

    assert(ctx == Ctx::Expr);

    assert(prefix.expr.is_bad() || !(prefix.op.equals("+") || prefix.op.equals("~")));

    write_expr(m, f, prefix.kid);

    if (!prefix.expr.is_bad())
    {
        m.write().code(prefix.expr);
    }

}

void
write_ternary(Module& m, Function& f, const TernaryNode& ternary)
{
    write_expr(m, f, ternary.cond);
    m.write().code(ternary.expr);
    write_expr(m, f, ternary.lhs);
    write_expr(m, f, ternary.rhs);
}

void
write_call(Module& m, Function& f, const CallNode& call, Ctx ctx)
{
    if (call.kind == CallNode::Indirect)
    {
        auto& index = call.callee.as<IndexNode>();
        write_expr(m, f, index.index->as<BinaryNode>().lhs);
    }
    for (AstNode* arg = call.first; arg; arg = arg->next)
    {
        write_expr(m, f, *arg);
    }
    switch (call.kind)
    {
    case CallNode::Import:
    {
        auto& func_imp_sig = m.func_imp(call.callee.as<NameNode>().str).sigs[call.import_preindex];
        m.write().imm_u32(func_imp_sig.func_imp_sig_index);
        assert(call.compute_length() == m.sig(func_imp_sig.sig_index).args.size());
        if (ctx == Ctx::Expr)
        {
            m.write().code(call.expr);
        }
        else
        {
            m.write().code(call.stmt);
        }
        break;
    }
    case CallNode::Internal:
    {
        auto func_index = m.func_index(call.callee.as<NameNode>().str);
        m.write().imm_u32(func_index);
        assert(call.compute_length() == m.func(func_index).sig().args.size());
        if (ctx == Ctx::Expr)
        {
            m.write().code(call.expr);
        }
        else
        {
            m.write().code(call.stmt);
        }
        break;
    }
    case CallNode::Indirect:
    {
        auto& index = call.callee.as<IndexNode>();
        auto func_ptr_tbl_i = m.func_ptr_table_index(index.array.as<NameNode>().str);
        m.write().imm_u32(func_ptr_tbl_i);
        assert(call.compute_length() == m.sig(m.func_ptr_table(func_ptr_tbl_i).sig_index).args.size());
        if (ctx == Ctx::Expr)
        {
            m.write().code(call.expr);
        }
        else
        {
            m.write().code(call.stmt);
        }
        break;
    }
    case CallNode::NaryBuiltin:
        assert(ctx == Ctx::Expr);
        m.write().imm_u32(call.compute_length());
        m.write().code(call.expr);
        break;
    case CallNode::FixedArityBuiltin:
        assert(ctx == Ctx::Expr);
        m.write().code(call.expr);
        break;
    case CallNode::Fround:
        assert(call.compute_length() == 1);
        if (call.expr.is_bad())
        {
            if (call.first->is<CallNode>())
            {
                // TODO (Mike): verify this
                write_call(m, f, call.first->as<CallNode>(), ctx);
                return;
            }
        }
        else
        {
            m.write().code(call.expr);
        }
        break;
    }
}

void
write_load(Module& m, Function& f, const IndexNode& index)
{
    write_index(m, f, index);
    m.write().code(index.expr);
}

void
write_expr(Module& m, Function& f, const AstNode& expr)
{
    if (NumLit::is(m, expr))
    {
        write_num_lit(m, f, NumLit(m, expr));
    }
    else {
        switch (expr.which) {
        case AstNode::Binary:
            write_binary(m, f, expr.as<BinaryNode>(), Ctx::Expr);
            break;
        case AstNode::Call:
            write_call(m, f, expr.as<CallNode>(), Ctx::Expr);
            break;
        case AstNode::Prefix:
            write_prefix(m, f, expr.as<PrefixNode>(), Ctx::Expr);
            break;
        case AstNode::Ternary:
            write_ternary(m, f, expr.as<TernaryNode>());
            break;
        case AstNode::Name:
            write_name(m, f, expr.as<NameNode>());
            break;
        case AstNode::Index:
            write_load(m, f, expr.as<IndexNode>());
            break;
        default:
            unreachable<void>();
        }
    }
}

void
write_return(Module& m, Function& f, const ReturnNode& ret)
{
    if (ret.expr)
    {
        write_expr(m, f, *ret.expr);
    }
    m.write().code(Stmt::Ret);
}

void
write_stmt_list(Module& m, Function& f, const AstNode* stmts)
{
    uint32_t num_stmts = 0;
    for (const AstNode* p = stmts; p; p = p->next)
    {
        num_stmts++;
    }

    // TODO (Mike): do we need to write this?
    m.write().imm_u32(num_stmts);
    for (const AstNode* n = stmts; n; n = n->next)
        write_stmt(m, f, *n);
}

void
write_block(Module& m, Function& f, const BlockNode& block)
{
    // TODO (Mike): what are blocks needed for?
    m.write().code(Stmt::Block);
    write_stmt_list(m, f, block.first);
}

void
write_if(Module& m, Function& f, const IfNode& i)
{
    if (i.if_false)
    {
        write_expr(m, f, i.cond);
        m.write().code(Stmt::IfElse);
        write_stmt(m, f, i.if_true);
        write_stmt(m, f, *i.if_false);
    }
    else
    {
        write_expr(m, f, i.cond);
        m.write().code(Stmt::IfThen);
        write_stmt(m, f, i.if_true);
    }
}

void
write_while(Module& m, Function& f, const WhileNode& w)
{
    write_expr(m, f, w.cond);
    m.write().code(Stmt::While);
    write_stmt(m, f, w.body);
}

void
write_do(Module& m, Function& f, const DoNode& d)
{
    write_stmt(m, f, d.body);
    write_expr(m, f, d.cond);
    m.write().code(Stmt::Do);
    // TODO (Mike): is this ok?
}

void
write_label(Module& m, Function& f, const LabelNode& l)
{
    // TODO (Mike): is a label statement really needed in encoding?
    m.write().code(Stmt::Label);
    f.push_label(l.str);
    write_stmt(m, f, l.stmt);
    f.pop_label(l.str);
}

void
write_break(Module& m, Function& f, const BreakNode& b)
{
    if (!b.str)
    {
        m.write().code(Stmt::Break);
        return;
    }

    m.write().code(Stmt::BreakLabel);
    m.write().imm_u32(f.label_depth(b.str));
}

void
write_continue(Module& m, Function& f, const ContinueNode& c)
{
    if (!c.str)
    {
        m.write().code(Stmt::Continue);
        return;
    }

    m.write().code(Stmt::ContinueLabel);
    m.write().imm_u32(f.label_depth(c.str));
}

void
write_switch(Module& m, Function& f, const SwitchNode& s)
{
    // TODO (Mike): this probably needs work
    write_expr(m, f, s.expr);
    m.write().code(Stmt::Switch);
    m.write().imm_u32(s.compute_length());
    bool wrote_default = false;
    for (const CaseNode* c = s.first; c; c = c->next)
    {
        if (c->label)
        {
            if (!c->first)
            {
                m.write().code(SwitchCase::Case0);
                m.write().imm_s32(NumLit(m, *c->label).int32());
            }
            else if (c->first == c->last)
            {
                m.write().code(SwitchCase::Case1);
                m.write().imm_s32(NumLit(m, *c->label).int32());
                write_stmt(m, f, c->first->stmt);
            }
            else
            {
                m.write().code(SwitchCase::CaseN);
                m.write().imm_s32(NumLit(m, *c->label).int32());
                m.write().imm_u32(c->compute_length());
                for (const CaseStmtNode* s = c->first; s; s = s->next)
                    write_stmt(m, f, s->stmt);
            }
        }
        else {
            assert(!wrote_default);
            wrote_default = true;
            if (!c->first) {
                m.write().code(SwitchCase::Default0);
            }
            else if (c->first == c->last) {
                m.write().code(SwitchCase::Default1);
                write_stmt(m, f, c->first->stmt);
            }
            else {
                m.write().code(SwitchCase::DefaultN);
                m.write().imm_u32(c->compute_length());
                for (const CaseStmtNode* s = c->first; s; s = s->next)
                    write_stmt(m, f, s->stmt);
            }
        }
    }
}

void
write_stmt(Module& m, Function& f, const AstNode& stmt)
{
    switch (stmt.which) {
    case AstNode::Call: write_call(m, f, stmt.as<CallNode>(), Ctx::Stmt); break;
    case AstNode::Prefix: write_prefix(m, f, stmt.as<PrefixNode>(), Ctx::Stmt); break;
    case AstNode::Binary: write_binary(m, f, stmt.as<BinaryNode>(), Ctx::Stmt); break;
    case AstNode::Return: write_return(m, f, stmt.as<ReturnNode>()); break;
    case AstNode::Block: write_block(m, f, stmt.as<BlockNode>()); break;
    case AstNode::If: write_if(m, f, stmt.as<IfNode>()); break;
    case AstNode::While: write_while(m, f, stmt.as<WhileNode>()); break;
    case AstNode::Do: write_do(m, f, stmt.as<DoNode>()); break;
    case AstNode::Label: write_label(m, f, stmt.as<LabelNode>()); break;
    case AstNode::Break: write_break(m, f, stmt.as<BreakNode>()); break;
    case AstNode::Continue: write_continue(m, f, stmt.as<ContinueNode>()); break;
    case AstNode::Switch: write_switch(m, f, stmt.as<SwitchNode>()); break;
    default: unreachable<void>();
    }
}

void
write_function_definition(Module& m, Function& f)
{
    if (f.num_i32_vars() < ImmLimit && f.num_f32_vars() == 0 && f.num_f64_vars() == 0) {
        m.write().code(VarTypesWithImm::OnlyI32, f.num_i32_vars());
    }
    else {
        VarTypes vt = (f.num_i32_vars() > 0 ? VarTypes::I32 : VarTypes(0)) |
            (f.num_f32_vars() > 0 ? VarTypes::F32 : VarTypes(0)) |
            (f.num_f64_vars() > 0 ? VarTypes::F64 : VarTypes(0));
        m.write().code(vt);
        if (vt & VarTypes::I32)
            m.write().imm_u32(f.num_i32_vars());
        if (vt & VarTypes::F32)
            m.write().imm_u32(f.num_f32_vars());
        if (vt & VarTypes::F64)
            m.write().imm_u32(f.num_f64_vars());
    }

    write_stmt_list(m, f, f.body());
}

void
write_function_definition_section(Module& m)
{
    for (auto& f : m.funcs())
        write_function_definition(m, f);
}

void
write_export_section(Module& m)
{
    if (m.exports().size() == 1 && !m.exports()[0].external) {
        m.write().code(ExportFormat::Default);
        m.write().imm_u32(m.exports().front().internal);
    }
    else {
        m.write().code(ExportFormat::Record);
        m.write().imm_u32(m.exports().size());
        for (auto& e : m.exports()) {
            m.write().c_str(e.external.c_str());
            m.write().imm_u32(e.internal);
        }
    }
}

void
write_module(Module& m)
{
    m.write().fixed_width<uint32_t>(MagicNumber);

    // Bogus unpacked-size; to be patched in patch_unpacked_size.
    m.write().fixed_width<uint32_t>(UINT32_MAX);

    write_constant_pool_section(m);
    write_signature_section(m);
    write_function_import_section(m);
    write_global_section(m);
    write_function_declaration_section(m);
    write_function_pointer_tables(m);
    write_function_definition_section(m);
    write_export_section(m);
}

const FuncNode&
parse(char* src)
{
    const AstNode* ast = cashew::Parser<AstNodePtr, AstBuilder>().parseToplevel(src);
    const TopLevelNode& top = ast->as<TopLevelNode>();
    if (!top.first || top.first != top.last || !top.first->is<FuncNode>())
    {
        throw runtime_error("there should be exactly one top-level asm.js statement in the asm.js file");
    }

    FuncNode& module_func = top.first->as<FuncNode>();
    List<AstNode>& body = module_func;
    if (!body.first || !body.first->is<StringNode>() || !body.first->as<StringNode>().str.equals("use asm"))
    {
        throw runtime_error("the argument to the top-level call should be an asm.js module");
    }

    return module_func;
}

void
patch_unpacked_size(std::ostream& os, uint32_t unpacked_size)
{
    os.seekp(sizeof(uint32_t));
    Out out(os);
    out.fixed_width<uint32_t>(unpacked_size);
}

void
pack(ostream& os, const FuncNode& module)
{
    Module m(os);
    analyze_module(m, module);
    write_module(m);
}

}  // namespace asmjs

int
main(int argc, char** argv)
try
{
    if (argc != 3 || !argv[1] || !argv[2])
    {
        cerr << "Usage: pack-asmjs in.js out.wasm" << endl;
        return -1;
    }

    // Parse the asm.js file.
    ifstream in_stream(argv[1], ios::binary | ios::ate);
    in_stream.exceptions(ios::failbit | ios::badbit);
    vector<char> in_bytes(in_stream.tellg());
    in_stream.seekg(0);
    in_stream.read(in_bytes.data(), in_bytes.size());
    in_bytes.push_back('\0');
    in_stream.close();
    const asmjs::FuncNode& module = asmjs::parse(in_bytes.data());

    // Write out the .asm file (with bogus unpacked-size).
    fstream out_stream(argv[2], ios::in | ios::out | ios::binary | ios::trunc);
    out_stream.exceptions(ios::failbit | ios::badbit);
    asmjs::pack(out_stream, module);

    // Compute unpacked-size (using unpack()) and patch the file.
    vector<uint8_t> out_bytes(out_stream.tellp());
    out_stream.seekg(0);
    out_stream.read((char*)out_bytes.data(), out_bytes.size());
#ifdef CHECKED_OUTPUT_SIZE
    uint32_t unpacked_size = asmjs::calculate_unpacked_size(out_bytes.data());
    asmjs::patch_unpacked_size(out_stream, unpacked_size);
#endif
    return 0;
}

catch (const ios::failure& err)
{
    cerr << "Failed with runtime error: " << err.what() << endl;
    return -1;
}

catch (const runtime_error& err)
{
    cerr << "Failed with runtime error: " << err.what() << endl;
    return -1;
}
