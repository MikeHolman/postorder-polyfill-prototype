#include "Packer.h"

namespace asmjs
{
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

AsmJSType
analyze_num_lit(Module& m, NumLit lit, unsigned lshift)
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
} // namespace asmjs
