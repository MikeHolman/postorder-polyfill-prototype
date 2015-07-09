#include "Packer.h"

namespace asmjs
{

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
write_num_lit(Module& m, Function& f, NumLit lit, unsigned lshift)
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

} // namespace asmjs
