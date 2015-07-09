#pragma once

namespace asmjs
{
    void write_signature_section(Module& m);
    void write_function_import_section(Module& m);
    void write_constant_pool_section(Module& m);
    void write_global_section(Module& m);
    void write_function_declaration_section(Module& m);
    void write_function_pointer_tables(Module& m);

    enum class Ctx { Expr, Stmt };
    void write_expr(Module&, Function&, const AstNode&);
    void write_call(Module& m, Function& f, const CallNode& call, Ctx ctx);
    void write_stmt(Module&, Function&, const AstNode&);

    template <class T, class TWithImm>
    void write_num_lit_pool(Module& m, uint32_t pool_index);
    void write_num_lit(Module& m, Function& f, NumLit lit, unsigned lshift = 0);
    bool is_num_lit_u32(Module& m, const AstNode& ast, uint32_t u32);
    bool is_double_coerced_call(const PrefixNode& prefix);
    bool is_int_coerced_call(const BinaryNode& binary);
    void write_bitwise(Module& m, Function& f, const BinaryNode& binary, Ctx ctx);
    void write_index(Module& m, Function& f, const IndexNode& index);
    void write_assign(Module& m, Function& f, const BinaryNode& binary, Ctx ctx);
    void write_store(Module& m, Function& f, const BinaryNode& binary, Ctx ctx);
    void write_binary(Module& m, Function& f, const BinaryNode& binary, Ctx ctx);
    void write_name(Module& m, Function& f, const NameNode& name);
    void write_prefix(Module& m, Function& f, const PrefixNode& prefix, Ctx ctx);
    void write_ternary(Module& m, Function& f, const TernaryNode& ternary);
    void write_load(Module& m, Function& f, const IndexNode& index);
    void write_return(Module& m, Function& f, const ReturnNode& ret);
    void write_stmt_list(Module& m, Function& f, const AstNode* stmts);
    void write_block(Module& m, Function& f, const BlockNode& block);
    void write_if(Module& m, Function& f, const IfNode& i);
    void write_while(Module& m, Function& f, const WhileNode& w);
    void write_do(Module& m, Function& f, const DoNode& d);
    void write_label(Module& m, Function& f, const LabelNode& l);
    void write_break(Module& m, Function& f, const BreakNode& b);
    void write_continue(Module& m, Function& f, const ContinueNode& c);
    void write_switch(Module& m, Function& f, const SwitchNode& s);

    void write_function_definition(Module& m, Function& f);

    void write_function_definition_section(Module& m);

    void write_export_section(Module& m);

    void write_module(Module& m);

} //namespace asmjs
