#pragma once

namespace asmjs
{
    void analyze_heap_ctor(Module& m, const VarNameNode& var);
    void analyze_import(Module& m, IString name, DotNode& dot);
    AstNode* analyze_global_definitions(Module& m, AstNode* stmt);
    Type extract_arg_type(Module& m, const ArgNode& arg, const AstNode& stmt);
    Type extract_var_init(Module& m, const AstNode& init);
    AsmJSType analyze_expr(Module&, Function&, AstNode&);
    RType analyze_call(Module& m, Function& f, CallNode& call, RType ret_type, AstNode* coercion = nullptr);
    AsmJSType analyze_num_lit(Module& m, NumLit lit, unsigned lshift = 0);
    AsmJSType analyze_stdlib_call(Module& m, Function& f, CallNode& call);
    void set_coercion_node_expr(AstNode* coercion, Expr expr);
    RType analyze_coerced_stdlib_call(Module& m, Function& f, CallNode& call, RType ret_type, AstNode* coercion);
    void analyze_call_import(Module& m, Function& f, CallNode& call, RType ret_type);
    void analyze_call_internal(Module& m, Function& f, CallNode& call, RType ret_type);
    void analyze_call_indirect(Module& m, Function& f, CallNode& call, RType ret_type);
    AsmJSType analyze_to_number(Module& m, Function& f, PrefixNode& prefix);
    AsmJSType analyze_negate(Module& m, Function& f, PrefixNode& prefix);
    AsmJSType analyze_bitnot(Module& m, Function& f, PrefixNode& prefix);
    AsmJSType analyze_not(Module& m, Function& f, PrefixNode& prefix);
    AsmJSType analyze_name(Module& m, Function& f, NameNode& name);
    AsmJSType analyze_prefix(Module& m, Function& f, PrefixNode& prefix);
    AsmJSType analyze_comma(Module& m, Function& f, BinaryNode& binary);
    HeapView analyze_index(Module& m, Function& f, IndexNode& index);
    AsmJSType analyze_load(Module& m, Function& f, IndexNode& index);
    AsmJSType analyze_store(Module& m, Function& f, BinaryNode& binary);
    AsmJSType analyze_assign(Module& m, Function& f, BinaryNode& binary);
    AsmJSType analyze_arith(Module& m, Function& f, BinaryNode& binary, PreTypeCode pre);
    Expr specialize(AsmJSType lhs, AsmJSType rhs, PreTypeSignCode pre);
    AsmJSType analyze_div_mod(Module& m, Function& f, BinaryNode& binary, PreTypeSignCode pre);
    AsmJSType analyze_eq(Module& m, Function& f, BinaryNode& binary, PreTypeCode pre);
    AsmJSType analyze_comp(Module& m, Function& f, BinaryNode& binary, PreTypeSignCode pre);
    AsmJSType analyze_bitwise(Module& m, Function& f, BinaryNode& binary, I32 i32);
    AsmJSType analyze_binary(Module& m, Function& f, BinaryNode& binary);
    AsmJSType analyze_ternary(Module& m, Function& f, TernaryNode& ternary);
    void analyze_stmt(Module& m, Function& f, AstNode& stmt);
    void analyze_function_definition(Module& m, const FuncNode& func);
    void analyze_func_ptr_table(Module& m, const VarNode& var);
    void analyze_export_stmt(Module& m, const ReturnNode& export_stmt);
    void analyze_module(Module& m, const FuncNode& module);
}
