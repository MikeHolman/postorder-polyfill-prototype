#pragma once

using namespace std;
using cashew::IString;

namespace asmjs
{
    // =================================================================================================
    // AST

    template <class T>
    struct ListElemBase
    {
        ListElemBase() : next(nullptr) {}

        T* next;
    };

    template <class T>
    struct List
    {
        List() : first(nullptr), last(nullptr) {}

        void append(T& n)
        {
            if (!first) {
                first = last = &n;
            }
            else {
                last->next = &n;
                last = &n;
            }
        }

        void append_list(List<T>& list)
        {
            for (T* p = list.first; p; p = p->next)
                append(*p);
        }

        uint32_t compute_length() const
        {
            uint32_t len = 0;
            for (T* p = first; p; p = p->next)
                len++;
            return len;
        }

        T* first;
        T* last;
    };

    struct AstNode : AstMemoryBase, ListElemBase<AstNode>
    {
        enum Enum {
            TopLevel, Function,
            Var, Return, Block, If, While, Do, Label, Break, Continue, Switch,
            Double, Int, Name, Prefix, Binary, Ternary, Call, String, Object, New, Dot, Index, Array
        } which;

        template <class T> bool is() const { return which == T::Which; }
        template <class T> T& as() { assert(is<T>()); return *static_cast<T*>(this); }
        template <class T> const T& as() const { assert(is<T>()); return *static_cast<const T*>(this); }

        AstNode(Enum which) : which(which) {}
    };

    struct TopLevelNode : AstNode, List<AstNode>
    {
        static const Enum Which = TopLevel;
        TopLevelNode() : AstNode(Which) {}
    };

    struct ArgNode : AstMemoryBase, ListElemBase<ArgNode>
    {
        ArgNode(IString name) : name(name) {}

        IString const name;
    };

    struct FuncNode : AstNode, List<AstNode>, List<ArgNode>
    {
        static const Enum Which = Function;
        FuncNode(IString name) : AstNode(Which), name(name) {}

        IString const name;
    };

    struct VarNameNode : AstMemoryBase, ListElemBase<VarNameNode>
    {
        VarNameNode(IString name, AstNode& init) : name(name), init(init) {}

        IString const name;
        AstNode& init;
    };

    struct VarNode : AstNode, List<VarNameNode>
    {
        static const Enum Which = Var;
        VarNode() : AstNode(Which) {}
    };

    struct ReturnNode : AstNode
    {
        static const Enum Which = Return;
        ReturnNode(AstNode* expr) : AstNode(Which), expr(expr) {}

        AstNode* const expr;
    };

    struct BlockNode : AstNode, List<AstNode>
    {
        static const Enum Which = Block;
        BlockNode() : AstNode(Which) {}
        BlockNode(AstNode& a, AstNode& b) : AstNode(Which) { append(a); append(b); }
    };

    struct IfNode : AstNode
    {
        static const Enum Which = If;
        IfNode(AstNode& cond, AstNode& if_true, AstNode* if_false)
            : AstNode(Which)
            , cond(cond)
            , if_true(if_true)
            , if_false(if_false)
        {}

        AstNode& cond;
        AstNode& if_true;
        AstNode* if_false;
    };

    struct WhileNode : AstNode
    {
        static const Enum Which = While;
        WhileNode(AstNode& cond, AstNode& body) : AstNode(Which), cond(cond), body(body) {}

        AstNode& cond;
        AstNode& body;
    };

    struct DoNode : AstNode
    {
        static const Enum Which = Do;
        DoNode(AstNode& body, AstNode& cond) : AstNode(Which), body(body), cond(cond) {}

        AstNode& body;
        AstNode& cond;
    };

    struct LabelNode : AstNode
    {
        static const Enum Which = Label;
        LabelNode(IString str, AstNode& stmt) : AstNode(Which), str(str), stmt(stmt) {}

        const IString str;
        AstNode& stmt;
    };

    struct BreakNode : AstNode
    {
        static const Enum Which = Break;
        BreakNode(IString str) : AstNode(Which), str(str) {}

        const IString str;
    };

    struct ContinueNode : AstNode
    {
        static const Enum Which = Continue;
        ContinueNode(IString str) : AstNode(Which), str(str) {}

        const IString str;
    };

    struct CaseStmtNode : AstMemoryBase, ListElemBase<CaseStmtNode>
    {
        CaseStmtNode(AstNode& stmt) : stmt(stmt) {}

        AstNode& stmt;
    };

    struct CaseNode : AstMemoryBase, ListElemBase<CaseNode>, List<CaseStmtNode>
    {
        CaseNode() : label(nullptr) {}
        CaseNode(AstNode& label) : label(&label) {}

        AstNode* const label;
    };

    struct SwitchNode : AstNode, List<CaseNode>
    {
        static const Enum Which = Switch;
        SwitchNode(AstNode& expr) : AstNode(Which), expr(expr) {}

        AstNode& expr;
    };

    struct DoubleNode : AstNode
    {
        static const Enum Which = Double;
        DoubleNode(double f64) : AstNode(Which), f64(f64) {}

        const double f64;
    };

    struct IntNode : AstNode
    {
        static const Enum Which = Int;
        IntNode(uint32_t u32) : AstNode(Which), u32(u32) {}

        const uint32_t u32;
    };

    struct NameNode : AstNode
    {
        static const Enum Which = Name;
        NameNode(IString str) : AstNode(Which), str(str) {}

        uint32_t index;
        IString const str;

        Expr expr;
        ExprWithImm expr_with_imm;
    };

    struct PrefixNode : AstNode
    {
        static const Enum Which = Prefix;
        PrefixNode(IString op, AstNode& kid)
            : AstNode(Which)
            , op(op)
            , kid(kid)
        {}

        IString const op;
        AstNode& kid;

        Expr expr;
    };


    struct BinaryNode : AstNode
    {
        static const Enum Which = Binary;
        BinaryNode(IString op, AstNode& lhs, AstNode& rhs)
            : AstNode(Which)
            , op(op)
            , lhs(lhs)
            , rhs(rhs)
            , kind(Generic)
            , stmt(Stmt::Bad)
            , stmt_with_imm(StmtWithImm::Bad)
            , comma_lhs_type(RType(-1))
        {}

        IString const op;
        AstNode& lhs;
        AstNode& rhs;

        enum Kind { Bitwise, Assign, Store, Comma, Generic } kind;
        Expr expr;
        Stmt stmt;
        StmtWithImm stmt_with_imm;
        RType comma_lhs_type;
        Expr store_rhs_conv;
    };

    struct TernaryNode : AstNode
    {
        static const Enum Which = Ternary;
        TernaryNode(AstNode& cond, AstNode& lhs, AstNode& rhs)
            : AstNode(Which)
            , cond(cond)
            , lhs(lhs)
            , rhs(rhs)
        {}

        AstNode& cond;
        AstNode& lhs;
        AstNode& rhs;

        Expr expr;
    };

    struct CallNode : AstNode, List<AstNode>
    {
        static const Enum Which = Call;
        CallNode(AstNode& callee)
            : AstNode(Which)
            , import_preindex(UINT32_MAX)
            , callee(callee)
            , kind(Kind(UINT32_MAX))
            , stmt(Stmt::Bad)
        {}

        uint32_t import_preindex;
        AstNode& callee;

        enum Kind { Internal, Indirect, Import, NaryBuiltin, FixedArityBuiltin, Fround } kind;
        Expr expr;
        Stmt stmt;
    };

    struct StringNode : AstNode
    {
        static const Enum Which = String;
        StringNode(IString str) : AstNode(Which), str(str) {}

        IString const str;
    };

    struct FieldNode : AstMemoryBase, ListElemBase<FieldNode>
    {
        FieldNode(IString key, AstNode& value) : key(key), value(value) {}

        IString const key;
        AstNode& value;
    };

    struct ObjectNode : AstNode, List<FieldNode>
    {
        static const Enum Which = Object;
        ObjectNode() : AstNode(Which) {}
    };

    struct NewNode : AstNode
    {
        static const Enum Which = New;
        NewNode(CallNode& call) : AstNode(Which), call(call) {}

        CallNode& call;
    };

    struct DotNode : AstNode
    {
        static const Enum Which = Dot;
        DotNode(AstNode& base, IString name) : AstNode(Which), base(base), name(name) {}

        AstNode& base;
        IString name;
    };

    struct IndexNode : AstNode
    {
        static const Enum Which = Index;
        IndexNode(AstNode& array, AstNode& index)
            : AstNode(Which)
            , array(array)
            , index(&index)
            , constant(false)
            , offset(0)
        {}

        AstNode& array;
        AstNode* index;

        bool constant;
        Expr expr;
        uint32_t offset;
    };

    struct ArrayNode : AstNode, List<AstNode>
    {
        static const Enum Which = Array;
        ArrayNode() : AstNode(Which) {}
    };

    // =================================================================================================
    // asm.js types

    class AsmJSType
    {
    public:
        enum Enum { Fixnum, Signed, Unsigned, Int, Float, Double, Void };
    private:
        Enum which_;
    public:
        AsmJSType() {}
        AsmJSType(Enum which) : which_(which) {}
        Enum which() const { return which_; }

        Type type() const {
            switch (which_) {
            case Fixnum: case Signed: case Unsigned: case Int:
                return Type::I32;
            case Float:
                return Type::F32;
            case Double:
                return Type::F64;
            case Void:;
            }
            return unreachable<Type>();
        }

        RType rtype() const {
            switch (which_) {
            case Fixnum: case Signed: case Unsigned: case Int:
                return RType::I32;
            case Float:
                return RType::F32;
            case Double:
                return RType::F64;
            case Void:
                return RType::Void;
            }
            return unreachable<RType>();
        }

        bool is_signed() const { return which_ == Fixnum || which_ == Signed; }
        bool is_unsigned() const { return which_ == Fixnum || which_ == Unsigned; }
        bool is_int() const { return type() == Type::I32; }
        bool is_float() const { return which_ == Float; }
        bool is_double() const { return which_ == Double; }
        AsmJSType forget_signedness() const { return type_switch(type(), Int, Float, Double); }
    };

    // =================================================================================================
    // Module

    class Function
    {
        Signature sig_;
        size_t sig_index_;
        bool set_ret_type_;
        unordered_map<IString, size_t> local_name_to_index_;
        vector<Type> locals_;
        size_t num_i32s_;
        size_t num_f32s_;
        size_t num_f64s_;
        size_t f32_temp_;
        size_t f64_temp_;
        const AstNode* body_;
        unordered_map<IString, size_t> label_to_depth_;

        void add_var(IString name, Type type)
        {
            assert(local_name_to_index_.find(name) == local_name_to_index_.end());
            local_name_to_index_.emplace(name, locals_.size());
            locals_.push_back(type);
        }

    public:
        explicit Function()
            : sig_(RType::Void)
            , set_ret_type_(false)
            , num_i32s_(0)
            , num_f32s_(0)
            , num_f64s_(0)
            , f32_temp_(UINT32_MAX)
            , f64_temp_(UINT32_MAX)
            , body_(nullptr)
        {}

        void add_arg(IString name, Type type)
        {
            assert(local_name_to_index_.find(name) == local_name_to_index_.end());
            assert(locals_.size() == sig_.args.size());
            local_name_to_index_.emplace(name, locals_.size());
            locals_.push_back(type);
            sig_.args.push_back(type);
        }

        void start_body(const AstNode* body,
            const vector<IString>& i32s,
            const vector<IString>& f32s,
            const vector<IString>& f64s)
        {
            assert(!body_);
            body_ = body;
            num_i32s_ = i32s.size();
            num_f32s_ = f32s.size();
            num_f64s_ = f64s.size();
            for (IString name : i32s)
                add_var(name, Type::I32);
            for (IString name : f32s)
                add_var(name, Type::F32);
            for (IString name : f64s)
                add_var(name, Type::F64);
        }

        void set_needs_f32_temp()
        {
            if (f32_temp_ != UINT32_MAX)
                return;
            f32_temp_ = sig_.args.size() + num_i32s_ + num_f32s_;
            num_f32s_++;
            locals_.insert(locals_.begin() + f32_temp_, Type::F32);
        }

        void set_needs_f64_temp()
        {
            if (f64_temp_ != UINT32_MAX)
                return;
            f64_temp_ = sig_.args.size() + num_i32s_ + num_f32s_ + num_f64s_;
            num_f64s_++;
            locals_.insert(locals_.begin() + f64_temp_, Type::F64);
        }

        void set_ret_type(RType ret)
        {
            assert(!set_ret_type_ || sig_.ret == ret);
            set_ret_type_ = true;
            sig_.ret = ret;
        }

        void push_label(IString name)
        {
            assert(label_to_depth_.find(name) == label_to_depth_.end());
            label_to_depth_.emplace(name, label_to_depth_.size());
        }

        void pop_label(IString name)
        {
            assert(label_to_depth_.find(name)->second == label_to_depth_.size() - 1);
            label_to_depth_.erase(name);
        }

        size_t label_depth(IString name) const
        {
            return label_to_depth_.find(name)->second;
        }

        const Signature& sig() const { return sig_; }
        void set_sig_index(size_t sig_index) { sig_index_ = sig_index; }
        size_t sig_index() const { return sig_index_; }

        size_t num_vars() const { return locals_.size() - sig_.args.size(); }
        size_t num_i32_vars() const { return num_i32s_; }
        size_t num_f32_vars() const { return num_f32s_; }
        size_t num_f64_vars() const { return num_f64s_; }
        size_t num_locals() const { return locals_.size(); }
        bool is_local_name(IString name) { return local_name_to_index_.find(name) != local_name_to_index_.end(); }
        size_t local_index(IString name) const { return local_name_to_index_.find(name)->second; }
        Type local_type(size_t i) const { return locals_[i]; }
        const AstNode* body() const { return body_; }
        size_t f32_temp() const { assert(f32_temp_ != UINT32_MAX); return f32_temp_; }
        size_t f64_temp() const { assert(f64_temp_ != UINT32_MAX); return f64_temp_; }
    };

    enum class PreTypeCode { Neg, Add, Sub, Mul, Eq, NEq, Abs, Ceil, Floor, Sqrt, Comma };

    inline Expr
        specialize(PreTypeCode pre, Type type)
    {
        switch (pre) {
        case PreTypeCode::Neg:   return type_switch(type, I32::Neg, F32::Neg, F64::Neg);
        case PreTypeCode::Add:   return type_switch(type, I32::Add, F32::Add, F64::Add);
        case PreTypeCode::Sub:   return type_switch(type, I32::Sub, F32::Sub, F64::Sub);
        case PreTypeCode::Mul:   return type_switch(type, I32::Mul, F32::Mul, F64::Mul);
        case PreTypeCode::Eq:    return type_switch(type, I32::EqI32, I32::EqF32, I32::EqF64);
        case PreTypeCode::NEq:   return type_switch(type, I32::NEqI32, I32::NEqF32, I32::NEqF64);
        case PreTypeCode::Abs:   return type_switch(type, I32::Abs, F32::Abs, F64::Abs);
        case PreTypeCode::Ceil:  return type_switch(type, I32::Bad, F32::Ceil, F64::Ceil);
        case PreTypeCode::Floor: return type_switch(type, I32::Bad, F32::Floor, F64::Floor);
        case PreTypeCode::Sqrt:  return type_switch(type, I32::Bad, F32::Sqrt, F64::Sqrt);
        case PreTypeCode::Comma: return type_switch(type, I32::Comma, F32::Comma, F64::Comma);
        }
        return unreachable<Expr>();
    }

    enum class PreTypeSignCode { ToF32, Div, Mod, LeTh, LeEq, GrTh, GrEq, Min, Max };

    inline Expr
        specialize(PreTypeSignCode pre, Signedness si)
    {
        switch (pre) {
        case PreTypeSignCode::ToF32: return signedness_switch(si, F32::FromS32, F32::FromU32);
        case PreTypeSignCode::Div:   return signedness_switch(si, I32::SDiv, I32::UDiv);
        case PreTypeSignCode::Mod:   return signedness_switch(si, I32::SMod, I32::UMod);
        case PreTypeSignCode::LeTh:  return signedness_switch(si, I32::SLeThI32, I32::ULeThI32);
        case PreTypeSignCode::LeEq:  return signedness_switch(si, I32::SLeEqI32, I32::ULeEqI32);
        case PreTypeSignCode::GrTh:  return signedness_switch(si, I32::SGrThI32, I32::UGrThI32);
        case PreTypeSignCode::GrEq:  return signedness_switch(si, I32::SGrEqI32, I32::UGrEqI32);
        case PreTypeSignCode::Min:   return signedness_switch(si, I32::SMin, I32::UMin);
        case PreTypeSignCode::Max:   return signedness_switch(si, I32::SMax, I32::UMax);
        }
        return unreachable<Expr>();
    }

    inline Expr
        specialize(PreTypeSignCode pre, Type type)
    {
        assert(type == Type::F32 || type == Type::F64);
        switch (pre) {
        case PreTypeSignCode::ToF32: return type == Type::F32 ? Expr::Bad() : F32::FromF64;
        case PreTypeSignCode::Div:   return type_switch(type, I32::Bad, F32::Div, F64::Div);
        case PreTypeSignCode::LeTh:  return type_switch(type, I32::Bad, I32::LeThF32, I32::LeThF64);
        case PreTypeSignCode::LeEq:  return type_switch(type, I32::Bad, I32::LeEqF32, I32::LeEqF64);
        case PreTypeSignCode::GrTh:  return type_switch(type, I32::Bad, I32::GrThF32, I32::GrThF64);
        case PreTypeSignCode::GrEq:  return type_switch(type, I32::Bad, I32::GrEqF32, I32::GrEqF64);
        case PreTypeSignCode::Mod:   assert(type == Type::F64); return F64::Mod;
        case PreTypeSignCode::Min:   assert(type == Type::F64); return F64::Min;
        case PreTypeSignCode::Max:   assert(type == Type::F64); return F64::Max;
        }
        return unreachable<Expr>();
    }

    class StdLibFunc
    {
    public:
        enum Enum { Mono, Poly, SignedPoly };

    private:
        Enum which_;
        union {
            Expr mono_;
            PreTypeCode poly_;
            PreTypeSignCode signed_poly_;
        } u;

    public:
        StdLibFunc(Expr e) : which_(Mono), u{} { u.mono_ = e; }
        StdLibFunc(PreTypeCode p) : which_(Poly), u{} { u.poly_ = p; }
        StdLibFunc(PreTypeSignCode p) : which_(SignedPoly), u{} { u.signed_poly_ = p; }

        Enum which() const { return which_; }
        Expr mono() const { assert(which_ == Mono); return u.mono_; }
        PreTypeCode poly() const { assert(which_ == Poly); return u.poly_; }
        PreTypeSignCode signed_poly() const { assert(which_ == SignedPoly); return u.signed_poly_; }
    };

    struct HeapView
    {
        HeapView(Type type, unsigned shift) : type(type), shift(shift), si(Signedness(-1)) {}
        HeapView(unsigned shift, Signedness si) : type(Type::I32), shift(shift), si(si) {}

        Type type;
        unsigned shift;
        Signedness si;
    };

    struct FuncImportSignature
    {
        FuncImportSignature(uint32_t sig_index) : sig_index(sig_index) {}

        uint32_t sig_index;
        uint32_t func_imp_sig_index;
    };

    struct FuncImport
    {
        FuncImport(IString external) : external(external) {}

        IString external;
        vector<FuncImportSignature> sigs;
    };

    struct FuncPtrTable
    {
        uint32_t sig_index;
        vector<uint32_t> elems;

        FuncPtrTable(uint32_t sig_index, vector<uint32_t>&& elems)
            : sig_index(sig_index)
            , elems(move(elems))
        {}
    };

    struct Global
    {
        Global(Type type, size_t index) : type(type), index(index) {}

        Type type;
        size_t index;
    };

    struct Export
    {
        Export(IString external, uint32_t internal) : external(external), internal(internal) {}

        IString external;
        uint32_t internal;
    };


    struct AstNodePtr
    {
        AstNode* ptr;
        AstNodePtr(AstNode* ptr = nullptr) : ptr(ptr) {}
        operator AstNode*() const { return ptr; }
    };

    bool is_double_coerced_call(const PrefixNode& prefix);

    bool is_int_coerced_call(const BinaryNode& binary);
}
