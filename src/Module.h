#pragma once

using namespace std;
using cashew::IString;


namespace asmjs
{

    class Module
    {
    private:
        Out out_;

        IString stdlib_;
        IString foreign_;
        IString buffer_;
        IString fround_;

        vector<Signature> sigs_;
        unordered_map<Signature, uint32_t, Signature::Hash> sig_to_index_;

        unordered_map<IString, uint32_t> func_to_index_;
        vector<Function> funcs_;

        unordered_map<IString, uint32_t> func_import_to_index_;
        vector<FuncImport> func_imps_;
        uint32_t num_func_imp_sigs_;

        unordered_map<IString, uint32_t> func_ptr_table_to_index_;
        vector<FuncPtrTable> func_ptr_tables_;

        typedef unordered_map<uint32_t, uint32_t> I32LitMap;
        typedef unordered_map<uint32_t, uint32_t> F32LitMap;
        typedef unordered_map<uint64_t, uint32_t> F64LitMap;
        I32LitMap i32_lit_map_;
        F32LitMap f32_lit_map_;
        F64LitMap f64_lit_map_;
        typedef pair<uint32_t, uint32_t> I32Lit;
        typedef pair<uint32_t, uint32_t> F32Lit;
        typedef pair<uint64_t, uint32_t> F64Lit;
        vector<I32Lit> i32s_;
        vector<F32Lit> f32s_;
        vector<F64Lit> f64s_;

        unordered_map<IString, Global> globals_;
        size_t num_global_i32_zero_;
        size_t num_global_f32_zero_;
        size_t num_global_f64_zero_;
        vector<IString> global_i32_imports_;
        vector<IString> global_f32_imports_;
        vector<IString> global_f64_imports_;

        unordered_map<IString, double> stdlib_doubles_;
        unordered_map<IString, StdLibFunc> stdlib_funcs_;
        unordered_map<IString, HeapView> heap_views_;
        vector<Export> exports_;

        bool finished_analysis_;

        void assert_unique_global_name(IString i)
        {
            assert(func_to_index_.find(i) == func_to_index_.end());
            assert(func_import_to_index_.find(i) == func_import_to_index_.end());
            assert(func_ptr_table_to_index_.find(i) == func_ptr_table_to_index_.end());
            assert(stdlib_doubles_.find(i) == stdlib_doubles_.end());
            assert(stdlib_funcs_.find(i) == stdlib_funcs_.end());
            assert(heap_views_.find(i) == heap_views_.end());
        }

        template <class SigT>
        size_t get_sig_index(SigT&& sig)
        {
            assert(!finished_analysis_);
            auto iter = sig_to_index_.find(sig);
            if (iter != sig_to_index_.end())
                return iter->second;
            size_t sig_index = sigs_.size();
            sigs_.push_back(sig);
            sig_to_index_.emplace(forward<SigT>(sig), sig_index);
            return sig_index;
        }

    public:
        Module(ostream& os) :
            out_(os),
            finished_analysis_(false) {}

        // ===============================================================================================
        // First-pass functions

        void stdlib(IString name) { stdlib_ = name; }
        void foreign(IString name) { foreign_ = name; }
        void buffer(IString name) { buffer_ = name; }
        IString stdlib() const { return stdlib_; }
        IString foreign() const { return foreign_; }
        IString buffer() const { return buffer_; }
        IString fround() const { return fround_; }

        void add_heap_view(IString name, HeapView v)
        {
            assert_unique_global_name(name);
            heap_views_.emplace(name, v);
        }

        void set_globals(const vector<IString>& i32_zero,
            const vector<IString>& f32_zero,
            const vector<IString>& f64_zero,
            const vector<pair<IString, IString>>& i32_import,
            const vector<pair<IString, IString>>& f32_import,
            const vector<pair<IString, IString>>& f64_import)
        {
            num_global_i32_zero_ = i32_zero.size();
            num_global_f32_zero_ = f32_zero.size();
            num_global_f64_zero_ = f64_zero.size();
            for (auto name : i32_zero)
                globals_.emplace(name, Global(Type::I32, globals_.size()));
            for (auto name : f32_zero)
                globals_.emplace(name, Global(Type::F32, globals_.size()));
            for (auto name : f64_zero)
                globals_.emplace(name, Global(Type::F64, globals_.size()));
            for (auto pair : i32_import) {
                globals_.emplace(pair.first, Global(Type::I32, globals_.size()));
                global_i32_imports_.push_back(pair.second);
            }
            for (auto pair : f32_import) {
                globals_.emplace(pair.first, Global(Type::F32, globals_.size()));
                global_f32_imports_.push_back(pair.second);
            }
            for (auto pair : f64_import) {
                globals_.emplace(pair.first, Global(Type::F64, globals_.size()));
                global_f64_imports_.push_back(pair.second);
            }
        }

        Function &add_func(IString name)
        {
            assert(!finished_analysis_);
            assert_unique_global_name(name);
            size_t func_index = funcs_.size();
            func_to_index_.emplace(name, func_index);
            funcs_.emplace_back();
            return funcs_.back();
        }

        void finish_func(Function& f)
        {
            f.set_sig_index(get_sig_index(f.sig()));
        }

        void add_func_import(IString internal, IString external)
        {
            assert(!finished_analysis_);
            assert_unique_global_name(internal);
            func_import_to_index_.emplace(internal, func_imps_.size());
            func_imps_.emplace_back(external);
        };

        size_t add_import_sig(IString internal, Signature&& sig)
        {
            assert(!finished_analysis_);
            size_t sig_index = get_sig_index(move(sig));
            FuncImport& func_imp = func_imps_[func_import_to_index_.find(internal)->second];
            for (size_t pre_index = 0; pre_index < func_imp.sigs.size(); pre_index++)
                if (sig_index == func_imp.sigs[pre_index].sig_index)
                    return pre_index;
            size_t pre_index = func_imp.sigs.size();
            func_imp.sigs.emplace_back(sig_index);
            return pre_index;
        }

        bool is_import_func(IString name)
        {
            return func_import_to_index_.find(name) != func_import_to_index_.end();
        }

        void add_stdlib_double(IString name, double d)
        {
            assert(!finished_analysis_);
            assert_unique_global_name(name);
            stdlib_doubles_.emplace(name, d);
        }

        bool is_stdlib_double(IString name) { return stdlib_doubles_.find(name) != stdlib_doubles_.end(); }
        double stdlib_double(IString name) { return stdlib_doubles_.find(name)->second; }

        template <class CodeT>
        void add_stdlib_func(IString name, CodeT code)
        {
            assert(!finished_analysis_);
            assert_unique_global_name(name);
            stdlib_funcs_.emplace(name, StdLibFunc(code));
        }

        void add_fround(IString name)
        {
            add_stdlib_func(name, PreTypeSignCode::ToF32);
            fround_ = name;
        }

        bool is_stdlib_func(IString name) { return stdlib_funcs_.find(name) != stdlib_funcs_.end(); }
        const StdLibFunc& stdlib_func(IString name) { return stdlib_funcs_.find(name)->second; }

        void add_func_ptr_table(IString name, vector<uint32_t>&& elems)
        {
            assert(!finished_analysis_);
            assert_unique_global_name(name);
            auto& sig = funcs_[elems[0]].sig();
            for (size_t func_index : elems)
                assert(sig == funcs_[func_index].sig());
            size_t sig_index = get_sig_index(sig);
            func_ptr_table_to_index_.emplace(name, func_ptr_tables_.size());
            func_ptr_tables_.emplace_back(sig_index, move(elems));
        }

        void add_default_export(IString internal_name)
        {
            assert(!finished_analysis_);
            assert(exports_.empty());
            auto result = func_to_index_.find(internal_name);
            exports_.push_back(Export(IString(), result->second));
        }

        void add_record_export(IString external, IString internal_name)
        {
            assert(!finished_analysis_);
            auto result = func_to_index_.find(internal_name);
            exports_.push_back(Export(external, result->second));
        }

        void add_lit(NumLit lit, unsigned lshift);

        unsigned imm_len(uint32_t u32);

        void process_i32_lit_map(I32LitMap& map, vector<I32Lit>& i32s);

        template <class MapT, class LitT>
        void process_f_lit_map(MapT& map, vector<LitT>& vec);

        void finish_analysis();

        // ===============================================================================================
        // Second-pass functions

        Out& write() { assert(finished_analysis_); return out_; }

        Global global(IString name) const { return globals_.find(name)->second; }
        size_t num_global_i32_zero() const { return num_global_i32_zero_; }
        size_t num_global_f64_zero() const { return num_global_f64_zero_; }
        size_t num_global_f32_zero() const { return num_global_f32_zero_; }
        const vector<IString>& global_i32_imports() const { return global_i32_imports_; }
        const vector<IString>& global_f32_imports() const { return global_f32_imports_; }
        const vector<IString>& global_f64_imports() const { return global_f64_imports_; }

        const vector<Signature>& sigs() { assert(finished_analysis_); return sigs_; }
        const Signature& sig(size_t i) { assert(finished_analysis_); return sigs_[i]; }

        vector<Function>& funcs() { assert(finished_analysis_); return funcs_; }
        uint32_t func_index(IString name) const { return func_to_index_.find(name)->second; }
        Function& func(uint32_t func_index) { return funcs_[func_index]; }

        vector<FuncImport>& func_imps() { assert(finished_analysis_); return func_imps_; }
        FuncImport& func_imp(IString name) { assert(finished_analysis_); return func_imps_[func_import_to_index_.find(name)->second]; }
        uint32_t num_func_imp_sigs() const { assert(finished_analysis_); return num_func_imp_sigs_; }

        const vector<FuncPtrTable>& func_ptr_tables() const { return func_ptr_tables_; }
        uint32_t func_ptr_table_index(IString name) const { return func_ptr_table_to_index_.find(name)->second; }
        const FuncPtrTable& func_ptr_table(size_t i) const { return func_ptr_tables_[i]; }

        HeapView heap_view(IString name) const { return heap_views_.find(name)->second; }
        const vector<Export>& exports() const { assert(finished_analysis_); return exports_; }

        bool lit_has_pool_index(NumLit lit, unsigned lshift, uint32_t *index) const;
        const vector<I32Lit>& i32s() const { assert(finished_analysis_); return i32s_; }
        const vector<F32Lit>& f32s() const { assert(finished_analysis_); return f32s_; }
        const vector<F64Lit>& f64s() const { assert(finished_analysis_); return f64s_; }
    };

} // namespace asmjs
