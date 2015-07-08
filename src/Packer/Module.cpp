#include "Packer.h"

namespace asmjs
{

void
Module::add_lit(NumLit lit, unsigned lshift)
{
    switch (lit.type()) {
    case Type::I32: i32_lit_map_[lit.uint32() << lshift]++; break;
    case Type::F32: assert(lshift == 0); f32_lit_map_[union_cast<uint32_t>(lit.float32())]++; break;
    case Type::F64: assert(lshift == 0); f64_lit_map_[union_cast<uint64_t>(lit.float64())]++; break;
    }
}

unsigned
Module::imm_len(uint32_t u32)
{
    if (u32 < ImmLimit)
        return 0;
    if (u32 < 0x80)
        return 1;
    if (u32 < 0x4000)
        return 2;
    if (u32 < 0x200000)
        return 3;
    if (u32 < 0x10000000)
        return 4;
    return 5;
}

void
Module::process_i32_lit_map(I32LitMap& map, vector<I32Lit>& i32s)
{
    i32s.assign(map.begin(), map.end());
    sort(i32s.begin(), i32s.end(), [](const I32Lit& lhs, const I32Lit& rhs) { return lhs.second > rhs.second; });
    for (uint32_t pool_index = 0; pool_index < i32s.size();) {
        uint32_t constant = i32s[pool_index].first;
        uint32_t uses = i32s[pool_index].second;
        int savings = imm_len(constant) - imm_len(pool_index);
        if (savings <= 0 || imm_len(constant) >= unsigned(savings) * uses) {
            map.erase(constant);
            i32s.erase(i32s.begin() + pool_index);
        }
        else {
            pool_index++;
        }
    }
    for (size_t i = 0; i < i32s.size(); i++)
        map[i32s[i].first] = static_cast<uint32_t>(i);
}

template <class MapT, class LitT>
void
Module::process_f_lit_map(MapT& map, vector<LitT>& vec)
{
    vec.assign(map.begin(), map.end());
    sort(vec.begin(), vec.end(), [](const LitT& lhs, const LitT& rhs) { return lhs.second > rhs.second; });
    size_t new_size = vec.size();
    for (; new_size > 0 && vec[new_size - 1].second <= 3; new_size--)
        map.erase(vec[new_size - 1].first);
    vec.resize(new_size);
    for (size_t i = 0; i < vec.size(); i++)
        map[vec[i].first] = static_cast<uint32_t>(i);
}

void
Module::finish_analysis()
{
    assert(!finished_analysis_);
    finished_analysis_ = true;

    num_func_imp_sigs_ = 0;
    for (auto& func_imp : func_imps_)
        for (auto& func_imp_sig : func_imp.sigs)
            func_imp_sig.func_imp_sig_index = num_func_imp_sigs_++;

    // Before finish_analysis, the *_lit_maps map literal to use count.
    // After finish_analysis, the *_lit_maps map literal to index in i32s_ (now sorted hottest first).
    process_i32_lit_map(i32_lit_map_, i32s_);
    process_f_lit_map(f32_lit_map_, f32s_);
    process_f_lit_map(f64_lit_map_, f64s_);
}

bool
Module::lit_has_pool_index(NumLit lit, unsigned lshift, uint32_t *index) const
{
    assert(finished_analysis_);
    switch (lit.type()) {
    case Type::I32: {
        auto iter = i32_lit_map_.find(lit.uint32() << lshift);
        if (iter == i32_lit_map_.end())
            return false;
        *index = iter->second;
        return true;
    }
    case Type::F32: {
        assert(lshift == 0);
        auto iter = f32_lit_map_.find(union_cast<uint32_t>(lit.float32()));
        if (iter == f32_lit_map_.end())
            return false;
        *index = iter->second;
        return true;
    }
    case Type::F64: {
        assert(lshift == 0);
        auto iter = f64_lit_map_.find(union_cast<uint64_t>(lit.float64()));
        if (iter == f64_lit_map_.end())
            return false;
        *index = iter->second;
        return true;
    }
    }
    return unreachable<bool>();
}

} // namespace asmjs
