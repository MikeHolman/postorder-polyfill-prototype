#pragma once

using namespace std;

namespace asmjs
{
    class AstMemory
    {
        vector<unique_ptr<uint8_t[]>> allocs_;
        uint8_t* free_;
        size_t remain_;

    public:
        AstMemory() : free_(nullptr), remain_(0) {}

        void* alloc(size_t bytes)
        {
            if (remain_ < bytes) {
                remain_ = max<size_t>(bytes, 64 * 1024);
                allocs_.emplace_back(new uint8_t[remain_]);
                free_ = allocs_.back().get();
            }

            uint8_t* ret = free_;
            free_ += bytes;
            remain_ -= bytes;
            return ret;
        }

    };

    struct AstMemoryBase
    {
        void* operator new(size_t bytes);
    };
}
