#include "Packer.h"

namespace asmjs
{
AstMemory ast_memory;

void *
AstMemoryBase::operator new(size_t bytes)
{
    return ast_memory.alloc(bytes);
}

}
