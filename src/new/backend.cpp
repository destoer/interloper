#include <interloper.h>
#include "backend/op_table.inl"

#include "backend/pool.cpp"
#include "backend/emitter.cpp"
#include "backend/rewrite_arch_ir.cpp"
#include "backend/cfg.cpp"
#include "backend/asm.cpp"
#include "backend/x86_emitter.cpp"
#include "backend/stack_allocator.cpp"
#include "backend/linear_alloc.cpp"
#include "backend/disass.cpp"
#include "backend/ir_x86.cpp"
#include "backend/elf.cpp"
#include "backend/intrin.cpp"
#include "backend/ir.cpp"


Option<itl_error> backend(Interloper& itl)
{
    UNUSED(itl);
    assert(false);   

    return option::none;
}