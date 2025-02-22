#include <ir.h>

void destroy_reg(Reg& ir_reg)
{
    UNUSED(ir_reg);
}

b32 is_arg_reg(arg_type type)
{
    return type <= arg_type::dst_src_reg;
}

b32 is_arg_src(arg_type type)
{
    return type == arg_type::dst_src_reg || type == arg_type::dst_src_float || type == arg_type::src_reg || type == arg_type::src_float;
}

constexpr b32 is_arg_src_const(arg_type type)
{
    return type == arg_type::dst_src_reg || type == arg_type::dst_src_float || type == arg_type::src_reg || type == arg_type::src_float;
}

b32 is_arg_dst(arg_type type)
{
    return type >= arg_type::dst_reg && type <= arg_type::dst_src_reg;
}

constexpr b32 is_arg_dst_const(arg_type type)
{
    return type >= arg_type::dst_reg && type <= arg_type::dst_src_reg;
}


b32 is_arg_float(arg_type type)
{
    return type == arg_type::dst_src_float || type == arg_type::src_float || type ==  arg_type::dst_float;
}

constexpr b32 is_arg_float_const(arg_type type)
{
    return type == arg_type::dst_src_float || type == arg_type::src_float || type ==  arg_type::dst_float;
}


u32 gpr_count(u32 size)
{
    return size / GPR_SIZE;
}


b32 is_mem_unallocated(Reg& reg)
{
    return reg.offset == UNALLOCATED_OFFSET;
}

b32 is_mem_allocated(Reg& reg)
{
    return reg.offset != UNALLOCATED_OFFSET;
}

b32 is_stack_unallocated(Reg& reg)
{
    return is_mem_unallocated(reg) && (reg.slot.kind == reg_kind::sym || reg.slot.kind == reg_kind::tmp);
}


b32 pending_stack_allocation(Reg& reg)
{
    return reg.flags & PENDING_STACK_ALLOCATION;
}


b32 is_stored_in_mem(Reg& reg)
{
    return reg.flags & STORED_IN_MEM;
}

// NOTE: this only works for structs, vars i.e power of two aligned sizes
// not arrays
std::pair<u32,u32> calc_alloc_size(u32 size)
{
    if(size > GPR_SIZE)
    {
        return std::pair{GPR_SIZE,gpr_count(size)};
    }

    else
    {
        return std::pair{size,1};
    }       
}

void assign_reg_size(Reg& reg, u32 size)
{
    const auto [reg_size,count]  = calc_alloc_size(size);

    reg.size = reg_size;
    reg.count = count;   
}


// NOTE: this doesn't account for external array storage
// just the type itself!
b32 resides_in_mem(const Type* type)
{
    return is_struct(type) || is_vla(type);
}

b32 is_aliased(const Reg& reg)
{
    return reg.flags & ALIASED;
}

b32 stored_in_mem(const Reg& reg)
{
    return reg.flags & STORED_IN_MEM;
}

b32 is_signed(const Reg& reg)
{
    return reg.flags & SIGNED_FLAG;
}

b32 is_global(const Reg& reg)
{
    return reg.segment == reg_segment::global || reg.segment == reg_segment::constant;
}

b32 is_local(const Reg& reg)
{
    return !is_global(reg);  
}

b32 is_arg(const Reg& reg)
{
    return reg.flags & FUNC_ARG;
}

b32 is_special_reg_fpr(spec_reg reg)
{
    return reg == spec_reg::rv_float;
}

b32 is_special_reg(RegSlot slot)
{
    return slot.kind == reg_kind::spec;
}

b32 is_var(RegSlot slot)
{
    return !is_special_reg(slot);
}

Reg make_reg(Interloper& itl, const RegSlot& slot, const Type* type)
{
    Reg reg;

    reg.slot = slot;

    u32 size = type_size(itl,type);

    // tmp's derived from expression are allways atleast gpr sized
    // this ensures that intermediate results allways get stored at 
    // "max" precision
    if(slot.kind == reg_kind::tmp && size < GPR_SIZE)
    {
        size = GPR_SIZE;
    }

    assign_reg_size(reg,size);

    const b32 sign = is_signed(type);

    if(sign)
    {
        reg.flags |= SIGNED_FLAG;
    }

    const b32 in_mem = resides_in_mem(type);

    if(in_mem)
    {
        reg.flags |= STORED_IN_MEM;
    }

    // store if this reg is const for opt purposes
    if(type->is_const)
    {
        reg.flags |= CONST;
    }

    if(is_float(type))
    {
        reg.flags |= REG_FLOAT;
    }

    return reg;
}

Reg make_reg(const RegSlot& slot, u32 size, b32 is_signed, b32 is_float)
{
    Reg reg;
    reg.slot = slot;

    assign_reg_size(reg,size);

    if(is_signed)
    {
        reg.flags |= SIGNED_FLAG;
    }

    if(is_float)
    {
        reg.flags |= REG_FLOAT;
    }

    return reg;
}


void print(const Reg& reg)
{
    const char* KIND_NAMES[] = {"local","global","constant","tmp"};
    printf("kind: %s\n",KIND_NAMES[u32(reg.slot.kind)]);
    printf("slot: 0x%x\n",reg.slot.kind == reg_kind::tmp? reg.slot.tmp_slot.handle : reg.slot.sym_slot.handle);

    printf("size: %d\n",reg.size);
    printf("count: %d\n",reg.count);

    printf("offset: 0x%x\n",reg.offset);

    printf("local reg: r%x\n",reg.local_reg);
    printf("global reg: r%x\n",reg.global_reg);

    printf("uses: %d\n",reg.cur_local_uses);
    
    for(u32 i = 0; i < count(reg.local_uses); i++)
    {
        printf("use[%d] -> %d\n",i,reg.local_uses[i]);
    }
}

const char* spec_reg_name(spec_reg reg)
{
    return SPECIAL_REG_NAMES[u32(reg)].buf;    
}


const char* reg_name(arch_target arch, u32 reg)
{
    switch(arch)
    {
        case arch_target::x86_64_t:
        {
            if(reg < X86_REG_SIZE)
            {
                return X86_NAMES[reg];
            }

            else if(reg == REG_FREE)
            {
                return "Free";
            }

            else
            {
                return "ERROR";
            }
        }
    }

    return nullptr;
}

RegSlot new_tmp(Function& func, u32 size)
{
    const TmpSlot tmp_slot = {count(func.registers)};

    const auto reg_slot = make_tmp_reg_slot(tmp_slot);

    const auto reg = make_reg(reg_slot,size,false,false);
    push_var(func.registers,reg);

    return reg_slot;
}

RegSlot new_float(Function& func)
{
    const TmpSlot tmp_slot = {count(func.registers)};
    const auto reg_slot = make_tmp_reg_slot(tmp_slot);

    auto reg = make_reg(reg_slot,sizeof(f64),false,true);
    push_var(func.registers,reg);

    return reg_slot;  
}

RegSlot new_tmp_ptr(Function &func)
{
    return new_tmp(func,GPR_SIZE);
}

void free_slot(Interloper& itl,Function& func, const Reg& reg)
{
    free_slot(itl,func,reg.slot);
}

void free_sym(Interloper& itl,Function& func, Symbol& sym)
{
    if(is_fixed_array(sym.type) && (sym.reg.slot.kind == reg_kind::sym || sym.reg.slot.kind == reg_kind::tmp))
    {
        auto [size,count] = calc_arr_allocation(itl,sym);
        free_fixed_array(itl,func,sym.reg.slot,size,count);
    }

    else
    {
        free_slot(itl,func,sym.reg);
    }

    sym.scope_end = cur_block(func);
}


bool is_local_reg(const Reg &reg)
{
    return !is_aliased(reg) && is_local(reg) && !stored_in_mem(reg);
}

const OpInfo& info_from_op(const Opcode& opcode)
{
    return OPCODE_TABLE[u32(opcode.op)];
}

struct ArchInfo
{
    u32 sp;
    u32 rv;
    u32 frv;
    u32 gpr;
    u32 fpr;
};

static constexpr ArchInfo ARCH_TABLE[ARCH_SIZE] = 
{
    {u32(x86_reg::rsp),u32(x86_reg::rax),u32(x86_reg::xmm0),15,15}, // x86
};

ArchInfo info_from_arch(arch_target arch)
{
    return ARCH_TABLE[u32(arch)];
}

u32 arch_sp(arch_target arch)
{
    const auto info = info_from_arch(arch);

    return info.sp;
}

u32 arch_rv(arch_target arch)
{
    const auto info = info_from_arch(arch);

    return info.rv;
}

u32 arch_frv(arch_target arch)
{
    const auto info = info_from_arch(arch);

    return info.frv;
}




b32 is_callee_saved(arch_target arch,u32 reg_idx)
{
    switch(arch)
    {
        case arch_target::x86_64_t:
        {
            const x86_reg reg = x86_reg(reg_idx);

            return reg != x86_reg::rax && reg != x86_reg::rsp;
        }
    }

    assert(false);
}



void log_reg(b32 print,SymbolTable& table, const String& fmt_string, ...)
{  
    if(!print)
    {
        return;
    }

    va_list args;
    va_start(args,fmt_string);

    for(u32 i = 0; i < fmt_string.size; i++)
    {
        if(fmt_string[i] == '%')
        {
            switch(fmt_string[i + 1])
            {
                // string
                case 's':
                {
                    const auto str = va_arg(args, const char*);
                    printf("%s",str);
                    break;
                }

                // hex
                case 'x':
                {
                    const auto v = va_arg(args, u32);

                    printf("%x",v);
                    break;
                }

                // int
                case 'd':
                {
                    const auto v = va_arg(args, s32);

                    printf("%d",v);
                    break;
                }

                // reg
                case 'r':
                {
                    const auto slot = va_arg(args,RegSlot);

                    switch(slot.kind)
                    {
                        case reg_kind::spec:
                        {
                            printf("%s",spec_reg_name(slot.spec));
                            break;
                        }

                        case reg_kind::sym:
                        {
                            const auto &sym = sym_from_slot(table,slot.sym_slot);
                            printf("%s",sym.name.buf);
                            break;
                        }

                        case reg_kind::tmp:
                        {
                            printf("t%d",slot.tmp_slot.handle);
                            break;
                        }
                    }
                    break;
                }

                default: assert(false);
            }


            // account for format
            i += 1;
        }

        else
        {
            putchar(fmt_string[i]);
        }
    }

    va_end(args);
}


struct AbiInfo
{
    u32 rv;
    u32 sp;

    // u32 args[32];
    // u32 arg_count;
};

static constexpr AbiInfo ABI_INFO[] = 
{
    {x86_reg::rax,x86_reg::rsp}, // arch_target::x86_64_t
};

// TODO: This should not just be down to arch
const AbiInfo& get_abi_info(arch_target arch)
{
    return ABI_INFO[u32(arch)];
}

u32 special_reg_to_reg(arch_target arch,spec_reg spec)
{
    switch(spec)
    {
        case spec_reg::sp:
        { 
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    return x86_reg::rsp;
                }
            }
            assert(false);
        }


        case spec_reg::rv: 
        {
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    return x86_reg::rax;
                }
            }
            assert(false);
        }

        case spec_reg::rv_float: 
        {
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    return x86_reg::xmm0;
                }
            }
            assert(false);
        }


        case spec_reg::rax: return u32(x86_reg::rax);
        case spec_reg::rcx: return u32(x86_reg::rcx);
        case spec_reg::rdx: return u32(x86_reg::rdx);
        case spec_reg::rdi: return u32(x86_reg::rdi); 
        case spec_reg::rsi: return u32(x86_reg::rsi); 
        case spec_reg::r8: return u32(x86_reg::r8);
        case spec_reg::r9: return u32(x86_reg::r9);
        case spec_reg::r10: return u32(x86_reg::r10);

        default: crash_and_burn("unhandled special reg %x\n",u32(spec)); 
    }    
}

std::pair<u32,u32> reg_offset(Interloper& itl,const Reg& ir_reg, u32 stack_offset)
{
    switch(ir_reg.segment)
    {
        case reg_segment::local:
        {
            const u32 SP = arch_sp(itl.arch);

            const u32 offset = ir_reg.offset + stack_offset;
            return std::pair{SP,offset};
        }

        case reg_segment::constant:
        {
            const u32 handle = ir_reg.offset;

            const PoolSlot pool_slot = pool_slot_from_idx(handle);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            return std::pair{u32(spec_reg::const_seg),section.offset};
        }

        case reg_segment::global:
        {
            return std::pair{u32(spec_reg::global_seg),ir_reg.offset};
        }
    }

    assert(false);
}