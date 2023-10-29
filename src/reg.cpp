#include <ir.h>

static constexpr u32 REG_FREE = SPECIAL_PURPOSE_REG_START - 1;
static constexpr u32 TMP_END = REG_FREE - 1;
static constexpr u32 REG_TMP_START = 0x00000000;

void destroy_reg(Reg& ir_reg)
{
    destroy_arr(ir_reg.usage);
}

b32 is_sym(SymSlot s)
{
    return s.handle >= SYMBOL_START;
}

u32 tmp(u32 ir_reg)
{
    return ir_reg + REG_TMP_START;
}

// dont correct special regs
b32 is_reg(SymSlot r)
{
    return r.handle < MACHINE_REG_SIZE;
}

b32 is_special_reg(SymSlot r)
{
    return r.handle >= SPECIAL_PURPOSE_REG_START && r.handle <= SPECIAL_PURPOSE_REG_START + SPECIAL_REG_SIZE;
}

b32 is_tmp(SymSlot s)
{
    return s.handle < TMP_END;
}


u32 slot_to_idx(SymSlot slot)
{
    return is_sym(slot)? sym_to_idx(slot) : slot.handle;
}

u32 gpr_count(u32 size)
{
    return size / GPR_SIZE;
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


b32 resides_in_mem(const Type* type)
{
    return is_struct(type) || is_array(type);
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

Reg make_reg(Interloper& itl, reg_kind kind,u32 slot, const Type* type)
{
    Reg reg;

    reg.kind = kind;

    const u32 size = type_size(itl,type);

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

    reg.slot = {slot};

    return reg;
}

Reg make_reg(reg_kind kind,u32 size, u32 slot, b32 is_signed)
{
    Reg reg;
    reg.kind = kind;

    assign_reg_size(reg,size);

    if(is_signed)
    {
        reg.flags |= SIGNED_FLAG;
    }

    reg.slot = {slot};

    return reg;
}


void print(const Reg& reg)
{
    const char* KIND_NAMES[] = {"local","global","tmp"};
    printf("kind: %s\n",KIND_NAMES[u32(reg.kind)]);
    printf("slot: 0x%x\n",reg.slot.handle);

    printf("size: %d\n",reg.size);
    printf("count: %d\n",reg.count);

    printf("offset: 0x%x\n",reg.offset);
    printf("locaiton: 0x%x\n",reg.location);

    printf("uses: %d\n",reg.uses);

    for(u32 i = 0; i < count(reg.usage); i++)
    {
        printf("use[%d] -> %d\n",i,reg.usage[i]);
    }
}


SymSlot new_tmp(Function& func, u32 size)
{
    const u32 slot = count(func.registers);

    const auto reg = make_reg(reg_kind::tmp,size,slot,false);
    push_var(func.registers,reg);

    return sym_from_idx(slot);
}

SymSlot new_tmp_ptr(Function &func)
{
    return new_tmp(func,GPR_SIZE);
}

void free_slot(Interloper& itl,Function& func, const Reg& reg)
{
    free_slot(itl,func,reg.slot);
}

void free_sym(Interloper& itl,Function& func, Symbol& sym)
{
    if(is_fixed_array(sym.type) && (sym.reg.kind == reg_kind::local || sym.reg.kind == reg_kind::tmp))
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