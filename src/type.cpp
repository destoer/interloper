#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, static_cast<u32>(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  static_cast<u32>(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  static_cast<u32>(-(0xffffffff / 2)), (0xffffffff / 2)},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    {builtin_type::void_t, false, false, 0, 0, 0},
};

Type type_array(builtin_type t, u32 size, bool is_const = false)
{
    auto rtype = Type(t);
    rtype.degree = 1;
    rtype.dimensions[0] = size;
    rtype.is_const = is_const;

    return rtype;   
}


const char *builtin_type_name(builtin_type t)
{
    return TYPE_NAMES[static_cast<size_t>(t)];
}

bool is_builtin(u32 type_idx)
{
    return type_idx < BUILTIN_TYPE_SIZE;
}

bool is_struct(u32 type_idx)
{
    return type_idx >= BUILTIN_TYPE_SIZE;
}

bool is_struct(const Type& t)
{
    return is_struct(t.type_idx);
}

bool is_builtin(const Type &t)
{
    return is_builtin(t.type_idx);
}


const char *base_type_name(Interloper& itl,u32 type_idx)
{
    UNUSED(itl);

    if(is_builtin(type_idx))
    {
        return builtin_type_name(builtin_type(type_idx));
    }

    else
    {
        unimplemented("user defined type base name");
    }
}


builtin_type conv_type_idx(int type_idx)
{
    return static_cast<builtin_type>(type_idx);
}

b32 is_runtime_size(const Type& type, u32 idx)
{
    return type.dimensions[idx] >= RUNTIME_SIZE;
}

b32 is_runtime_size(u32 size)
{
    return size >= RUNTIME_SIZE;
}

// NOTE: we can have a runtime sized array
// but still have an initial on it
u32 initial_runtime_size(u32 size)
{
    return size - RUNTIME_SIZE;
}

u32 make_runtime_size(u32 size)
{
    return size + RUNTIME_SIZE;
}

bool is_simple_type(const Type &type)
{
    return !type.degree;
}

// just a plain type, i.e has no array anywhere in its definiton
bool same_simple_type(const Type &type1, const Type &type2)
{
    return (type1.type_idx == type2.type_idx) && (type1.ptr_indirection == type2.ptr_indirection); 
}

// get the base type the array holds
// i.e u32@ or u32
// not u32[]
Type contained_arr_type(const Type &type)
{
    Type accessed_type = type;
    accessed_type.degree = 0;

    return accessed_type;
}


// pointer is active if not contained by an array
// of any kind
bool pointer_active(const Type &t)
{
    return t.degree == 0 || !t.contains_ptr;
}

bool is_pointer(const Type &t)
{
    return t.ptr_indirection && pointer_active(t); 
}

// NOTE: we use this to signify we are taking a fixed sized array
// from something else and want to assign it to a vla etc
// (under normal circumstances you should not be able to access to a pointer of this kind)
// as we want to hide to semantics of its representation so we can use it
// like any other array, there may be a better way to acheive this

bool is_fixed_array_pointer(const Type& t)
{
    return is_pointer(t) && t.degree && t.dimensions[0] != RUNTIME_SIZE;
}


bool is_array(const Type &t)
{
    return t.degree >= 1 && !is_pointer(t);
}

bool is_plain(const Type &t)
{
    return !is_pointer(t) && !is_array(t);
}

bool is_plain_builtin(const Type &t)
{
    return is_builtin(t) && is_plain(t);
}


bool is_trivial_copy(const Type &t)
{
    return is_pointer(t) || is_plain_builtin(t);
}


bool is_bool(const Type &t)
{
    return is_plain_builtin(t) && conv_type_idx(t.type_idx) == builtin_type::bool_t;
}

bool is_integer(const Type &t)
{
    return is_plain_builtin(t) && builtin_type_info[t.type_idx].is_integer;
}

bool is_signed(const Type &t)
{
    return is_plain_builtin(t) && builtin_type_info[t.type_idx].is_signed;
}

bool is_signed_integer(const Type &t)
{
    return is_signed(t) && is_integer(t);
}

u32 builtin_size(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].size;
}

u32 builtin_max(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].max;
}

u32 builtin_min(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].min;
}


builtin_type cast_builtin(Type &type)
{
    return static_cast<builtin_type>(type.type_idx);
}


u32 type_size(Interloper& itl,const Type &type)
{
    UNUSED(itl);

    if(is_plain_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_size(static_cast<builtin_type>(type.type_idx));
    }

    else if(is_pointer(type))
    {
        return GPR_SIZE;
    }
    
    // TODO: this doesnt handle VLA
    else if(is_array(type))
    {
        if(is_runtime_size(type,0))
        {
            return GPR_SIZE * 2;
        }

        return GPR_SIZE;
    }

    // user defined type
    else
    {
        const auto& structure = struct_from_type_idx(itl.struct_table,type.type_idx);
        return structure.size;
    }
}

u32 type_min(Interloper& itl,const Type &type)
{
    UNUSED(itl);

    if(is_plain_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_min(static_cast<builtin_type>(type.type_idx));
    }

    else
    {
        unimplemented("user defined type min\n");
    }
}

u32 type_max(Interloper& itl,const Type &type)
{
    UNUSED(itl);

    if(is_plain_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_max(static_cast<builtin_type>(type.type_idx));
    }

    else
    {
        unimplemented("user defined type max\n");
    }
}

// NOTE: when we we impl fixed size arrays the count should probably
// return any initial size that it has and not just the initial size

// NOTE: this needs to be reworked to support deduced sizes

std::pair<u32,u32> arr_size(Interloper&itl,const Type& arr_type)
{
    u32 count = arr_type.dimensions[0];
    for(u32 i = 0; i < arr_type.degree; i++)
    {
        if(arr_type.dimensions[i] == DEDUCE_SIZE)
        {
            return std::pair<u32,u32>{DEDUCE_SIZE,DEDUCE_SIZE};
        }

        else if(is_runtime_size(arr_type,i))
        {
            return std::pair<u32,u32>{RUNTIME_SIZE,RUNTIME_SIZE};
        }


        // accumulate the count
        if(i == 0)
        {
            count = arr_type.dimensions[i];
        }

        else
        {
            count *= arr_type.dimensions[i];
        }
    }

    const auto contained_type = contained_arr_type(arr_type);
    const u32 size = type_size(itl,contained_type);

    return std::pair<u32,u32>{size,count};
}

std::string fmt_index(u32 index)
{
    if(index == RUNTIME_SIZE)
    {
        return "[]";
    }

    return "[" + std::to_string(index) +  "]";
}

std::string type_name(Interloper& itl,const Type &type)
{
    UNUSED(itl);

    if(is_builtin(type))
    {
        std::string plain = builtin_type_name(static_cast<builtin_type>(type.type_idx));

        if(type.is_const)
        {
            plain = "const " + plain;
        }

        // TODO: this type printing does not handle nesting

        // could be pointer to an array
        if(!type.contains_ptr)
        {
            for(u32 i = 0; i < type.degree; i++)
            {
                plain = plain + fmt_index(type.dimensions[i]);
            }  

            for(u32 i = 0; i < type.ptr_indirection; i++)
            {
                plain = plain + "@";
            } 
        }

        // could be array of pointers
        else
        {
            for(u32 i = 0; i < type.ptr_indirection; i++)
            {
                plain = plain + "@";
            } 

            for(u32 i = 0; i < type.degree; i++)
            {
                plain = plain + fmt_index(type.dimensions[i]);
            }       
        }
        return plain;
    }


    else
    {
        unimplemented("type_name: user defined type");
        return "undefined_type";
    }
}

// TODO: do we want to pass the operation in here for when we support overloading?
Type effective_arith_type(Interloper& itl,const Type &ltype, const Type &rtype)
{
    UNUSED(itl);

    // builtin type
    if(is_plain_builtin(rtype) && is_plain_builtin(ltype))
    {
        // both integers
        if(is_integer(rtype) && is_integer(ltype))
        {
            const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
            const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

            // return the larger size of the type (promotion)
            return (builtin_size(builtin_l) > builtin_size(builtin_r))? ltype : rtype; 
        }

        // something else
        else
        {
            panic(itl,"arithmetic operation undefined for %s and %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            return Type(builtin_type::void_t);
        }

    }

    // one or more user defined
    else
    {
        unimplemented("user defined type arithmetic!\n");       
    }
}

void check_logical_operation(Interloper& itl,const Type &ltype, const Type &rtype)
{
    UNUSED(itl);

    // both are builtin
    if(is_plain_builtin(rtype) && is_plain_builtin(ltype))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers 
        if(is_integer(rtype) && is_integer(ltype))
        {
            if(is_signed(rtype) != is_signed(ltype))
            {
                panic(itl,"logical comparision on different signs %s and %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }
        }

        // both bool
        else if(builtin_r == builtin_type::bool_t && builtin_l == builtin_type::bool_t)
        {
            
        }

        // something else
        else
        {
            panic(itl,"logical operation undefined for %s and %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
        }
    }

    // one or more type is user defined
    // here probably the only valid thing is both are the same
    else
    {
        unimplemented("check assign user defined type!\n");
    }   
}


bool is_value_type(const Type& type)
{
    return is_plain(type);
}

void check_const(Interloper&itl, const Type& ltype, const Type& rtype, bool is_arg, bool is_initializer)
{
    // handle const
    // TODO: this does not typecheck arrays yet
    if(rtype.is_const)
    {
        if(is_arg)
        {
            // if both are value types this is fine as its just a copy
            if(is_value_type(rtype) && is_value_type(rtype))
            {

            }

            // if the ltype is const and the rtype is not this is illegal
            else if(!ltype.is_const)
            {
                panic(itl,"cannot pass const ref to mut ref: %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
                return;
            }
        }

        else
        {
            // ltype is const
            // only valid given an initialisation
            if(ltype.is_const && !is_initializer)
            {
                panic(itl,"cannot to const: %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }

            // ltype is not const, fine given that the rtype is a value type
            else if(!is_value_type(rtype))
            {
                panic(itl,"cannot assign const ref to mut ref: %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }  
        }
    }

    
    else if(ltype.is_const)
    {
        // if its an arg or initalizer its fine
        if(!is_initializer && !is_arg)
        {
            panic(itl,"cannot assign to const: %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            return;
        }
    }
}

void check_assign(Interloper& itl,const Type &ltype, const Type &rtype, bool is_arg = false, bool is_initializer = false)
{
    // check const first
    check_const(itl,ltype,rtype,is_arg,is_initializer);


    // if we have the same types we dont care
    if(is_simple_type(ltype) && is_simple_type(rtype) && same_simple_type(ltype,rtype))
    {
        return;
    }

    // both are builtin
    if(is_plain_builtin(rtype) && is_plain_builtin(ltype))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers
        if(is_integer(ltype) && is_integer(rtype))
        {
            // would narrow (assign is illegal)
            if(builtin_size(builtin_l) < builtin_size(builtin_r))
            {
                panic(itl,"narrowing conversion %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }

            // unsigned cannot assign to signed
            // TODO: do we want to be this pedantic with integer conversions?
            if(!is_signed(builtin_l) && is_signed(builtin_r))
            {
                panic(itl,"unsigned = signed (%s = %s)\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            // void is not assignable!
            if(builtin_r == builtin_type::void_t || builtin_l == builtin_type::void_t)
            {
                panic(itl,"void assign %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }

            else
            {
                print(itl.cur_line);
                unimplemented("non integer assign %s = %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
            }           
        }
    }

    // check assign by ltype
    else
    {
        if(is_pointer(ltype))
        {
            if(ltype.ptr_indirection != rtype.ptr_indirection)
            {
                panic(itl,"expected pointer of type %s got %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
                return;
            }
        }

        else if(is_array(ltype))
        {
            // type idx along with the indirection, and contain type
            // must be the same
            if(!is_array(rtype))
            {
                panic(itl,"expected array of %s got %s\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
                return;
            }

            if(ltype.type_idx != rtype.type_idx)
            {
                panic(itl,"expected array of underlying type %s got %s\n",base_type_name(itl,ltype.type_idx),base_type_name(itl,rtype.type_idx));
                return;
            }

            if(ltype.ptr_indirection != rtype.ptr_indirection)
            {
                panic(itl,"expected pointer of indirection %d got %d\n",ltype.ptr_indirection,rtype.ptr_indirection);
                return;
            }

            if(ltype.degree != rtype.degree)
            {
                panic(itl,"expected array of degree %d got %d\n",ltype.degree,rtype.degree);
                return;
            }


            // when we conv them the rule is to push struct convs to a vla
            // until we hit the end or something that is allready a vla
            // as it will allready hold it in the correct from 

            // dimension assign
            // assign to var size, have to be equal or a runtime size
            // [][] = [][3]
            // [][3] = [][3]
            // [][] = [][]
            // [][] = [3][3] 


            // for arg passing only
            // valid
            // [3] = [3]


            // we need to make sure we cant
            // assign to static arrays?
            // wait do we actually care....
            if(!is_arg)
            {
                if(!is_runtime_size(ltype.dimensions[0]))
                {
                    panic(itl,"%s = %s, cannot assign to fixed size array\n",type_name(itl,ltype).c_str(),type_name(itl,rtype).c_str());
                    return;
                }
            }

            for(u32 i = 0; i < ltype.degree; i++)
            {
                // any assignment is valid if the dst is a vla
                if(!is_runtime_size(ltype.dimensions[i]))
                {
                    if(ltype.dimensions[i] != rtype.dimensions[i])
                    {
                        panic(itl,"(%d) expected array of size %d got %d\n",i,ltype.dimensions[i],rtype.dimensions[i]);
                        return;
                    }
                }
            }
            

        }


        else
        {
            unimplemented("check assign user defined type!\n");
        }
    }
}

// start here
// we need to implement proper stores and loads 
// for each type first
void handle_cast(Interloper& itl,IrEmitter &emitter, u32 dst_slot,u32 src_slot,const Type &old_type, const Type &new_type)
{
    UNUSED(itl);

    // we dont care if we have the same type
    // i.e this cast does nothing
    if(is_simple_type(old_type) && is_simple_type(new_type) && same_simple_type(old_type,new_type))
    {
        return;
    }



    // handle side effects of the cast
    // builtin type
    if(is_plain_builtin(old_type) && is_plain_builtin(new_type))
    {
        const auto builtin_old = static_cast<builtin_type>(old_type.type_idx);
        const auto builtin_new = static_cast<builtin_type>(new_type.type_idx);

        // integer
        if(is_integer(old_type) && is_integer(new_type))
        {
            // TODO: make sure this is optimised out

            // unsigned -> larger type
            // zero extend 
            // (this is done by default)
            
            
            // signed -> larger type
            // sign extend
            if(is_signed(old_type) && is_signed(new_type) && 
                builtin_size(builtin_old) < builtin_size(builtin_new))
            {
                switch(builtin_old)
                {
                    case builtin_type::s8_t: 
                    {
                        emit(emitter,op_type::sxb,dst_slot,src_slot);
                        break;
                    }

                    case builtin_type::s16_t:
                    {
                        emit(emitter,op_type::sxh,dst_slot,src_slot);
                        break;
                    }

                    default: panic("invalid signed integer upcast");
                }
            }

            // larger type -> smaller type
            // truncate value (mask)
            else if(builtin_size(builtin_old) > builtin_size(builtin_new))
            {
                switch(builtin_size(builtin_new))
                {
                    case 1: 
                    {
                        emit(emitter,op_type::and_imm,dst_slot,src_slot,0xff);
                        break;
                    }

                    case 2:  
                    {
                        emit(emitter,op_type::and_imm,dst_slot,src_slot,0xffff);
                        break;
                    }

                    default: panic("invalid signed integer downcast");
                }
            }

            // cast doesnt do anything but move into a tmp so the IR doesnt break
            else
            {
                emit(emitter,op_type::mov_reg,dst_slot,src_slot);
            }

        }

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(builtin_new))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
            emit(emitter,op_type::mov_reg,dst_slot,src_slot);
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(builtin_old) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(builtin_old))
            {
                emit(emitter,op_type::cmpsgt_imm,dst_slot,src_slot,0);
            }

            // unsigned
            else
            {
                emit(emitter,op_type::cmpugt_imm,dst_slot,src_slot,0);
            }
        }        

        else
        {
            unimplemented("handle cast builtin illegal %s -> %s\n",type_name(itl,old_type).c_str(),type_name(itl,new_type).c_str());
        }
    }

    // probably only pointers are gonna valid for casts here
    else
    {
        unimplemented("handle cast user defined type!\n");        
    }

}


Type get_type(Interloper &itl, AstNode *type_decl)
{
    Type type;

    if(type_decl->type_idx == STRUCT_IDX)
    {
        const auto name = type_decl->literal;

        const auto struct_opt = get_struct(itl.struct_table,name);

        if(struct_opt)
        {
            const auto structure = struct_opt.value();
            type.type_idx = structure.type_idx;
        }

        else
        {
            panic(itl,"no such struct %s\n",name.c_str());
            return type;
        }
    }

    else
    {
        type.type_idx = type_decl->type_idx;
    }

    // not a plain plain type
    if(type_decl->nodes.size())
    {
        AstNode *arr_decl = nullptr;
        AstNode *ptr_decl = nullptr;

        for(auto n : type_decl->nodes)
        {
            switch(n->type)
            {
                // type is a constant
                case ast_type::const_t:
                {
                    type.is_const = true;
                    break;
                }

                case ast_type::ptr_indirection:
                {
                    ptr_decl = n;
                    break;
                }

                case ast_type::arr_dimensions:
                {
                    if(ptr_decl)
                    {
                        type.contains_ptr = true;
                    }
                    arr_decl = n;
                    break;
                }

                default: assert(false);
            }
        }

        // parse out pointer indirection
        if(ptr_decl)
        {
            type.ptr_indirection = ptr_decl->type_idx;
        }

        // parse out array dimensions
        if(arr_decl)
        {
            type.degree = arr_decl->nodes.size();
            
            for(u32 i = 0; i < type.degree; i++)
            {
                if(i >= MAX_ARR_SIZE)
                {
                    panic(itl,"array dimensions execeeded %s\n",type_decl->literal.c_str());
                    return type;
                }

                auto n = arr_decl->nodes[i];

                // variable size
                if(n->type == ast_type::arr_var_size)
                {
                    type.dimensions[i] = RUNTIME_SIZE;
                }

                else if(n->type == ast_type::arr_deduce_size)
                {
                    type.dimensions[i] = DEDUCE_SIZE;
                }

                // fixed size: const expr
                else
                {
                    type.dimensions[i] = eval_const_expr(n);
                }
            }
        }
    }

    return type;
}