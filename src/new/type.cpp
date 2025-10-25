#include <interloper.h>

DefInfo* lookup_definition(NameSpace* root, const String& name);
void print_namespace_tree(NameSpace* root, u32 depth);
void init_arr_sub_sizes(Interloper&itl,Type* type);
void store_arr_data(Interloper& itl, Function& func, RegSlot slot, RegSlot data);
void store_arr_len(Interloper& itl, Function& func, RegSlot slot,RegSlot len);

RegSlot mul_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm);
RegSlot udiv_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm);

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},
    {builtin_type::u64_t, true, false ,8,  0, u64(0xffffffff'ffffffff)},

    {builtin_type::s8_t, true, true, 1, u64(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  u64(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  u64(-(0xffffffff / 2)), (0xffffffff / 2)},
    {builtin_type::s64_t, true, true ,8,  u64(-(0xffffffff'ffffffff / 2)), u64(0xffffffff'ffffffff / 2)},

    {builtin_type::c8_t, true, false, 1, 0, 0xff},

    {builtin_type::byte_t, true, false, 1, 0, 0xff},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    // note: the range limits on this aren't really integral
    {builtin_type::f64_t, false, false ,8,  0, u64(0xffffffff'ffffffff)},

    {builtin_type::null_t, false,false, GPR_SIZE,0,0},

    // internal

    {builtin_type::void_t, false, false, 0, 0, 0},
};


#include "type/helper.cpp"
#include "type/format.cpp"
#include "type/size.cpp"
#include "type/lookup.cpp"
#include "type/checker.cpp"
#include "type/ast.cpp"

void add_type_to_scope(NameSpace* name_space, TypeDecl* decl)
{
    DefInfo info;
    info.type = definition_type::type;
    info.type_decl = decl;

    add(name_space->table,decl->name,info);
}

template<typename T>
T* alloc_type_decl(Interloper& itl)
{
    T* out = (T*)allocate(itl.type_allocator,sizeof(T));
    *out = {};

    return out;
}

void add_internal_type_decl(Interloper& itl, u32 type_idx, const String& name, type_kind kind)
{
    TypeDecl* type_decl = alloc_type_decl<TypeDecl>(itl);

    type_decl->type_idx = type_idx;
    type_decl->name = name;
    type_decl->kind = kind;
    type_decl->name_space = itl.global_namespace;
    type_decl->state = type_def_state::checked;
    
    add_type_to_scope(itl.global_namespace,type_decl);    
}


void add_type_definition(Interloper& itl, type_def_kind kind, AstNode* root, const String& name, const String& filename, NameSpace* name_space)
{
    TypeDef* definition = alloc_type_decl<TypeDef>(itl);

    definition->decl.name = name;
    definition->decl.flags = TYPE_DECL_DEF_FLAG;
    definition->decl.name_space = name_space;
    definition->decl.kind = type_kind(kind);

    definition->filename = filename;
    definition->root = root;
    definition->kind = kind;


    add_type_to_scope(name_space,(TypeDecl*)definition);
    push_var(itl.type_decl,root);
}


b32 type_exists(Interloper& itl, const String& name)
{
    return lookup_complete_decl(itl,name) != nullptr;
}


ConstValueResult access_builtin_type_info(Interloper& itl, builtin_type type, const String& member_name)
{
    const BuiltinTypeInfo& info = builtin_type_info[u32(type)];

    if(member_name == "size")
    {
        return ConstValue{make_builtin(itl,builtin_type::u32_t),u64(info.size)};
    }

    else if(member_name == "max")
    {
        return ConstValue{make_builtin(itl,builtin_type::u32_t),info.max};
    }

    else if(member_name == "min")
    {
        return ConstValue{make_builtin(itl,builtin_type::u32_t),info.min};
    }

    return compile_error(itl,itl_error::undefined_type_oper,"unknown type info for builtin type %s.%S",TYPE_NAMES[u32(type)],member_name);
}



TypeResult access_builtin_type_info(Interloper& itl, Function& func, RegSlot dst_slot, builtin_type type, const String& member_name)
{
    auto type_info_res = access_builtin_type_info(itl,type,member_name);

    if(!type_info_res)
    {
        return type_info_res.error();
    }

    auto data = *type_info_res;
    mov_imm(itl,func,dst_slot,data.value);

    return data.type;
}


ConstValueResult access_type_info(Interloper& itl,const TypeDecl& type_decl, const String& member_name)
{
    switch(type_decl.kind)
    {
        case type_kind::builtin:
        {
            builtin_type type = builtin_type(type_decl.type_idx);

            return access_builtin_type_info(itl,type,member_name);
        }

        case type_kind::struct_t:
        {
            if(member_name == "size")
            {
                const auto& structure = itl.struct_table[type_decl.type_idx];
                const u64 size = structure.size;

                return ConstValue{make_builtin(itl,builtin_type::u32_t),size};
            }

            else
            {
                return compile_error(itl,itl_error::enum_type_error,"unknown type info for struct %S",type_decl.name);
            }
        }

        case type_kind::enum_t:
        {
            if(member_name == "len")
            {
                const auto enumeration = itl.enum_table[type_decl.type_idx];

                const u64 enum_len = enumeration.member_map.size;

                return ConstValue{make_builtin(itl,builtin_type::u32_t),enum_len};
            }

            else
            {
                return compile_error(itl,itl_error::enum_type_error,"unknown type info for enum %S",type_decl.name);
            }
        }

        case type_kind::alias_t:
        {
            return compile_error(itl,itl_error::generic_type_error,"cannot access type properties on alias %S",type_decl.name);
        }
    }

    assert(false);
}

TypeResult access_type_info(Interloper& itl, Function& func, RegSlot dst_slot, const TypeDecl& type_decl, const String& member_name)
{
    auto type_info_res = access_type_info(itl,type_decl,member_name);

    if(!type_info_res)
    {
        return type_info_res.error();
    }

    auto [type,ans] = *type_info_res;

    mov_imm(itl,func,dst_slot,ans);

    return type;
}



void add_internal_alias(Interloper& itl, Type* type,const String& name)
{
    const u32 type_idx = count(itl.alias_table);
    add_internal_type_decl(itl,type_idx,name,type_kind::alias_t); 
    push_var(itl.alias_table,type);   
}

void finalise_type(TypeDecl& decl, u32 type_idx)
{
    decl.type_idx = type_idx;
    decl.state = type_def_state::checked;
}

Option<itl_error> parse_alias_def(Interloper& itl, TypeDef& def)
{
    AliasNode* node = (AliasNode*)def.root;

    auto type_res = get_complete_type(itl,node->type);

    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;

    if(itl.print_types)
    {
        printf("type alias %s = %s\n",node->name.buf,type_name(itl,type).buf);
    }

    const u32 type_idx = count(itl.alias_table);
    finalise_type(def.decl,type_idx);
    push_var(itl.alias_table,type); 

    return option::none;
}

void declare_compiler_type_aliases(Interloper& itl) 
{
    /// usize
    add_internal_alias(itl,make_builtin(itl,builtin_type::u64_t),"usize");

    // ssize
    add_internal_alias(itl,make_builtin(itl,builtin_type::s64_t),"ssize");

    add_internal_alias(itl,make_array(itl,make_builtin(itl,builtin_type::c8_t,false),RUNTIME_SIZE),"string");
}

Option<itl_error> parse_struct_def(Interloper& itl, TypeDef& def);
Option<itl_error> parse_alias_def(Interloper& itl, TypeDef& def);
Option<itl_error> parse_enum_def(Interloper& itl, TypeDef& def, Set<u64>& set);

Option<itl_error> parse_def(Interloper& itl, TypeDef& def)
{
    log(itl.itl_log,"Parse type: %s\n",def.decl.name.buf);
    // this node make be from a different context
    // save the current one
    push_context(itl);

    Option<itl_error> res = option::none;

    switch(def.decl.state)
    {
        case type_def_state::not_checked:
        {
            // mark as checking to lock this against recursion!
            def.decl.state = type_def_state::checking;

            switch(def.kind)
            {
                case type_def_kind::struct_t:
                {
                    res = parse_struct_def(itl,def);
                    break;
                }

                case type_def_kind::alias_t:
                {
                    res = parse_alias_def(itl,def);
                    break;
                }

                case type_def_kind::enum_t: 
                {
                    auto set = make_set<u64>();

                    res = parse_enum_def(itl,def,set);
                    destroy_set(set);
                    break;
                }
            }

            break;
        }

        case type_def_state::checking:
        {
            // TODO: add heuristics to scan for where!
            return compile_error(itl,itl_error::black_hole,"Parse def: type %S is recursively defined",def.decl.name);
        }

        // already checked we don't care
        case type_def_state::checked:
        {
            break;
        }
    }

    log(itl.itl_log,"Finish parsing type: %s\n",def.decl.name.buf);

    pop_context(itl);
    return res;
}


void destroy_sig(FuncSig& sig)
{
    destroy_arr(sig.return_type);
    destroy_arr(sig.args);
    destroy_arr(sig.pass_as_reg);
}

void destroy_func(Function& func)
{
    destroy_sig(func.sig);

    for(auto& reg : func.registers)
    {
        destroy_reg(reg);
    }

    destroy_arr(func.registers);
    destroy_emitter(func.emitter);
}