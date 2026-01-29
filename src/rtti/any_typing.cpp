#include "interloper.h"
#include "type.h"
void destroy_trie(TypeTrieNode& root)
{
    for(u32 i = 0; i < count(root.nodes); i++)
    {
        destroy_trie(root.nodes[i]);
        destroy_arr(root.nodes);

        root = {};
    }
}

void destroy_rtti_cache(RttiCache& cache)
{
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        destroy_trie(cache.builtin_type_cache[i]);
        cache.builtin_type_cache[i] = {};
    }

    cache = {};
}

TypeTrieNode& make_rtti_internal(Interloper& itl, const Type* type);

TypeTrieNode& make_rtti_array(Interloper& itl, const ArrayType* array_type)
{
    auto& rtti = itl.rtti_cache;

    TypeTrieNode& root = make_rtti_internal(itl,array_type->contained_type);
    
    // try find an existing node
    for(auto& node:root.nodes)
    {
        // if we find a complete match return
        if(is_array(node.type))
        {
            ArrayType* node_type = (ArrayType*)node.type;
            
            if(node_type->size == array_type->size)
            {
                return node;
            }
        }
    }

    // there is no node that meets the requirements we will have to dump in another

    // reserve room for array type
    const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.array_struct_size);
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    // push in base type
    write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::array_t));   

    // write in array type
    write_const_pool_pointer(itl.const_pool,section,rtti.array_contained_offset,root.slot);
    write_const_pool(itl.const_pool,section,rtti.array_size_offset,array_type->size);
    write_const_pool(itl.const_pool,section,rtti.array_sub_size_offset,array_type->sub_size);

    rtti.type_data_size += section.size;

    TypeTrieNode node;

    node.slot = slot;
    node.type = (Type*)array_type;

    push_var(root.nodes,node);

    //return newly inserted node
    return root.nodes[count(root.nodes) - 1];    
}

TypeTrieNode& make_rtti_pointer(Interloper& itl, const PointerType* pointer_type)
{
    auto& rtti = itl.rtti_cache;

    // get contained type
    TypeTrieNode& root  = make_rtti_internal(itl,pointer_type->contained_type);

    // try find an existing node
    for(u32 i = 0; i < count(root.nodes); i++)
    {
        auto& node = root.nodes[i];

        if(is_pointer(node.type))
        {
            return node;
        }
    }

    // no matching pointer make one

    // reserve room for pointer type
    const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.pointer_struct_size);
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    // push in base type
    write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::pointer_t));            

    // push in pointer type
    write_const_pool_pointer(itl.const_pool,section,rtti.pointer_contained_offset,root.slot);

    TypeTrieNode node;

    node.slot = slot;
    node.type = (Type*)pointer_type;

    push_var(root.nodes,node);

    // return newly inserted node
    return root.nodes[count(root.nodes) - 1];    
}

TypeTrieNode& make_rtti_builtin(Interloper& itl, const Type* type)
{
    auto& rtti = itl.rtti_cache;

    const builtin_type builtin = cast_builtin(type);
    auto& root = rtti.builtin_type_cache[u32(builtin)];

    // base type is not yet in the pool
    if(root.slot.handle == INVALID_HANDLE)
    {
        // allocate a slot in the pool for us
        const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.builtin_type_struct_size);
        auto& section = pool_section_from_slot(itl.const_pool,slot);

        // write in base type struct
        write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::builtin_t));
        write_const_pool(itl.const_pool,section,rtti.builtin_type_offset,u32(builtin));


        root.slot = slot;

        rtti.type_data_size += section.size;
    }

    return root;    
}

    // // Fill in enum
    // const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.enum_type_struct_size);
    // auto& section = pool_section_from_slot(itl.const_pool,slot);



    // const auto pointer_slot = push_const_pool_vla(itl.const_pool,member_data,string_node->string.size);

TypeTrieNode& make_rtti_enum(Interloper& itl, EnumType* enum_type)
{
    auto& rtti = itl.rtti_cache;

    TypeTrieNode* enum_node = lookup(rtti.enum_type_cache,enum_type->enum_idx);
    if(enum_node)
    {
        return *enum_node;
    }

    auto enumeration = enum_from_type(itl.enum_table,enum_type);
    const u32 member_size = enumeration.member_map.size;

    // Write in members
    const auto member_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.enum_member_size * member_size);
    auto& member_section = pool_section_from_slot(itl.const_pool,member_slot);

    u32 member_idx = 0;
    for(const auto& member_node : enumeration.member_map)
    {
        const auto& member = member_node.v;
        const auto name_slot = push_const_pool_string(itl.const_pool,member.name);

        const u32 base = member_idx * rtti.enum_member_size;

        write_const_pool_vla(itl.const_pool,member_section,base + rtti.enum_member_name_offset,name_slot,member.name.size);
        write_const_pool(itl.const_pool,member_section,base + rtti.enum_member_value_offset,member.value);
        member_idx += 1;
    }

    // Write in enum struct
    const auto enum_struct_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.enum_struct_struct_size);
    auto& enum_struct_section = pool_section_from_slot(itl.const_pool,enum_struct_slot);

    write_const_pool_vla(itl.const_pool,enum_struct_section,rtti.enum_struct_member_offset,member_slot,member_size);
    const auto name_slot = push_const_pool_string(itl.const_pool,enumeration.name);
    write_const_pool_vla(itl.const_pool,enum_struct_section,rtti.enum_struct_name_offset,name_slot,enumeration.name.size);

    // Write in type
    const auto enum_type_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.enum_type_struct_size);
    auto& enum_type_section = pool_section_from_slot(itl.const_pool,enum_type_slot);
    write_const_pool(itl.const_pool,enum_type_section,rtti.type_class_offset,u32(rtti_type_class::enum_t));
    write_const_pool_pointer(itl.const_pool,enum_type_section,rtti.enum_type_enumeration_offset,enum_struct_slot);

    TypeTrieNode node;
    node.slot = enum_type_slot;
    node.type = (Type*)enum_type;

    return *add(rtti.enum_type_cache,enum_type->enum_idx,node);
}

TypeTrieNode& make_rtti_internal(Interloper& itl, const Type* type)
{
    switch(type->kind)
    {
        case type_class::array_t:
        {
            return make_rtti_array(itl,(ArrayType*)type);
        }

        case type_class::pointer_t:
        {
            return make_rtti_pointer(itl,(PointerType*)type);
        }

        case type_class::builtin_t:
        {
            return make_rtti_builtin(itl,type);
        }

        case type_class::enum_t:
        {
            return make_rtti_enum(itl,(EnumType*)type);
        }


        default:
        {
            compile_panic(itl,itl_error::invalid_expr, "Any type info not handled for %t",type);
        }
    }
}


PoolSlot make_rtti(Interloper& itl, const Type* type)
{
    const auto node = make_rtti_internal(itl,type);
    return node.slot;
}

RegSlot acquire_rtti(Interloper& itl, Function& func, const Type* type)
{
    const PoolSlot pool_slot = make_rtti(itl,type);

    return pool_addr_res(itl,func,pool_slot,0);
}