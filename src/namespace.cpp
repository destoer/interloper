DefNode* alloc_new_scope()
{
    DefNode* new_scope = (DefNode*)malloc(sizeof(DefNode));
    assert(new_scope);

    *new_scope = {};
    new_scope->table = make_table<String,DefInfo>();
    new_scope->name_space = "0__anon__0";
    new_scope->full_name = "0__anon__0";

    return new_scope;
}

DefNode* new_anon_scope(DefNode* root)
{
    auto new_scope = alloc_new_scope();

    // Insert the new scope
    new_scope->parent = root;
    push_var(root->nodes,new_scope);
    return new_scope;
}

DefInfo* lookup_definition(DefNode* name_space, const String& name)
{
    DefNode* cur_scope = name_space;

    while(cur_scope)
    {
        DefInfo* def_info = lookup(cur_scope->table,name);

        if(def_info)
        {
            return def_info;
        }
   
        cur_scope = cur_scope->parent;
    }

    return nullptr;    
}

DefInfo* lookup_definition_scoped(DefNode* name_space, const String& name)
{
   return lookup(name_space->table,name);
}

DefInfo* lookup_typed_definition_scoped(DefNode* name_space, const String& name, definition_type type)
{
    DefInfo* definition = lookup_definition_scoped(name_space,name);

    if(definition && definition->type == type)
    {
        return definition;
    }

    return nullptr; 
}

DefInfo* lookup_typed_definition(DefNode* name_space, const String& name, definition_type type)
{
    DefInfo* definition = lookup_definition(name_space,name);

    if(definition && definition->type == type)
    {
        return definition;
    }

    return nullptr;
}


DefNode* new_named_scope(ArenaAllocator& string_allocator,DefNode* root, const Array<String>& name)
{
    // TODO: this is wrong but we don't have nested scopes atm so we dont care
    assert(count(name) == 1);

    DefNode* new_scope = new_anon_scope(root);
    new_scope->name_space = copy_string(string_allocator,name[0]);
    new_scope->full_name = new_scope->name_space;

    return new_scope;
}


DefNode* scan_namespace(const DefNode* root, const Array<String>& name_space)
{
    u32 name_idx = 0;
    DefNode *result = nullptr;

    while(name_idx != count(name_space))
    {
        bool found = false;

        for(size_t i = 0; i < count(root->nodes); i++)
        {
            const auto node = root->nodes[i];
            if(node->name_space == name_space[name_idx])
            {
                found = true;
                name_idx++;
                root = node;

                if(name_idx == count(name_space))
                {
                    result = node;
                }
            }
        }

        if(!found)
        {
            return nullptr;
        }
    }
    
    return result;
}

DefNode* find_name_space(Interloper& itl, const String& name)
{
    for(size_t i = 0; i < count(itl.def_root->nodes); i++)
    {
        if(itl.def_root->nodes[i]->name_space == name)
        {
            return itl.def_root->nodes[i];
        }
    }

    return nullptr;
}

void print_depth(int depth);

void print_namespace_tree(DefNode* root, u32 depth)
{
    print_depth(depth);

    if(!root)
    {
        puts("namespace: NULL");
        return;
    }

    printf("namespace %s: \n",root->name_space.buf);

    auto table = root->table;

    for(u32 i = 0; i < count(table.buf); i++)
    {
        auto& bucket = table.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            print_depth(depth + 1);
            printf("def %s %s\n",bucket[j].key.buf,definition_type_name(&bucket[j].v));
        }
    }    


    for(size_t i = 0; i < count(root->nodes); i++)
    {
        print_namespace_tree(root->nodes[i],depth + 2);
    }
}

TypeDecl* lookup_incomplete_decl_scoped(DefNode* name_space, const String& name)
{
    DefInfo* info = lookup_typed_definition_scoped(name_space,name,definition_type::type);

    if(info)
    {
        return info->type_decl;
    }

    return nullptr;
}

TypeDecl* lookup_incomplete_decl(Interloper& itl, const String& name)
{
    DefInfo* info = lookup_typed_definition(itl.symbol_table.scope,name,definition_type::type);

    if(info)
    {
        return info->type_decl;
    }

    return nullptr;
}

TypeDecl* lookup_complete_decl(Interloper& itl, const String& name)
{
    TypeDecl* type_decl = lookup_incomplete_decl(itl,name);

    if (type_decl->state != type_def_state::checked)
    {
        return nullptr;
    }

    return type_decl;
}

TypeDef* lookup_type_def(Interloper& itl, const String& name)
{
    TypeDecl* type_decl = lookup_incomplete_decl(itl,name);

    if(!type_decl || !(type_decl->flags & TYPE_DECL_DEF_FLAG))
    {
        return nullptr;
    }

    return (TypeDef*)type_decl;
}


// NOTE: this gets a function ONLY in the requested scope
FunctionDef* lookup_func_def_scope(Interloper& itl, DefNode* name_space, const String& name)
{
    auto func_def = lookup_typed_definition_scoped(name_space,name,definition_type::function);

    if(func_def)
    {
        return &itl.func_table.table[func_def->handle];
    }

    //printf("lookup scoped: %s\n",full_name.buf);
    return nullptr;
}

// get a function only in the global scope
FunctionDef* lookup_func_def_global(Interloper& itl, const String& name)
{
    //printf("lookup global: %s\n",name.buf);
    return lookup_func_def_scope(itl,itl.def_root, name);
}

// Search each scope from the bottom for a function
FunctionDef* lookup_func_def_default(Interloper& itl, const String& name)
{
    auto func_def = lookup_typed_definition(itl.symbol_table.scope,name,definition_type::function);

    if(func_def)
    {
        return &itl.func_table.table[func_def->handle];
    }

    // fail attempt to find globally
    return nullptr;
}
