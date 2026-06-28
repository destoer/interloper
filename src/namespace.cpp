NameSpace* alloc_new_scope(ArenaAllocator& arena)
{
    NameSpace* new_scope = (NameSpace*)allocate(arena,sizeof(NameSpace));
    assert(new_scope);

    *new_scope = {};
    new_scope->table = make_table<String,DefInfo>();
    new_scope->name_space = "0__anon__0";
    new_scope->full_name = "0__anon__0";

    return new_scope;
}

NameSpace* new_anon_scope(ArenaAllocator& arena,NameSpace* root)
{
    auto new_scope = alloc_new_scope(arena);

    // Insert the new scope
    new_scope->parent = root;
    push_var(root->nodes,new_scope);
    return new_scope;
}

DefInfo* lookup_definition(NameSpace* name_space, const String& name)
{
    NameSpace* cur_scope = name_space;

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

DefInfo* lookup_definition_scoped(NameSpace* name_space, const String& name)
{
   return lookup(name_space->table,name);
}

DefInfo* lookup_typed_definition_scoped(NameSpace* name_space, const String& name, definition_type type)
{
    DefInfo* definition = lookup_definition_scoped(name_space,name);

    if(definition && definition->type == type)
    {
        return definition;
    }

    return nullptr; 
}

DefInfo* lookup_typed_definition(NameSpace* name_space, const String& name, definition_type type)
{
    DefInfo* definition = lookup_definition(name_space,name);

    if(definition && definition->type == type)
    {
        return definition;
    }

    return nullptr;
}


NameSpace* new_named_scope(ArenaAllocator& arena,ArenaAllocator& string_allocator,NameSpace* root, const String& name)
{
    NameSpace* new_scope = new_anon_scope(arena,root);
    new_scope->name_space = copy_string(string_allocator,name);
    new_scope->full_name = new_scope->name_space;

    return new_scope;
}

NameSpace* scan_namespace(Parser& parser, const Array<String>& name_space)
{
    NameSpace* root = parser.ctx.global_namespace;

    u32 name_idx = 0;

    while(name_idx != count(name_space))
    {
        bool found = false;

        for(NameSpace* node : root->nodes)
        {
            if(node->name_space == name_space[name_idx])
            {
                root = node;

                found = true;
                name_idx++;

                if(name_idx == count(name_space))
                {
                    return node;
                }
            }
        }

        if(!found)
        {
            break;
        }
    }

    // Namespace does not allready exist create it!
    const ConstSpan<String> new_space = make_const_span(name_space,name_idx,count(name_space) - name_idx);

    for(const String& name : new_space)
    {
        root = new_named_scope(*parser.alloc->namespace_allocator,*parser.alloc->global_string_allocator,root,name);
    }

    return root;
}

NameSpace* find_name_space(Interloper& itl, const String& name)
{
    for(auto& node : itl.global_namespace->nodes)
    {
        if(node->name_space == name)
        {
            return node;
        }
    }

    return nullptr;
}

void print_depth(int depth);

void print_namespace_tree(NameSpace* root, u32 depth)
{
    print_depth(depth);

    if(!root)
    {
        puts("namespace: NULL");
        return;
    }

    printf("namespace %s: \n",root->name_space.buf);

    auto table = root->table;

    for(auto& table_node : table)
    {
        print_depth(depth + 1);
        printf("def %s %s\n",table_node.key.buf,definition_type_name(&table_node.v));
    }    

    for(NameSpace* node : root->nodes)
    {
        print_namespace_tree(node,depth + 2);
    }
}

TypeDecl* lookup_incomplete_decl_internal_scoped(NameSpace* name_space, const String& name)
{
    DefInfo* info = lookup_typed_definition_scoped(name_space,name,definition_type::type);

    if(info)
    {
        return info->type_decl;
    }

    return nullptr;
}

TypeDecl* lookup_incomplete_decl_internal(Interloper& itl, const TypeLookupInfo& info)
{
    DefInfo* def = nullptr;
    
    if(!info.name_space)
    {
        def = lookup_typed_definition(itl.symbol_table.ctx->name_space,info.name,definition_type::type);

        if(def)
        {
            return def->type_decl;
        }

        return nullptr;
    }

    return lookup_incomplete_decl_internal_scoped(info.name_space,info.name);
}

Result<TypeDecl*,itl_error> lookup_incomplete_decl(Interloper& itl, const TypeLookupInfo& info)
{
    const auto decl = lookup_incomplete_decl_internal(itl,info);

    // Check the type decl event exists?
    if(!decl)
    {
        return compile_error(itl,itl_error::undeclared,"Type %n%s is not declared",info.name_space,info.name);
    }

    // Check we got back the kind of type we requested
    if(info.kind == type_lookup_kind::struct_t && decl->kind != type_kind::struct_t)
    {
        return compile_error(itl,itl_error::struct_error,"Type %n%s is not a struct",info.name_space,info.name);
    }

    else if(info.kind == type_lookup_kind::enum_t && decl->kind != type_kind::enum_t)
    {
        return compile_error(itl,itl_error::enum_type_error,"Type %n%s is not an enum",info.name_space,info.name);
    }

    // Check any generics are used correctly
    switch(decl->kind)
    {
        case type_kind::builtin: break;
        
        case type_kind::alias_t:
        {
            if(info.generic_args)
            {
                return compile_error(itl,itl_error::generic,"Cannot use type alias as a generic",
                    info.name_space,info.name);          
            }

            break;
        }

        case type_kind::enum_t: 
        case type_kind::struct_t:
        {
            TypeDef* def = (TypeDef*)decl;

            // Check validity on templates
            if(def->generic_base && !info.generic_args)
            {
                assert(false);
                return compile_error(itl,itl_error::generic,"Generic type %n%s instantiated without args",
                    info.name_space,info.name);
            }

            if(!def->generic_base && info.generic_args)
            {
                return compile_error(itl,itl_error::generic,"Generic instantiation on plain type %n%s",
                    info.name_space,info.name);
            }

            break;
        }
    }

    return decl;
}

// NOTE: this gets a function ONLY in the requested scope
FunctionDef* lookup_func_def_scope(Interloper& itl, NameSpace* name_space, const String& name)
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
    return lookup_func_def_scope(itl,itl.global_namespace, name);
}

// Search each scope from the bottom for a function
FunctionDef* lookup_func_def_default(Interloper& itl, const String& name)
{
    auto func_def = lookup_typed_definition(itl.symbol_table.ctx->name_space,name,definition_type::function);

    if(func_def)
    {
        return &itl.func_table.table[func_def->handle];
    }

    // fail attempt to find globally
    return nullptr;
}

FunctionDef* lookup_func_def(Interloper& itl, NameSpace* name_space, const String& name)
{
    return name_space? lookup_func_def_scope(itl,name_space,name) : lookup_func_def_default(itl,name);
}

void destroy_namespace_node(NameSpace* root)
{
    if(!root)
    {
        return;
    }

    for(auto& node : root->nodes)
    {
        destroy_namespace_node(node);
    }

    destroy_table(root->table);
    destroy_arr(root->nodes);
}

void destroy_namespace_tree(Interloper& itl)
{
    destroy_namespace_node(itl.global_namespace);
    destroy_allocator(itl.namespace_allocator);
    itl.global_namespace = nullptr;
}

void setup_namespace(Interloper& itl)
{
    // Setup the global scope
    itl.global_namespace = alloc_new_scope(itl.namespace_allocator);
    itl.global_namespace->name_space = "global";
    itl.global_namespace->full_name = itl.global_namespace->name_space;
    itl.ctx.name_space = itl.global_namespace;
}