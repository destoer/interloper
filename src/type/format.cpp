String fmt_index(Interloper& itl,u32 index)
{
    if(index == RUNTIME_SIZE)
    {
        return make_string(itl.string_allocator,"[]",2);
    }

    char buf[32];
    const u32 len = sprintf(buf,"[%d]",index);

    return make_string(itl.string_allocator,buf,len);
}

void push_const_name(Interloper& itl, StringBuffer& buffer, const Type* type, const String& string)
{
    if(is_const(type))
    {
        push_string(itl.string_allocator,buffer,string);
    }
}


String type_name(Interloper& itl,const Type *type);

void format_generic(Interloper& itl, StringBuffer& buffer, const GenericOverload& overload)
{
    push_char(itl.string_allocator,buffer,'<');
    char tmp_buffer[64] = {0};

    for(u32 g = 0; g < count(overload); g++)
    {
        const auto& generic = overload[g];

        if(generic.constraint == constraint_type::type)
        {
            u32 len = 0;

            const auto& known = generic.known.value;
            if(known.type == known_value_type::gpr_t)
            {
                len = sprintf(tmp_buffer,"%ld",known.gpr);
            }

            else
            {
                len = sprintf(tmp_buffer,"%lf",known.fpr);
            }

            push_string(itl.string_allocator,buffer,make_static_string(tmp_buffer,len));
        }

        else
        {
            push_string(itl.string_allocator,buffer,type_name(itl,generic.type));
        }

        if(g != count(overload) - 1)
        {
            push_char(itl.string_allocator,buffer,',');
        }
    }

    push_char(itl.string_allocator,buffer,'>');
}

String type_name(Interloper& itl,const Type *type)
{
    StringBuffer prefix;
    StringBuffer compound;
    StringBuffer generic;

    String plain;

    b32 done = false;

    while(!done)
    {
        switch(type->kind)
        {
            case type_class::pointer_t:
            {
                PointerType* pointer_type = (PointerType*)type;
                const bool nullable = pointer_type->pointer_kind == pointer_type::nullable;
                push_const_name(itl,compound,type," const");
                push_char(itl.string_allocator,compound,nullable? '?' : '@');

                type = pointer_type->contained_type;
                break;
            }

            case type_class::struct_t: 
            {
                push_const_name(itl,prefix,type,"const ");

                const auto structure =  struct_from_type(itl.struct_table,(StructType*)type);
                plain = structure.name;
                if(structure.overload)
                {
                    format_generic(itl,generic,structure.overload);
                }

                done = true;
                break;                
            }

            case type_class::enum_t: 
            {
                push_const_name(itl,prefix,type,"const ");

                const auto enumeration = enum_from_type(itl.enum_table,(EnumType*)type);
                plain = enumeration.name;  
                done = true;
                break;              
            }

            case type_class::array_t:
            {
                push_const_name(itl,compound,type," const");

                ArrayType* array_type = (ArrayType*)type;

                push_string(itl.string_allocator,compound,fmt_index(itl,array_type->size));
                type = array_type->contained_type;
                break;
            }

            case type_class::func_pointer_t:
            {
                FuncPointerType* func_type = (FuncPointerType*)type;
                const FuncSig& sig = func_type->sig;
                StringBuffer func_name;

                push_const_name(itl,prefix,type,"const ");

                push_string(itl.string_allocator,func_name,"func(");

                // print args
                // TODO: this should probably hide hidden args...
                for(u32 a = 0; a < count(sig.args); a++)
                {
                    if(a != 0)
                    {
                        push_char(itl.string_allocator,func_name,',');
                    }

                    const auto& sym = sym_from_slot(itl.symbol_table,sig.args[a]);

                    push_string(itl.string_allocator,func_name,sym.name);

                    push_string(itl.string_allocator,func_name," : ");

                    push_string(itl.string_allocator,func_name,type_name(itl,sym.type));
                }

                push_string(itl.string_allocator,func_name,") ");


                // single return
                if(count(sig.return_type) == 1)
                {
                    push_string(itl.string_allocator,func_name,type_name(itl,sig.return_type[0]));
                }

                // tuple return
                else
                {
                    assert(false);
                }

                // null term the string
                push_char(itl.string_allocator,func_name,'\0');

                plain = make_string(func_name);
                done = true;
                break;
            }

            // builtin
            case type_class::builtin_t:
            {
                push_const_name(itl,prefix,type,"const ");

                plain = builtin_type_name(cast_builtin(type));
                done = true;
                break;
            }

            case type_class::tuple_t:
            {
                assert(false);
                break;
            }
        }

    }

    push_string(itl.string_allocator,prefix,plain);
    if(generic)
    {
        push_char(itl.string_allocator,generic,'\0');
        push_string(itl.string_allocator,prefix,make_string(generic));
    }

    // null term both strings
    push_char(itl.string_allocator,prefix,'\0');
    push_char(itl.string_allocator,compound,'\0');


    // produce the final string!
    String name = cat_string(itl.string_allocator,make_string(prefix),make_string(compound));

    return name;
}