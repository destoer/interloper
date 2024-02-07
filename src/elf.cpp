// https://www.sco.com/developers/gabi/latest/contents.html

#include <elf.h>

struct ElfFunc
{
    u32 offset;
    u32 size;
};


struct ElfAllocSection
{
    u32 section_idx = 0;
    Elf64_Shdr header;
    
    u32 program_idx;
    Elf64_Phdr program_header;   

    u32 symbol_idx;

    // NOTE: code is owned externally in the 
    // for text AsmEmitter struct
    // for const dat ain the const pool
};

struct ElfGlobal
{
    u32 program_idx;
    Elf64_Phdr program_header;   
};

struct ElfStringTable
{
    u32 section_idx = 0;
    Elf64_Shdr header;

    Array<u8> buffer;
    u32 cur_string_idx;
};

struct ElfSymbolTable
{
    u32 section_idx = 0;
    Elf64_Shdr header;

    Array<Elf64_Sym> table;
};

// NOTE: atm we only care about supporting 64 bit
struct Elf
{
    // output file
    String name;

    // final buffer
    Array<u8> buffer;

    // TODO: these shouldn't be hard coded
    arch_target arch = arch_target::x86_64_t;
    os_target os = os_target::linux_t;

    // basic elf header
    Elf64_Ehdr header;

    ElfStringTable section_string_table;
    ElfStringTable symtab_string_table;

    ElfAllocSection text_section;
    ElfAllocSection const_data;
    ElfGlobal global;

    ElfSymbolTable symbol_table;

    // we have a null section
    u16 section_count = 1;
    u16 program_count = 0;
};


void setup_header(Elf& elf)
{
    auto& header = elf.header;

    memset(&header,0,sizeof(header));

    // executable file
    header.e_type = ET_EXEC;

    switch(elf.arch)
    {
        case arch_target::x86_64_t:
        { 
            header.e_machine = EM_X86_64;
            break;
        }
    }

    header.e_version = EV_CURRENT;
    header.e_ehsize = sizeof(header);

    // for now we only know the section header sizes
    header.e_phentsize = sizeof(Elf64_Phdr);
    header.e_shentsize = sizeof(Elf64_Shdr);

    // setup ident
    header.e_ident[EI_MAG0] = 0x7f;
    header.e_ident[EI_MAG1] = 'E';
    header.e_ident[EI_MAG2] = 'L';
    header.e_ident[EI_MAG3] = 'F';

    switch(elf.arch)
    {
        case arch_target::x86_64_t:
        { 
            // 64 bit little endian
            header.e_ident[EI_CLASS] = ELFCLASS64;
            header.e_ident[EI_DATA] = ELFDATA2LSB;

            header.e_ident[EI_ABIVERSION] = 0;
            break;
        }
    }

    header.e_ident[EI_VERSION] = EV_CURRENT;

    switch(elf.os)
    {
        case os_target::linux_t:
        {
            header.e_ident[EI_OSABI] = ELFOSABI_LINUX;
            break;
        }
    }

    
}


void align_elf(Elf& elf, u32 align)
{
    const u64 cur_offset = elf.buffer.size;
    resize(elf.buffer,align_val(cur_offset,align));  
}

u32 push_string(ElfStringTable& string_table, const String& name)
{
    // add a string and null term it
    const u32 offset = push_mem(string_table.buffer,name);
    push_var(string_table.buffer,'\0');
    
    string_table.cur_string_idx += 1;

    return offset;   
}

u32 add_section(Elf& elf, const String& name, Elf64_Shdr& section)
{
    // first name is allways empty
    section.sh_name = push_string(elf.section_string_table,name);
    return elf.section_count++;
}

u32 add_program_header(Elf& elf)
{
    return elf.program_count++;
}

u32 add_symbol(Elf& elf,const String& name,u32 section_idx,u32 st_info, u32 offset, u32 size)
{
    auto& symbol_table = elf.symbol_table;
    auto& symtab_string_table = elf.symtab_string_table;

    Elf64_Sym sym;
    memset(&sym,0,sizeof(sym));

    // add symbol name
    sym.st_name = push_string(symtab_string_table,name);

    // write the offset into value for now
    // we will rewrite it later
    sym.st_value = offset;

    // set as function
    sym.st_info = st_info;

    // setup size
    sym.st_size = size;

    // what seciton is this sym apart of
    sym.st_shndx = section_idx;

    const u32 idx = count(symbol_table.table);

    // add symbol
    push_var(symbol_table.table,sym);

    return idx;
}


u32 add_section_symbol(Elf& elf,const String& name, u32 section_idx)
{
    // write symbol and offset later
    return add_symbol(elf,name,section_idx,ELF64_ST_INFO(STB_GLOBAL,STT_SECTION),0,0);
}

void add_func_symbol(Elf& elf,const AsmFunc& func)
{
    add_symbol(elf,func.ir_func->name,elf.text_section.section_idx,ELF64_ST_INFO(STB_GLOBAL,STT_FUNC),func.offset,func.size);
}

void setup_alloc_section(Elf& elf,ElfAllocSection& section, const String& name,const String& sym_name, u32 shf_flags, u32 program_flags)
{
    auto& section_header = section.header;

    memset(&section_header,0,sizeof(section_header));

    section.section_idx = add_section(elf,name,section_header);

    section_header.sh_type = SHT_PROGBITS;
    section_header.sh_flags = shf_flags;
    section_header.sh_addralign = 16;

    // setup program header
    auto& program_header = section.program_header;

    memset(&program_header,0,sizeof(program_header));

    // load, align to 4096 byte page, read | execute
    program_header.p_type = PT_LOAD;
    program_header.p_align = 4096;
    program_header.p_flags = program_flags;

    section.program_idx = add_program_header(elf);   

    // add symbol
    section.symbol_idx = add_section_symbol(elf,sym_name,section.section_idx);
}




void setup_text(Elf& elf)
{
    setup_alloc_section(elf,elf.text_section,".text","_text",SHF_EXECINSTR | SHF_ALLOC,PF_X | PF_R);
}

void setup_const(Elf& elf)
{
    setup_alloc_section(elf,elf.const_data,".rodata","_rodata",SHF_ALLOC,PF_R);
}

void setup_global(Interloper& itl,Elf& elf)
{
    auto& program_header = elf.global.program_header;

    memset(&program_header,0,sizeof(program_header));

    // load, align to 4096 byte page, read | execute
    program_header.p_type = PT_LOAD;
    program_header.p_align = 4096;
    program_header.p_flags = PF_R | PF_W;
    
    program_header.p_memsz = itl.global_alloc.size;

    elf.global.program_idx = add_program_header(elf);   
}

void setup_string(Elf& elf, ElfStringTable& string_table, const String& name)
{
    push_var(string_table.buffer,'\0');

    memset(&string_table.header,0,sizeof(string_table.header));
    
    string_table.header.sh_type = SHT_STRTAB;
    string_table.header.sh_flags = SHF_STRINGS;

    string_table.header.sh_addralign = 1;

    string_table.section_idx = add_section(elf,name,string_table.header);
}

void setup_symtab(Elf& elf)
{
    auto& symbol_table = elf.symbol_table;
    auto& header = symbol_table.header;

    memset(&header,0,sizeof(header));

    symbol_table.section_idx = add_section(elf,".symtab",header);

    header.sh_type = SHT_SYMTAB;

    header.sh_entsize = sizeof(Elf64_Sym);

    header.sh_addralign = 8;

    // link to the sym table we are going to use
    header.sh_link = elf.symtab_string_table.section_idx;
}

void add_null_sym(Elf& elf)
{
    auto& symbol_table = elf.symbol_table;

    // insert the null sym entry
    Elf64_Sym null_sym;
    memset(&null_sym,0,sizeof(null_sym));

    push_var(symbol_table.table,null_sym);
}


Elf make_elf(Interloper& itl,const String& filename)
{
    Elf elf;
    elf.name = filename;

    // setup initial header section
    setup_header(elf);

    // setup the section string table
    setup_string(elf,elf.section_string_table,".shstrtab");

    setup_string(elf,elf.symtab_string_table,".strtab");

    // insert the null sym entry 
    // NOTE: we want this done before we dump any symbols
    add_null_sym(elf);

    // setup text section
    setup_text(elf);

    setup_const(elf);

    setup_global(itl,elf);

    setup_symtab(elf);

    return elf;
}


void write_program_header(Elf& elf,u32 header_idx, u32 offset, u64 v)
{
    const u32 base_addr = elf.header.e_phoff + (header_idx * elf.header.e_phentsize);

    write_mem(elf.buffer,base_addr + offset,v);    
}

void write_section_header(Elf& elf,u32 header_idx, u32 offset, u64 v)
{
    const u32 base_addr = elf.header.e_shoff + (header_idx * elf.header.e_shentsize);

    write_mem(elf.buffer,base_addr + offset,v);
}

void finalise_section_headers(Elf& elf)
{
    // align section headers
    align_elf(elf,sizeof(Elf64_Shdr));

    // add null section
    Elf64_Shdr null_section;
    memset(&null_section,0,sizeof(null_section));
    push_mem(elf.buffer,&null_section,sizeof(null_section));


    // add string section header
    auto& section_string_table = elf.section_string_table;
    push_mem(elf.buffer,&section_string_table.header,sizeof(section_string_table.header));

    // symbol string table header
    auto& symtab_string_table = elf.symtab_string_table;
    push_mem(elf.buffer,&symtab_string_table.header,sizeof(symtab_string_table.header));


    // add text section header
    auto& text_section = elf.text_section;
    push_mem(elf.buffer,&text_section.header,sizeof(text_section.header));

    // add const section header
    auto& const_data = elf.const_data;
    push_mem(elf.buffer,&const_data.header,sizeof(const_data.header));

    // add symtab header
    auto& symbol_table = elf.symbol_table;

    // + 1 of final local symbol?
    elf.symbol_table.header.sh_info = count(elf.symbol_table.table) + 1;
    push_mem(elf.buffer,&symbol_table.header,sizeof(symbol_table.header));


    // setup location of program headers
    elf.header.e_phoff = elf.buffer.size;
}

static constexpr u32 BASE_ADDR = 0x400000;

template<typename T>
u32 push_section_data(Elf& elf, u32 section_idx, const Array<T>& buffer, b32 write_addr = false)
{
    // write offset
    const u64 buffer_offset = push_mem(elf.buffer,buffer);
    write_section_header(elf,section_idx,offsetof(Elf64_Shdr,sh_offset),buffer_offset); 

    // addr
    if(write_addr)
    {
        write_section_header(elf,section_idx,offsetof(Elf64_Shdr,sh_addr),BASE_ADDR + buffer_offset);
    }

    // size
    write_section_header(elf,section_idx,offsetof(Elf64_Shdr,sh_size),buffer.size);

    return buffer_offset;  
}

void finalise_program_vaddr(Elf& elf, u32 program_idx, u64 vaddr)
{
    write_program_header(elf,program_idx,offsetof(Elf64_Phdr,p_vaddr),vaddr);
    write_program_header(elf,program_idx,offsetof(Elf64_Phdr,p_paddr),vaddr);
}

std::pair<u64,u64> finalise_alloc_section(Elf& elf, ElfAllocSection& section,Array<u8> buffer)
{
    // make sure page size aligns
    align_elf(elf,section.program_header.p_align);

    const u64 offset = push_section_data(elf,section.section_idx,buffer,true);
    
    // finish up the program header
    write_program_header(elf,section.program_idx,offsetof(Elf64_Phdr,p_offset),offset);

    // start at 4MB
    const u64 vaddr = offset + BASE_ADDR;

    finalise_program_vaddr(elf,section.program_idx,vaddr);

    // write size
    write_program_header(elf,section.program_idx,offsetof(Elf64_Phdr,p_filesz),buffer.size);
    write_program_header(elf,section.program_idx,offsetof(Elf64_Phdr,p_memsz),buffer.size);


    return std::pair{offset,vaddr};
}

void write_symtab(Interloper& itl, Elf& elf, u64 text_vaddr, u64 const_vaddr)
{
    // add symtab
    auto& symbol_table = elf.symbol_table;

    // setup the offsets of the section symbools
    symbol_table.table[elf.text_section.symbol_idx].st_value = text_vaddr;
    symbol_table.table[elf.const_data.symbol_idx].st_value = const_vaddr;

    // rewrite every symbol know we know start of text section
    for(u32 s = 1; s < count(symbol_table.table); s++)
    {
        auto& sym = symbol_table.table[s];

        if(ELF64_ST_TYPE(sym.st_info) == STT_FUNC)
        {
            const u32 offset = sym.st_value;

            const u32 func_addr = text_vaddr + offset;

            sym.st_value = func_addr;
        }
    }

    // we can also write in all the correct label pos
    finalise_labels(itl,text_vaddr);

    // write entry point (i.e find start)

    const auto& start = lookup_internal_function(itl,String("start"));
    auto& start_label = label_from_slot(itl.symbol_table.label_lookup,start.label_slot);

    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_entry),start_label.offset);

    // align the symbol table data
    align_elf(elf,symbol_table.header.sh_addralign);

    push_section_data(elf,symbol_table.section_idx,symbol_table.table);
}

void finalise_const_pool(Interloper& itl,Elf& elf,u32 const_offset, u64 const_vaddr)
{
    // get raw pool data to rewrite its contents
    auto& const_pool = itl.const_pool;

    // perform pool rewriting

    // rewrite all labels
    for(u32 l = 0; l < count(const_pool.label); l++)
    {
        const auto pool_label = const_pool.label[l];
        const auto label = label_from_slot(itl.symbol_table.label_lookup,pool_label.label_slot);

        write_mem<u64>(elf.buffer,const_offset + pool_label.pool_offset,label.offset);
    }

    // rewrite all pointers
    for(u32 p = 0; p < count(const_pool.pool_pointer); p++)
    {
        const auto& pool_pointer = const_pool.pool_pointer[p];
        const auto& data_pointer = pool_pointer.pointer;
        auto& section = pool_section_from_slot(itl.const_pool,data_pointer.slot);

        const u32 addr = const_offset + pool_pointer.pool_offset;

        write_mem<u64>(elf.buffer,addr,(section.offset + data_pointer.offset) + const_vaddr);
    }   
}

void finalise_section_data(Interloper& itl,Elf& elf)
{
    // add string section data
    auto& section_string_table = elf.section_string_table;
    push_section_data(elf,section_string_table.section_idx,section_string_table.buffer);

    // add symtab string
    auto& symtab_string_table = elf.symtab_string_table;
    push_section_data(elf,symtab_string_table.section_idx,symtab_string_table.buffer);

    // add text section data
    const auto [text_offset,text_vaddr] = finalise_alloc_section(elf,elf.text_section,itl.asm_emitter.buffer);
    itl.asm_emitter.base_offset = text_offset;

    // add const section data
    const auto [const_offset, const_vaddr] = finalise_alloc_section(elf,elf.const_data,itl.const_pool.buf);
    itl.const_pool.base_vaddr = const_vaddr;

    // write in symtab
    write_symtab(itl,elf,text_vaddr,const_vaddr);

    finalise_const_pool(itl,elf,const_offset,const_vaddr);

    // write in global vaddr
    // TODO: if we want more sections we probably want something that keeps track of cur alloc addr
    auto& global = elf.global;
    itl.global_alloc.base_vaddr = align_val(const_vaddr + itl.const_pool.buf.size,global.program_header.p_align);
    finalise_program_vaddr(elf,global.program_idx,itl.global_alloc.base_vaddr);
}

void finalise_elf_header(Elf& elf)
{
    // update final seciton count
    elf.header.e_shnum = elf.section_count;

    // section headers immediatly follow elf header
    elf.header.e_shoff = sizeof(elf.header);

    // give section name offset
    elf.header.e_shstrndx = elf.section_string_table.section_idx;   

    // actually dump the data
    push_mem(elf.buffer,&elf.header,sizeof(elf.header));
}

void finalise_program_headers(Elf& elf)
{
    // align program headers
    align_elf(elf,sizeof(u64));

    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_phoff),elf.header.e_phoff);
    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_phnum),elf.program_count);
    
    // start writing out the headers

    // text section
    auto &text_section = elf.text_section;
    push_mem(elf.buffer,&text_section.program_header,sizeof(text_section.program_header));

    // const data
    auto &const_data = elf.const_data;
    push_mem(elf.buffer,&const_data.program_header,sizeof(const_data.program_header));

    // const data
    auto &global = elf.global;
    push_mem(elf.buffer,&global.program_header,sizeof(global.program_header));
}

void finalise_elf(Interloper& itl,Elf& elf)
{
    finalise_elf_header(elf);

    // NOTE: we must add the section headers
    // into the buffer in the same order they were created
    finalise_section_headers(elf);

    finalise_program_headers(elf);

    finalise_section_data(itl,elf);

    printf("program size: %d\n",elf.buffer.size);
}

void destroy_elf(Elf& elf)
{
    destroy_arr(elf.section_string_table.buffer);

    destroy_arr(elf.symtab_string_table.buffer);

    destroy_arr(elf.symbol_table.table);

    destroy_arr(elf.buffer);
}

void write_elf(Elf& elf)
{
    std::ofstream fp(elf.name.buf,std::ios::out | std::ios::binary);

    if(!fp)
    {
        crash_and_burn("could not open file %s\n",elf.name.buf);
    }

    file_write_arr(fp,elf.buffer.data,elf.buffer.size);

    // TODO: set the executable perms on the file

    fp.close();
}

void rewrite_rel_label(Interloper& itl,Elf& elf,const LinkOpcode& link, LabelSlot slot)
{
    auto& asm_emitter = itl.asm_emitter;
    const u32 text_offset = itl.asm_emitter.base_offset;

    // get the lable and write in the relative addr
    const auto label = label_from_slot(itl.symbol_table.label_lookup,slot);

    const u32 label_addr = label.offset;
    const u32 instr_addr = (link.offset + asm_emitter.base_vaddr);

    const s32 rel_addr = (label_addr  - instr_addr) - 4;

    write_mem(elf.buffer,text_offset + link.offset,rel_addr);
}

void rewrite_rel_const_pool(Interloper& itl,Elf& elf,const LinkOpcode& link, PoolSlot slot)
{
    auto& asm_emitter = itl.asm_emitter;
    auto& const_pool = itl.const_pool;
    const u32 text_offset = itl.asm_emitter.base_offset;

    // get pool section and write in relative addr
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    const u32 const_addr = (const_pool.base_vaddr + section.offset);
    const u32 instr_addr = (asm_emitter.base_vaddr + link.offset);

    const s32 rel_addr = (const_addr - instr_addr) - 4;

    write_mem(elf.buffer,text_offset + link.offset,rel_addr);
}


void rewrite_rel_load_store(Interloper& itl,Elf& elf,const LinkOpcode& link)
{
    auto& asm_emitter = itl.asm_emitter;
    auto& const_pool = itl.const_pool;
    auto& global = itl.global_alloc;

    const auto opcode = link.opcode;

    const s64 section_offset = opcode.v[2];
    const SymSlot spec = sym_from_idx(opcode.v[1]);

    const u32 text_offset = itl.asm_emitter.base_offset;

    switch(spec.handle)
    {
        case CONST_IR:
        {
            const u32 const_addr = (const_pool.base_vaddr + section_offset);
            const u32 instr_addr = (asm_emitter.base_vaddr + link.offset);

            const s32 rel_addr = (const_addr - instr_addr) - 4;

            write_mem(elf.buffer,text_offset + link.offset,rel_addr);
            break;
        }

        case GP_IR:
        {
            const u32 global_addr = (global.base_vaddr + section_offset);
            const u32 instr_addr = (asm_emitter.base_vaddr + link.offset);

            const s32 rel_addr = (global_addr - instr_addr) - 4;

            write_mem(elf.buffer,text_offset + link.offset,rel_addr);
            break;
        }

        default:
        {
            unimplemented("[ELF X86 LINK]: unknown load_store handle %s",spec_reg_name(spec));
            break;
        }
    }
}


void link_opcodes(Interloper& itl, Elf& elf)
{
    auto& asm_emitter = itl.asm_emitter;

    for(u32 l = 0; l < count(asm_emitter.link); l++)
    {
        const auto link = asm_emitter.link[l];
        const auto opcode = link.opcode;

        switch(opcode.op)
        {
            case op_type::call:
            {
                const LabelSlot slot = label_from_idx(opcode.v[0]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            case op_type::load_func_addr:
            {
                const LabelSlot slot = label_from_idx(opcode.v[1]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            case op_type::je:
            {
                const LabelSlot slot = label_from_idx(opcode.v[0]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            case op_type::jne:
            {
                const LabelSlot slot = label_from_idx(opcode.v[0]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            case op_type::b:
            {
                const LabelSlot slot = label_from_idx(opcode.v[0]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            case op_type::pool_addr:
            {
                const PoolSlot slot = pool_slot_from_idx(opcode.v[1]);
                rewrite_rel_const_pool(itl,elf,link,slot);
                break;
            }

            case op_type::lb:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::lh:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::lw:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::ld:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::lsb:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::lsw:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::sb:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::sh:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::sw:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::sd:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            case op_type::lea:
            {
                rewrite_rel_load_store(itl,elf,link);
                break;
            }

            default:
            {
                auto& info = info_from_op(opcode);
                printf("[ELF link X86]: unknown opcode: %s\n",info.fmt_string.buf);
                assert(false);
                break;
            }
        }
    }
}

// TODO: we need to generalise this away from x86
void link_elf(Interloper& itl, Elf& elf)
{
    link_opcodes(itl,elf);
}

void emit_elf(Interloper& itl)
{
    Elf elf = make_elf(itl,"test-prog");

    // Add every function in the emitter
    // NOTE: this adds just the definitons
    // we wont push the data in until we finalise the elf

    auto& asm_emitter = itl.asm_emitter;

    for(u32 f = 0; f < count(asm_emitter.func); f++)
    {
        auto& func = asm_emitter.func[f];
        add_func_symbol(elf,func);
    }
    

    finalise_elf(itl,elf);

    // now we know the poistions go back and "link" the text section
    link_elf(itl,elf);

    write_elf(elf);
    destroy_elf(elf);
}