// https://www.sco.com/developers/gabi/latest/contents.html

#include <elf.h>

struct ElfFunc
{
    u32 offset;
    u32 size;
};

struct ElfTextSection
{
    u32 section_idx = 0;
    Elf64_Shdr header;
    
    u32 program_idx;
    Elf64_Phdr program_header;

    // NOTE: code is owned externally in the AsmEmitter struct
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

    ElfTextSection text_section;

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

void setup_text(Elf& elf)
{
    auto& text_section = elf.text_section;

    auto& section_header = text_section.header;

    memset(&section_header,0,sizeof(section_header));

    text_section.section_idx = add_section(elf,".text",section_header);

    section_header.sh_type = SHT_PROGBITS;
    section_header.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
    section_header.sh_addralign = 16;

    // setup program header
    auto& program_header = text_section.program_header;

    memset(&program_header,0,sizeof(program_header));

    // load, align to 4096 byte page, read | execute
    program_header.p_type = PT_LOAD;
    program_header.p_align = 4096;
    program_header.p_flags = PF_X | PF_R;

    text_section.program_idx = add_program_header(elf);
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


    // insert the null sym entry
    Elf64_Sym null_sym;
    memset(&null_sym,0,sizeof(null_sym));

    push_var(symbol_table.table,null_sym);
}


void add_func_symbol(Elf& elf,const AsmFunc& func)
{
    auto& symbol_table = elf.symbol_table;
    auto& symtab_string_table = elf.symtab_string_table;

    Elf64_Sym sym;
    memset(&sym,0,sizeof(sym));

    // add symbol name
    sym.st_name = push_string(symtab_string_table,func.name);

    // write the offset into value for now
    // we will rewrite it later
    sym.st_value = func.offset;

    // set as function
    sym.st_info = ELF64_ST_INFO(STB_GLOBAL,STT_FUNC);

    // setup size
    sym.st_size = func.size;

    // what seciton is this sym apart of
    sym.st_shndx = elf.text_section.section_idx;

    // add symbol
    push_var(symbol_table.table,sym);
}

Elf make_elf(const String& filename)
{
    Elf elf;
    elf.name = filename;

    // setup initial header section
    setup_header(elf);

    // setup the section string table
    setup_string(elf,elf.section_string_table,".shstrtab");

    setup_string(elf,elf.symtab_string_table,".strtab");


    // setup text section
    setup_text(elf);

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
    // add null section
    Elf64_Shdr null_section;
    memset(&null_section,0,sizeof(null_section));
    push_mem(elf.buffer,&null_section,sizeof(null_section));


    // add string section header
    auto& section_string_table = elf.section_string_table;
    push_mem(elf.buffer,&section_string_table.header,sizeof(section_string_table.header));

    // string table header
    auto& symtab_string_table = elf.symtab_string_table;
    push_mem(elf.buffer,&symtab_string_table.header,sizeof(symtab_string_table.header));

    // add text section header
    auto& text_section = elf.text_section;
    push_mem(elf.buffer,&text_section.header,sizeof(text_section.header));

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

void align_elf(Elf& elf, u32 align)
{
    const u64 cur_offset = elf.buffer.size;
    resize(elf.buffer,align_val(cur_offset,align));   
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
    auto& text_section = elf.text_section;

    // make sure page size aligns
    align_elf(elf,elf.text_section.program_header.p_align);

    const u64 text_offset = push_section_data(elf,text_section.section_idx,itl.asm_emitter.buffer,true);
    
    itl.asm_emitter.base_offset = text_offset;

    // finish up the program header
    write_program_header(elf,text_section.program_idx,offsetof(Elf64_Phdr,p_offset),text_offset);

    // start at 4MB
    const u64 vaddr = text_offset + BASE_ADDR;

    write_program_header(elf,text_section.program_idx,offsetof(Elf64_Phdr,p_vaddr),vaddr);
    write_program_header(elf,text_section.program_idx,offsetof(Elf64_Phdr,p_paddr),vaddr);


    // write size
    const u64 size = itl.asm_emitter.buffer.size;

    write_program_header(elf,text_section.program_idx,offsetof(Elf64_Phdr,p_filesz),size);
    write_program_header(elf,text_section.program_idx,offsetof(Elf64_Phdr,p_memsz),size);

    // add symtab
    auto& symbol_table = elf.symbol_table;

    // rewrite every symbol know we know start of text section
    for(u32 s = 1; s < count(symbol_table.table); s++)
    {
        auto& sym = symbol_table.table[s];

        const u32 offset = sym.st_value;

        const u32 func_addr = vaddr + offset;

        sym.st_value = func_addr;
    }

    // for itl
    // TODO: write the label info
    finalise_labels(itl,vaddr);

    // write entry point (i.e find start)

    const auto& start = *lookup(itl.function_table,String("start"));
    auto& start_label = label_from_slot(itl.symbol_table.label_lookup,start.label_slot);

    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_entry),start_label.offset);

    // align the symbol table data
    align_elf(elf,symbol_table.header.sh_addralign);

    push_section_data(elf,symbol_table.section_idx,symbol_table.table);
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
    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_phoff),elf.header.e_phoff);
    write_mem(elf.buffer,offsetof(Elf64_Ehdr,e_phnum),elf.program_count);
    
    // start writing out the headers
    auto &text_section = elf.text_section;
    push_mem(elf.buffer,&text_section.program_header,sizeof(text_section.program_header));

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

    const s32 rel_addr = (label.offset  - (link.offset + asm_emitter.base_vaddr)) - 4;

    write_mem(elf.buffer,text_offset + link.offset,rel_addr);
}

// TODO: we need to generalise this away from x86
void link_elf(Interloper& itl, Elf& elf)
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

            case op_type::b:
            {
                const LabelSlot slot = label_from_idx(opcode.v[0]);
                rewrite_rel_label(itl,elf,link,slot);
                break;
            }

            default:
            {
                assert(false);
                break;
            }
        }
    }
}

void emit_elf(Interloper& itl)
{
    Elf elf = make_elf("test-prog");

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