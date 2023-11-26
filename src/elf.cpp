// https://www.sco.com/developers/gabi/latest/contents.html

#include <elf.h>

// NOTE: atm we only care about supporting 64 bit
struct Elf
{
    String name;
    
    arch_target arch = arch_target::x86_64_t;
    os_target os = os_target::linux_t;

    // basic elf header
    Elf64_Ehdr header;
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

    // inited later?
    // e_phoff, e_shoff, e_flags

    header.e_ehsize = sizeof(header);

    //header.e_phentsize = 

    //e_phnum, e_shentsize, e_shnum
    // e_shtrndx


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

Elf make_elf(const String& filename)
{
    Elf elf;
    elf.name = filename;

    // setup initial header section
    setup_header(elf);


    return elf;
}

void destroy_elf(Elf& elf)
{
    UNUSED(elf);
}

// insert a new function into the elf file
void add_function(Elf& elf,const String &func_name,u8* buffer, u32 size);

void write_elf(Elf& elf)
{
    std::ofstream fp(elf.name.buf,std::ios::out | std::ios::binary);

    if(!fp)
    {
        crash_and_burn("could not open file %s\n",elf.name.buf);
    }

    // for now just dump the header out
    file_write_arr(fp,(u8*)&elf.header,sizeof(elf.header));

    // TODO: set the executable perms on the file

    fp.close();
}


void build_test_binary()
{
    Elf elf = make_elf("test-prog");

    // shellcode for basic exit program
    /*
    _start:
        mov rbx, 0xdeadbeef
        mov rax, 1
        int 0x80
    */
    // add_function(elf,"_start",SHELLCODE,sizeof(SHELLCODE));
    
    write_elf(elf);
    destroy_elf(elf);
}