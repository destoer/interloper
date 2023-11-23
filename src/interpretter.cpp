#include <interloper.h>

void trace_add(Trace& trace, u64 src, u64 dst);

void reset_trace(Trace& trace)
{
    trace.idx = 0;
    memset(trace.history_target,0,sizeof(trace.history_target));
    memset(trace.history_source,0,sizeof(trace.history_source));    
}

void trace_add(Trace& trace, u64 src, u64 dst)
{
    trace.history_source[trace.idx] = src;
    trace.history_target[trace.idx] = dst;
    trace.idx = (trace.idx + 1) & 0xf;    
}

void print_trace(Trace& trace)
{        
    printf("pc trace:\n");
    for(int i = 0; i < 0x10; i++)
    {
        const auto offset = (trace.idx + i) & 0xf;
        printf("%d: %08lx -> %08lx\n",i,trace.history_source[offset],trace.history_target[offset]);
    }
}

void write_pc(Interpretter& interpretter, u64 target)
{
    trace_add(interpretter.trace,interpretter.regs[PC] - sizeof(Opcode),target);

    interpretter.regs[PC] = target;
}

// TODO: add a basic debugger to this

void print_regs(Interpretter& interpretter)
{
    puts("regs:");
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        printf("r%d = 0x%016lx\n",i,interpretter.regs[i]);
    }

    printf("PC = 0x%016lx\n",interpretter.regs[PC]);
    printf("SP = 0x%016lx\n",interpretter.regs[SP]);
}

// fread does not seem to play nice with stdin
// so we are gonna have to use the bottom level os primitives

const u32 FILE_R = 0;
const u32 FILE_W = 1;

// TODO: in proper vm we need the actual system.itl to do the heavy lifting with this translation
enum class file_opt
{
    create,
    append,
    open,
};

#ifdef __linux__

#include <fcntl.h>
#include <unistd.h>

bool handle_valid(s64 handle)
{
    return s64(handle) >= 0;
}

s64 os_open(const char* file, u32 mode, file_opt opt)
{
    u32 flag = 0;

    switch(mode)
    {
        case FILE_W: flag = O_WRONLY; break;
        case FILE_R: flag = O_RDONLY; break;
        default: printf("invalid file mode: %d\n",mode); return -1;
    }

    switch(opt)
    {
        case file_opt::create:
        {
            flag |= O_CREAT;
            break;
        }

        case file_opt::open: 
        {
            break;
        }

        case file_opt::append:
        {
            flag |= O_APPEND;
            break;
        }
    }

    if(strcmp(file,"stdout") == 0)
    {
        file = "/dev/stdout";
    }

    else if(strcmp(file,"stdin") == 0)
    {
        file = "/dev/stdin";
    }

    const s64 handle = open(file,flag);
/*
    if(handle == -1)
    {
        printf("[VM]: warning could not open file: %s : %d\n",file,mode);
        perror("error: ");
    }
*/
    return handle;
}

s64 os_read(s64 handle, void* buf, u32 len)
{
    return read(handle,buf,len);
}

s64 os_write(s64 handle, const void* buf, u32 len)
{
    return write(s64(handle),buf,len);
}

s64 os_close(s64 handle)
{
    return close(handle);
}

#else 

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

bool handle_valid(s64 handle)
{
    return (HANDLE)handle != INVALID_HANDLE_VALUE;
}

s64 os_open(const char* file, u32 mode, file_opt opt)
{
    u32 flag = 0;

    //printf("open: '%s'\n",file);

    switch(mode)
    {
        case FILE_W: flag = GENERIC_WRITE; break;
        case FILE_R: flag = GENERIC_READ; break;
        default: printf("invalid file mode: %d\n",mode); return -1;
    }

    u32 open_flag = 0;

    switch(opt)
    {
        case file_opt::create:
        {
            open_flag = CREATE_NEW;
            break;
        }

        case file_opt::append:
        {
            open_flag = OPEN_EXISTING;
            flag |= FILE_APPEND_DATA;
            break;
        }

        case file_opt::open:
        {
            open_flag = OPEN_EXISTING;
            break;
        }
    }

    if(strcmp(file,"stdin") == 0)
    {
        return s64(GetStdHandle(STD_INPUT_HANDLE));
    }

    else if(strcmp(file,"stdout") == 0)
    {
        return s64(GetStdHandle(STD_OUTPUT_HANDLE));
    }

    // NOTE: this needs OPEN_EXISTING for con io
    const HANDLE handle = CreateFileA(file,flag,FILE_SHARE_READ,NULL,open_flag,FILE_ATTRIBUTE_NORMAL,NULL); 

    if(handle == INVALID_HANDLE_VALUE)
    {
        //printf("[VM]: warning could not open file: %s : %d\n",file,mode);
    }

    return s64(handle);
}

s64 os_read(s64 handle, void* buf, u32 len)
{
    DWORD read = 0;
    
    if(!ReadFile((HANDLE)handle,buf,len,&read,NULL))
    {
        //printf("failed to read file: %d\n",GetLastError());
        return -1;
    }

    return read;
}

s64 os_write(s64 handle, const void* buf, u32 len)
{
    DWORD wrote = 0;
    
    if(!WriteFile((HANDLE)handle,buf,len,&wrote,NULL))
    {
        return -1;
    }
                 
    return wrote;
}

s64 os_close(s64 handle)
{
    return CloseHandle((HANDLE)handle);
}

#endif

template<typename access_type>
access_type read_mem(Interpretter& interpretter,u32 addr)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    const u32 size = sizeof(access_type);

    if(addr + size <= interpretter.program.size)
    {
        return handle_read<access_type>(&interpretter.program[addr]);
    }

    else if(addr >= interpretter.program.size && addr + size <= interpretter.program.size +  interpretter.global.size)
    {
        return handle_read<access_type>(&interpretter.global[addr - interpretter.program.size]);
    }

    else if(addr >= 0x20000000 && addr + size <= 0x20000000 + interpretter.stack.size)
    {
        return handle_read<access_type>(&interpretter.stack[addr - 0x20000000]);
    }

    else if(addr >= 0x3000'0000 && addr + size <= 0x3000'0000 + interpretter.alloc.size)
    {
        return handle_read<access_type>(&interpretter.alloc[addr - 0x3000'0000]);
    }

    else
    {
        print_regs(interpretter);
        crash_and_burn("%lx: out of bounds read of %x at %x\n",interpretter.regs[PC] - sizeof(Opcode),size,addr);
    }
}

static_assert(PROGRAM_ORG == 0);

// get a pointer, plus size left in the section
std::pair<void*,u32> get_vm_ptr_internal(Interpretter& interpretter,u32 addr)
{
    // read out the program
    if(addr <= interpretter.program.size)
    {
        return std::pair{&interpretter.program.data[addr],interpretter.program.size - addr};
    }

    else if(addr >= interpretter.program.size && addr <= interpretter.program.size +  interpretter.global.size)
    {
        return std::pair{&interpretter.global[addr - interpretter.program.size],((interpretter.program.size +  interpretter.global.size) - addr)};
    }

    else if(addr >= 0x20000000 && addr <= 0x20000000 + interpretter.stack.size)
    {
        return std::pair{&interpretter.stack[addr - 0x20000000],(0x20000000 + interpretter.stack.size) - addr};
    }

    else if(addr >= 0x3000'0000 && addr <= 0x3000'0000 + interpretter.alloc.size)
    {
        return std::pair(&interpretter.alloc[addr - 0x3000'0000],(0x3000'0000 + interpretter.alloc.size) - addr );
    }

    else
    {
        return std::pair{nullptr,0};
    }    
}

void* get_vm_ptr(Interpretter& interpretter, u32 addr, u32 size)
{
    auto [ptr,remain] = get_vm_ptr_internal(interpretter,addr);

    if(remain < size)
    {
        return nullptr;
    }

    return ptr;
}

char* get_vm_str(Interpretter& interpretter, u32 addr)
{
    auto [ptr,remain] = get_vm_ptr_internal(interpretter,addr);

    char* str = (char*)ptr;

    while(remain)
    {
        // null terminated string
        if(*str == '\0')
        {
            return (char*)ptr;
        }

        str++;
        remain--;
    }

    return nullptr;
}

template<typename access_type>
void write_mem(Interpretter& interpretter,u32 addr, access_type v)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    const u32 size = sizeof(access_type);

    if(addr >= interpretter.program.size && addr + size <= interpretter.program.size +  interpretter.global.size)
    {
        handle_write<access_type>(&interpretter.global[addr - interpretter.program.size],v);
    }

    else if(addr >= 0x20000000 && addr + size <= 0x20000000 + interpretter.stack.size)
    {
        handle_write<access_type>(&interpretter.stack[addr - 0x20000000],v);
    }

    else if(addr >= 0x3000'0000 && addr + size <= 0x3000'0000 + interpretter.alloc.size)
    {
        handle_write<access_type>(&interpretter.alloc[addr - 0x3000'0000],v);
    }

    else
    {
        print_regs(interpretter);
        crash_and_burn("%lx: out of bounds write of %x at %x:%x\n",interpretter.regs[PC] - sizeof(Opcode),size,addr,v);
    }
}


void execute_opcode(Interpretter& interpretter,const Opcode &opcode)
{
    auto &regs = interpretter.regs;

    switch(opcode.op)
    {

        case op_type::mov_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]];
            break;
        }

        case op_type::add_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] + regs[opcode.v[2]];
            break;
        }


        case op_type::sub_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] - regs[opcode.v[2]];
            break;
        }

        case op_type::mul_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] * regs[opcode.v[2]];
            break;
        }

        case op_type::or_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] | regs[opcode.v[2]];
            break;
        }

        case op_type::and_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] & regs[opcode.v[2]];
            break;
        }


        case op_type::lsl_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] << regs[opcode.v[2]];
            break;
        }

        case op_type::lsr_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] >> regs[opcode.v[2]];
            break;
        }

        case op_type::asr_reg:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) >> regs[opcode.v[2]];
            break;
        }

        case op_type::xor_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] ^ regs[opcode.v[2]];
            break;
        }

        case op_type::not_reg:
        {
            regs[opcode.v[0]] = ~regs[opcode.v[1]];
            break;
        }

        case op_type::div_reg:
        {
            if(regs[opcode.v[2]] == 0)
            {
                print_regs(interpretter);
                crash_and_burn("division by zero at %08x\n",regs[PC]);
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] / regs[opcode.v[2]];
            break;
        }

        case op_type::mod_reg:
        {
            if(regs[opcode.v[2]] == 0)
            {
                print_regs(interpretter);
                crash_and_burn("mod by zero at %08x\n",regs[PC]);
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] % regs[opcode.v[2]];
            break;
        }

        case op_type::sxb:
        {
            regs[opcode.v[0]] = s64(s8(regs[opcode.v[1]]));
            break;
        }

        case op_type::sxh:
        {
            regs[opcode.v[0]] = s64(s16(regs[opcode.v[1]]));
            break;
        }

        case op_type::sxw:
        {
            regs[opcode.v[0]] = s64(s32(regs[opcode.v[1]]));
            break;
        }

        case op_type::mov_imm:
        {
            regs[opcode.v[0]] = opcode.v[1];
            break;
        }


        case op_type::sub_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] - opcode.v[2];
            break;
        }

        case op_type::add_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] + opcode.v[2];
            break;
        }

        case op_type::mul_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] * opcode.v[2];
            break;
        }

        case op_type::and_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] & opcode.v[2];
            break;
        }

        case op_type::xor_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] ^ opcode.v[2];
            break;
        }

        case op_type::lb:
        {
            regs[opcode.v[0]] = read_mem<u8>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::lh:
        {
            regs[opcode.v[0]] = read_mem<u16>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::lw:
        {
            regs[opcode.v[0]] = read_mem<u32>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }


        case op_type::ld:
        {
            regs[opcode.v[0]] = read_mem<u64>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::lsb:
        {
            regs[opcode.v[0]] = read_mem<s8>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::lsh:
        {
            regs[opcode.v[0]] = read_mem<s16>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::lsw:
        {
            regs[opcode.v[0]] = read_mem<s32>(interpretter,regs[opcode.v[1]]+opcode.v[2]);
            break;
        }

        case op_type::sb:
        {
            write_mem<u8>(interpretter,regs[opcode.v[1]]+opcode.v[2],regs[opcode.v[0]]);
            break;
        }

        case op_type::sh:
        {
            write_mem<u16>(interpretter,regs[opcode.v[1]]+opcode.v[2],regs[opcode.v[0]]);
            break;
        }


        case op_type::lea:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] + opcode.v[2];
            break;
        }

        case op_type::sw:
        {
            write_mem<u32>(interpretter,regs[opcode.v[1]]+opcode.v[2],regs[opcode.v[0]]);
            break;
        }

        case op_type::sd:
        {
            write_mem<u64>(interpretter,regs[opcode.v[1]]+opcode.v[2],regs[opcode.v[0]]);
            break;
        }

        case op_type::push:
        {
            const u64 v = regs[opcode.v[0]];
            regs[SP] -= GPR_SIZE;
            write_mem<u64>(interpretter,regs[SP],v); 
            break;               
        }

        case op_type::pop:
        {
            regs[opcode.v[0]] = read_mem<u64>(interpretter,regs[SP]);
            regs[SP] += GPR_SIZE;
            break;
        }


        case op_type::pushm:
        {
            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(opcode.v[0],r))
                { 
                    const u64 v = regs[r];
                    
                    regs[SP] -= GPR_SIZE;
                    write_mem<u64>(interpretter,regs[SP],v); 
                }
            }
            break;               
        }

        case op_type::popm:
        {
            for(s32 r = MACHINE_REG_SIZE - 1; r >= 0; r--)
            {
                if(is_set(opcode.v[0],r))
                { 
                    regs[r] = read_mem<u64>(interpretter,regs[SP]);
                    regs[SP] += GPR_SIZE;
                }
            }
            break;
        }

        case op_type::call:
        {
            // push
            
            regs[SP] -= GPR_SIZE;
            write_mem<u64>(interpretter,regs[SP],regs[PC]);

            write_pc(interpretter,opcode.v[0]);
            break;
        }

        case op_type::call_reg:
        {
            // push
            regs[SP] -= GPR_SIZE;
            write_mem<u64>(interpretter,regs[SP],regs[PC]);

            write_pc(interpretter,regs[opcode.v[0]]);
            break;
        }


        case op_type::ret:
        {              
            // pop pc
            const u64 target = read_mem<u64>(interpretter,regs[SP]);
            write_pc(interpretter,target);

            regs[SP] += GPR_SIZE;
            break;
        }

        // signed compare
        case op_type::cmpsgt_imm:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) > s64(opcode.v[2]);
            break;                
        }

        case op_type::cmpslt_reg:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) < s64(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsle_reg:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) <= s64(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsgt_reg:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) > s64(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsge_reg:
        {
            regs[opcode.v[0]] = s64(regs[opcode.v[1]]) >= s64(regs[opcode.v[2]]);
            break;
        }

        // unsigned compare
        case op_type::cmpugt_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] > opcode.v[2];
            break;                
        }

        case op_type::cmpult_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] < regs[opcode.v[2]];
            break;
        }

        case op_type::cmpule_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] <= regs[opcode.v[2]];
            break;
        }

        case op_type::cmpugt_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] > regs[opcode.v[2]];
            break;
        }

        case op_type::cmpuge_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] >= regs[opcode.v[2]];
            break;
        }


        // compare equality
        case op_type::cmpeq_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] == regs[opcode.v[2]];
            break;
        }

        case op_type::cmpne_reg:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] != regs[opcode.v[2]];
            break;
        }

        case op_type::cmpeq_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] == opcode.v[2];
            break;
        }

        case op_type::cmpne_imm:
        {
            regs[opcode.v[0]] = regs[opcode.v[1]] != opcode.v[2];
            break;
        }

        case op_type::bnc:
        {
            if(!regs[opcode.v[1]])
            {
                write_pc(interpretter,opcode.v[0]);
            }
            break;
        }

        case op_type::bc:
        {
            if(regs[opcode.v[1]])
            {
                write_pc(interpretter,opcode.v[0]);
            }
            break;
        }


        case op_type::b:
        {
            write_pc(interpretter,opcode.v[0]);
            break;
        }

        case op_type::b_reg:
        {
            write_pc(interpretter,regs[opcode.v[0]]);
            break;
        }

        // system call
        case op_type::swi:
        {
            // TODO: our interpreter relies on 64 bit pointers for now
            static_assert(sizeof(u32*) == 8);

            switch(opcode.v[0])
            {
                case SYSCALL_EXIT: // exit
                {
                    interpretter.quit = true;
                    break;
                }

                case SYSCALL_OPEN:
                {
                    char* ptr = get_vm_str(interpretter,regs[R0]); 

                    if(!ptr)
                    {
                        crash_and_burn("out of str for file open at %lx:%x\n",interpretter.regs[PC] - sizeof(Opcode),regs[R0]);
                    }

                    const u32 mode = regs[R1];

                    if(mode > FILE_W)
                    {
                        crash_and_burn("invalid file mode for at %lx:%x\n",interpretter.regs[PC] - sizeof(Opcode),regs[R1]);
                    }

                    if(regs[R2] > u32(file_opt::open))
                    {
                        crash_and_burn("invalid file opt: %d",regs[R2]);
                    }


                    regs[R0] = os_open(ptr,mode,file_opt(regs[R2]));
                    break;
                }

                case SYSCALL_CLOSE:
                {
                    const s64 handle = regs[0];

                    if(handle_valid(handle))
                    {
                        os_close(handle);
                    }
                    break;
                }

                case SYSCALL_WRITE:
                {
                    const s64 handle = regs[0];
                    const u32 len = regs[2];

                    void* ptr = get_vm_ptr(interpretter,regs[1],len);

                    if(!ptr)
                    {
                        crash_and_burn("out of bounds file write at %lx:%x:%x\n",interpretter.regs[PC] - sizeof(Opcode),regs[R1],len);
                    }

                    if(!handle_valid(handle))
                    {
                        crash_and_burn("invalid file handle for write at %lx\n",interpretter.regs[PC] - sizeof(Opcode));
                    }

                    //printf("write %ld: %lx %d\n",handle,regs[1],len);

                    regs[R0] = os_write(handle,ptr,len);
                    break;
                }

                case SYSCALL_READ:
                {
                    const s64 handle = regs[0];
                    const u32 len = regs[2];

                    void* ptr = get_vm_ptr(interpretter,regs[1],len);

                    if(!ptr)
                    {
                        crash_and_burn("out of bounds file read at %lx:%x:%x\n",interpretter.regs[PC] - sizeof(Opcode),regs[R1],len);
                    }

                    if(!handle_valid(handle))
                    {
                        crash_and_burn("invalid file handle for read at %lx\n",interpretter.regs[PC] - sizeof(Opcode));
                    }

                    regs[R0] = os_read(handle,ptr,len);
                    break;                  
                }

                case SYSCALL_ALLOC:
                {
                    const u64 size = regs[0];
                    const u64 old_size = interpretter.alloc.size;

                    const u64 new_size = size + old_size;
                    resize(interpretter.alloc,new_size);

                    //printf("alloc: %ld\n",size);

                    // alloc is at the end of the old one
                    regs[R0] = 0x3000'0000 + old_size;
                    break;
                }

                case SYSCALL_TIME:
                {
                    regs[R0] = time(NULL);
                    break;
                }

                default:
                {
                    printf("unknown syscall: %lx\n",opcode.v[0]);
                    break;
                }
            }
            break;
        }

        // directives/pseudo ops should not be hit at runtime..
        case op_type::push_arg:
        case op_type::clean_args:
        case op_type::alloc_stack:
        case op_type::pool_addr:
        case op_type::alloc_slot:
        case op_type::alloc_local_array:
        case op_type::alloc_global_array:
        case op_type::free_fixed_array:
        case op_type::free_slot:
        case op_type::exit_block:
        case op_type::placeholder:
        case op_type::spill:
        case op_type::spill_all:
        case op_type::reload_slot:
        case op_type::spill_slot:
        case op_type::spill_func_bounds:
        case op_type::load:
        case op_type::addrof:
        case op_type::buf_alloc:
        case op_type::alloc_vla:
        case op_type::state_dump:
        case op_type::load_struct_s8:
        case op_type::load_struct_s16:
        case op_type::load_struct_s32:
        case op_type::load_struct_u8:
        case op_type::load_struct_u16:
        case op_type::load_struct_u32:
        case op_type::load_struct_u64:
        case op_type::store_struct_u8:
        case op_type::store_struct_u16:
        case op_type::store_struct_u32:
        case op_type::store_struct_u64:
        case op_type::load_func_addr:
        case op_type::END:
        {
            print_regs(interpretter);
            crash_and_burn("directive not removed!?");
        }

    }    
}

void reset(Interpretter& interpretter, u32 global_size)
{
    memset(interpretter.regs,0,sizeof(interpretter.regs));
    memset(interpretter.stack.data,0,interpretter.stack.size);

    if(global_size)
    {
        resize(interpretter.global,global_size);
    }

    interpretter.regs[PC] = 0;
    interpretter.regs[SP] = 0x20000000 + count(interpretter.stack);

    interpretter.quit = false;

    reset_trace(interpretter.trace);
}

Interpretter make_interpretter()
{
    Interpretter interpretter;

    resize(interpretter.stack,STACK_SIZE);

    reset(interpretter,0);

    return interpretter;
}

void destroy_interpretter(Interpretter& interpretter)
{
    destroy_arr(interpretter.stack);
    destroy_arr(interpretter.global);
    destroy_arr(interpretter.alloc);
}

s32 run(Interpretter& interpretter,const Array<u8>& program, u32 global_size)
{
    printf("starting progam execution: %x bytes long\n",program.size);
    
    reset(interpretter,global_size);

    auto &regs = interpretter.regs;

    interpretter.program = program;

    while(!interpretter.quit)
    {
    
        // force pc only be program area for simplicty
        // self modfying code inside this vm would not be very useful anyways
        if(regs[PC] + sizeof(Opcode) > program.size)
        {
            print_regs(interpretter);
            print_trace(interpretter.trace);
            crash_and_burn("attempted to execute out of bounds: %x : %x\n",regs[PC],interpretter.program.size);
        }

        if((regs[PC] % sizeof(Opcode)) != 0)
        {            
            print_regs(interpretter);
            print_trace(interpretter.trace);
            crash_and_burn("attempted to execute mid instr: %x : %x\n",regs[PC],interpretter.program.size);            
        }

        const auto opcode = read_mem<Opcode>(interpretter.program,regs[PC]);

    #if 0
        printf("%016x: ",regs[PC]);
        disass_opcode_raw(opcode);
    #endif
    
        regs[PC] += OP_SIZE;

        execute_opcode(interpretter,opcode);
    }

    // destroy any heap allocations
    destroy_arr(interpretter.alloc);

    //print_trace(interpretter.trace);

    const s32 exit_code = s32(regs[0]);

    printf("exit: %d : %x\n",exit_code,exit_code);
    return exit_code;
}

