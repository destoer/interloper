#include <interloper.h>


void print_regs(Interpretter& interpretter)
{
    puts("regs:");
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        printf("r%d = 0x%08x\n",i,interpretter.regs[i]);
    }

    printf("PC = 0x%08x\n",interpretter.regs[PC]);
    printf("SP = 0x%08x\n",interpretter.regs[SP]);
}

template<typename access_type>
access_type read_mem(Interpretter& interpretter,u32 addr)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + interpretter.stack.size())
    {
        return handle_read<access_type>(interpretter.stack.data(),addr - 0x20000000);
    }

    else
    {
        print_regs(interpretter);
        panic("%x: out of bounds read at %x\n",interpretter.regs[PC] - sizeof(Opcode),addr);
    }
}

static_assert(PROGRAM_ORG == 0);
void* get_vm_ptr(Interpretter& interpretter,u32 addr, u32 size)
{
    // read out the program
    if(addr + size <= interpretter.program.size)
    {
        return &interpretter.program.data[addr];
    }

    if(addr >= 0x20000000 && addr + size < 0x20000000 + interpretter.stack.size())
    {
        return &interpretter.stack[addr - 0x20000000];
    }

    else
    {
        return nullptr;
    }    
}

template<typename access_type>
void write_mem(Interpretter& interpretter,u32 addr, access_type v)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + interpretter.stack.size())
    {
        handle_write<access_type>(interpretter.stack.data(),addr - 0x20000000,v);
    }

    else
    {
        print_regs(interpretter);
        panic("%08x: out of bounds write at %x:%x\n",interpretter.regs[PC] - sizeof(Opcode),addr,v);
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
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) >> regs[opcode.v[2]];
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
                panic("division by zero at %08x\n",regs[PC]);
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] / regs[opcode.v[2]];
            break;
        }

        case op_type::mod_reg:
        {
            if(regs[opcode.v[2]] == 0)
            {
                print_regs(interpretter);
                panic("mod by zero at %08x\n",regs[PC]);
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] % regs[opcode.v[2]];
            break;
        }

        case op_type::sxb:
        {
            regs[opcode.v[0]] = s32(s8(regs[opcode.v[1]]));
            break;
        }

        case op_type::sxh:
        {
            regs[opcode.v[0]] = s32(s16(regs[opcode.v[1]]));
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

        case op_type::push:
        {
            const u32 v = regs[opcode.v[0]];
            regs[SP] -= sizeof(u32);
            write_mem<u32>(interpretter,regs[SP],v); 
            break;               
        }

        case op_type::pop:
        {
            regs[opcode.v[0]] = read_mem<u32>(interpretter,regs[SP]);
            regs[SP] += sizeof(u32);
            break;
        }


        case op_type::pushm:
        {
            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(opcode.v[0],r))
                { 
                    const u32 v = regs[r];
                    
                    regs[SP] -= sizeof(u32);
                    write_mem<u32>(interpretter,regs[SP],v); 
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
                    regs[r] = read_mem<u32>(interpretter,regs[SP]);
                    regs[SP] += sizeof(u32);
                }
            }
            break;
        }

        case op_type::call:
        {
            // push
            
            regs[SP] -= sizeof(u32);
            write_mem<u32>(interpretter,regs[SP],regs[PC]);

            regs[PC] = opcode.v[0];
            break;
        }

        case op_type::ret:
        {              
            // pop pc
            regs[PC] = read_mem<u32>(interpretter,regs[SP]);
            regs[SP] += sizeof(u32);
            break;
        }

        // signed compare
        case op_type::cmpsgt_imm:
        {
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) > s32(opcode.v[2]);
            break;                
        }

        case op_type::cmpslt_reg:
        {
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) < s32(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsle_reg:
        {
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) <= s32(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsgt_reg:
        {
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) > s32(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsge_reg:
        {
            regs[opcode.v[0]] = s32(regs[opcode.v[1]]) >= s32(regs[opcode.v[2]]);
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

        case op_type::bnc:
        {
            if(!regs[opcode.v[1]])
            {
                regs[PC] = opcode.v[0];
            }
            break;
        }

        case op_type::bc:
        {
            if(regs[opcode.v[1]])
            {
                regs[PC] = opcode.v[0];
            }
            break;
        }


        case op_type::b:
        {
            regs[PC] = opcode.v[0];
            break;
        }

        // system call
        case op_type::swi:
        {
            switch(opcode.v[0])
            {
                case SYSCALL_EXIT: // exit
                {
                    interpretter.quit = true;
                    break;
                }

                // TODO: bounds check this
                case SYSCALL_WRITE_STRING:
                {   
                    void* ptr = get_vm_ptr(interpretter,regs[R0],regs[R1]);

                    if(!ptr)
                    {
                        panic("out of bounds print at %x:%x\n",interpretter.regs[PC] - sizeof(Opcode),regs[R0]);
                        return;
                    }

                    fwrite(ptr,1,regs[R1],stdout);
                    break;
                }

                default:
                {
                    printf("unknown syscall: %x\n",opcode.v[0]);
                    break;
                }
            }
            break;
        }

        // directives/pseudo ops should not be hit at runtime..
        case op_type::load_arr_len:
        case op_type::load_arr_data:
        case op_type::init_arr_idx:
        case op_type::arr_index:
        case op_type::push_arg:
        case op_type::clean_args:
        case op_type::free_stack:
        case op_type::alloc_stack:
        case op_type::pool_addr:
        case op_type::alloc_slot:
        case op_type::alloc:
        case op_type::free_slot:
        case op_type::free_reg:
        case op_type::save_regs:
        case op_type::restore_regs:
        case op_type::exit_block:
        case op_type::placeholder:
        case op_type::spill_rv:
        case op_type::spill:
        case op_type::load:
        case op_type::addrof:
        case op_type::buf_alloc:
        case op_type::alloc_vla:
        case op_type::state_dump:
        case op_type::END:
        {
            print_regs(interpretter);
            panic("directive not removed!?");
        }

    }    
}

void reset(Interpretter& interpretter)
{
    interpretter.stack.resize(STACK_SIZE);
    std::fill(interpretter.stack.begin(),interpretter.stack.end(),0);

    memset(interpretter.regs,0,sizeof(interpretter.regs));

    interpretter.regs[PC] = 0;
    interpretter.regs[SP] = 0x20000000 + interpretter.stack.size();

    interpretter.quit = false;
}

s32 run(Interpretter& interpretter,const Array<u8>& program)
{
    //puts("BOOP!"); exit(1);

    printf("starting progam execution: %x bytes long\n",program.size);
    reset(interpretter);
    

    auto &regs = interpretter.regs;

    interpretter.program = program;

    while(!interpretter.quit)
    {
    
        // force pc only be program area for simplicty
        // self modfying code inside this vm would not be very useful anyways
        if(regs[PC] + sizeof(Opcode) > program.size)
        {
            print_regs(interpretter);
            panic("attempted to execute out of bounds: %x : %x\n",regs[PC],interpretter.program.size);
        }

        const auto opcode = read_var<Opcode>(interpretter.program,regs[PC]);

    #if 0
        printf("%08x: ",regs[PC]);
        disass_opcode_raw(opcode);
    #endif
    
        regs[PC] += OP_SIZE;


        execute_opcode(interpretter,opcode);
    }

    printf("\nexit code: %d\n",regs[0]);
    return regs[0];
}

