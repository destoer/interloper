#include <interloper.h>


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
        printf("%x: warning out of bounds read at %x\n",interpretter.regs[PC],addr);
        return 0;
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
        printf("%x: warning out of bounds write at %x:%x\n",interpretter.regs[PC],addr,v);
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
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) >> regs[opcode.v[2]];
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
                printf("division by zero at %08x\n",regs[PC]);
                break;
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] / regs[opcode.v[2]];
            break;
        }

        case op_type::mod_reg:
        {
            if(regs[opcode.v[2]] == 0)
            {
                printf("mod by zero at %08x\n",regs[PC]);
                break;
            }

            regs[opcode.v[0]] = regs[opcode.v[1]] % regs[opcode.v[2]];
            break;
        }

        case op_type::sxb:
        {
            regs[opcode.v[0]] = static_cast<s32>(static_cast<s8>(regs[opcode.v[1]]));
            break;
        }

        case op_type::sxh:
        {
            regs[opcode.v[0]] = static_cast<s32>(static_cast<s16>(regs[opcode.v[1]]));
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


        case op_type::sw:
        {
            write_mem<u32>(interpretter,regs[opcode.v[1]]+opcode.v[2],regs[opcode.v[0]]);
            break;
        }

        case op_type::push:
        {
            regs[SP] -= sizeof(u32);
            write_mem<u32>(interpretter,regs[SP],regs[opcode.v[0]]); 
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
                    regs[SP] -= sizeof(u32);
                    write_mem<u32>(interpretter,regs[SP],regs[r]); 
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
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) > static_cast<s32>(opcode.v[2]);
            break;                
        }

        case op_type::cmpslt_reg:
        {
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) < static_cast<s32>(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsle_reg:
        {
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) <= static_cast<s32>(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsgt_reg:
        {
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) > static_cast<s32>(regs[opcode.v[2]]);
            break;
        }

        case op_type::cmpsge_reg:
        {
            regs[opcode.v[0]] = static_cast<s32>(regs[opcode.v[1]]) >= static_cast<s32>(regs[opcode.v[2]]);
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
                case 0x0: // exit
                {
                    interpretter.quit = true;
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

        // directives should not be hit at runtime..
        case op_type::push_arg:
        case op_type::clean_args:
        case op_type::alloc_slot:
        case op_type::free_slot:
        case op_type::save_regs:
        case op_type::restore_regs:
        case op_type::exit_block:
        case op_type::placeholder:
        case op_type::spill_rv:
        case op_type::spill:
        case op_type::load:
        case op_type::END:
        {
            puts("directive not removed!?");
            exit(1);
        }

    }    
}

void reset(Interpretter& interpretter)
{
    interpretter.stack.resize(16 * 1024 * 1024);
    std::fill(interpretter.stack.begin(),interpretter.stack.end(),0);

    memset(interpretter.regs,0,sizeof(interpretter.regs));

    interpretter.regs[PC] = 0;
    interpretter.regs[SP] = 0x20000000 + interpretter.stack.size();

    interpretter.quit = false;
}

s32 run(Interpretter& interpretter,const u8 *program, u32 size)
{
    //puts("BOOP!"); exit(1);

    puts("startring progam execution\n\n\n");
    panic(!program, "attempted to execute empty program");
    reset(interpretter);
    

    auto &regs = interpretter.regs;

    while(!interpretter.quit)
    {
        Opcode opcode;

        // force pc only be program area for simplicty
        // self modfying code inside this vm would not be very useful anyways
        if(regs[PC] + sizeof(opcode) > size)
        {
            panic("attempted to execute out of bounds: %x : %x\n",regs[PC],size);
        }

        memcpy(&opcode,&program[regs[PC]],sizeof(opcode));

        //printf("%08x: ",regs[PC]);
        //disass_opcode_raw(opcode);

        regs[PC] += OP_SIZE;

        execute_opcode(interpretter,opcode);
    }

    printf("exit code: %d\n",regs[0]);
    return regs[0];
}

