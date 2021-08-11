#include <interloper.h>


template<typename access_type>
access_type Interpretter::read_mem(u32 addr)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + stack.size())
    {
        return handle_read<access_type>(stack.data(),addr - 0x20000000);
    }

    else
    {
        printf("%x: warning out of bounds read at %x\n",regs[PC],addr);
        return 0;
    }
}

template<typename access_type>
void Interpretter::write_mem(u32 addr, access_type v)
{
    // force align access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + stack.size())
    {
        handle_write<access_type>(stack.data(),addr - 0x20000000,v);
    }

    else
    {
        printf("%x: warning out of bounds write at %x:%x\n",regs[PC],addr,v);
    }
}


s32 Interpretter::run(const u8 *program, u32 size)
{
    puts("startring progam execution\n\n\n");

    this->program = program;
    this->size = size;

    assert(program);

    stack.resize(16 * 1024 * 1024);
    std::fill(stack.begin(),stack.end(),0);

    memset(regs,0,sizeof(regs));

    regs[PC] = 0;
    regs[SP] = 0x20000000 + stack.size();


    bool quit = false;

    while(!quit)
    {
        Opcode opcode;

        // force pc only be program area for simplicty
        // self modfying code inside this vm would not be very useful anyways
        if(regs[PC] + sizeof(opcode) > size)
        {
            printf("attempted to execute out of bounds: %x : %x\n",regs[PC],size);
            exit(1);
        }

        memcpy(&opcode,&program[regs[PC]],sizeof(opcode));

        //printf("%08x: ",regs[PC]);
        //disass_opcode_raw(opcode);

        regs[PC] += OP_SIZE;

        switch(opcode.op)
        {

            case op_type::mov_reg:
            {
                regs[opcode.v1] = regs[opcode.v2];
                break;
            }

            case op_type::add_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] + regs[opcode.v3];
                break;
            }


            case op_type::sub_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] - regs[opcode.v3];
                break;
            }

            case op_type::mul_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] * regs[opcode.v3];
                break;
            }

            case op_type::or_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] | regs[opcode.v3];
                break;
            }

            case op_type::and_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] & regs[opcode.v3];
                break;
            }

            case op_type::xor_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] ^ regs[opcode.v3];
                break;
            }

            case op_type::not_reg:
            {
                regs[opcode.v1] = ~regs[opcode.v1];
                break;
            }

            case op_type::div_reg:
            {
                if(regs[opcode.v3] == 0)
                {
                    printf("division by zero at %08x\n",regs[PC]);
                    break;
                }

                regs[opcode.v1] = regs[opcode.v2] / regs[opcode.v3];
                break;
            }

            case op_type::sxb:
            {
                regs[opcode.v1] = static_cast<s32>(static_cast<s8>(regs[opcode.v2]));
                break;
            }

            case op_type::sxh:
            {
                regs[opcode.v1] = static_cast<s32>(static_cast<s16>(regs[opcode.v2]));
                break;
            }

            case op_type::mov_imm:
            {
                regs[opcode.v1] = opcode.v2;
                break;
            }


            case op_type::sub_imm:
            {
                regs[opcode.v1] = regs[opcode.v2] - opcode.v3;
                break;
            }

            case op_type::add_imm:
            {
                regs[opcode.v1] = regs[opcode.v2] + opcode.v3;
                break;
            }


            case op_type::and_imm:
            {
                regs[opcode.v1] = regs[opcode.v2] & opcode.v3;
                break;
            }

            case op_type::xor_imm:
            {
                regs[opcode.v1] = regs[opcode.v2] ^ opcode.v3;
                break;
            }

            case op_type::lb:
            {
                regs[opcode.v1] = read_mem<u8>(regs[opcode.v2]+opcode.v3);
                break;
            }

            case op_type::lh:
            {
                regs[opcode.v1] = read_mem<u16>(regs[opcode.v2]+opcode.v3);
                break;
            }

            case op_type::lw:
            {
                regs[opcode.v1] = read_mem<u32>(regs[opcode.v2]+opcode.v3);
                break;
            }

            case op_type::lsb:
            {
                regs[opcode.v1] = read_mem<s8>(regs[opcode.v2]+opcode.v3);
                break;
            }

            case op_type::lsh:
            {
                regs[opcode.v1] = read_mem<s16>(regs[opcode.v2]+opcode.v3);
                break;
            }

            case op_type::sb:
            {
                write_mem<u8>(regs[opcode.v2]+opcode.v3,regs[opcode.v1]);
                break;
            }

            case op_type::sh:
            {
                write_mem<u16>(regs[opcode.v2]+opcode.v3,regs[opcode.v1]);
                break;
            }


            case op_type::sw:
            {
                write_mem<u32>(regs[opcode.v2]+opcode.v3,regs[opcode.v1]);
                break;
            }

            case op_type::push:
            {
                regs[SP] -= sizeof(u32);
                write_mem<u32>(regs[SP],regs[opcode.v1]); 
                break;               
            }

            case op_type::pop:
            {
                regs[opcode.v1] = read_mem<u32>(regs[SP]);
                regs[SP] += sizeof(u32);
                break;
            }


            case op_type::call:
            {
                // push
               
                regs[SP] -= sizeof(u32);
                write_mem<u32>(regs[SP],regs[PC]);

                regs[PC] = opcode.v1;
                break;
            }

            // TODO: why does this read out of bounds
            case op_type::ret:
            {              
                // pop pc
                regs[PC] = read_mem<u32>(regs[SP]);
                regs[SP] += sizeof(u32);
                break;
            }

            // signed compare
            case op_type::cmpsgt_imm:
            {
                regs[opcode.v1] = static_cast<s32>(regs[opcode.v2]) > static_cast<s32>(opcode.v3);
                break;                
            }

            case op_type::cmpslt_reg:
            {
                regs[opcode.v1] = static_cast<s32>(regs[opcode.v2]) < static_cast<s32>(regs[opcode.v3]);
                break;
            }

            case op_type::cmpsle_reg:
            {
                regs[opcode.v1] = static_cast<s32>(regs[opcode.v2]) <= static_cast<s32>(regs[opcode.v3]);
                break;
            }

            case op_type::cmpsgt_reg:
            {
                regs[opcode.v1] = static_cast<s32>(regs[opcode.v2]) > static_cast<s32>(regs[opcode.v3]);
                break;
            }

            case op_type::cmpsge_reg:
            {
                regs[opcode.v1] = static_cast<s32>(regs[opcode.v2]) >= static_cast<s32>(regs[opcode.v3]);
                break;
            }



            // unsigned compare
            case op_type::cmpugt_imm:
            {
                regs[opcode.v1] = regs[opcode.v2] > opcode.v3;
                break;                
            }

            case op_type::cmpult_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] < regs[opcode.v3];
                break;
            }

            case op_type::cmpule_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] <= regs[opcode.v3];
                break;
            }

            case op_type::cmpugt_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] > regs[opcode.v3];
                break;
            }

            case op_type::cmpuge_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] >= regs[opcode.v3];
                break;
            }


            // compare equality
            case op_type::cmpeq_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] == regs[opcode.v3];
                break;
            }

            case op_type::cmpne_reg:
            {
                regs[opcode.v1] = regs[opcode.v2] != regs[opcode.v3];
                break;
            }

            // system call
            case op_type::swi:
            {
                switch(opcode.v1)
                {
                    case 0x0: // exit
                    {
                        quit = true;
                        break;
                    }

                    default:
                    {
                        printf("unknown syscall: %x\n",opcode.v1);
                        break;
                    }
                }
                break;
            }

            // directives should not be hit at runtime..
            case op_type::push_arg:
            case op_type::clean_args:
            case op_type::free_slot_stack:
            case op_type::save_reg:
            case op_type::restore_reg:
            case op_type::END:
            {
                puts("directive not removed!?");
                exit(1);
            }

        }
    }

    this->program = nullptr;
    this->size = 0;

    printf("exit code: %d\n",regs[0]);
    return regs[0];
}

