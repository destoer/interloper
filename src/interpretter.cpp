#include <interloper.h>


template<typename access_type>
access_type Interpretter::read_mem(uint32_t addr)
{
    // force algin access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + stack.size())
    {
        return handle_read<access_type>(stack.data(),addr - 0x20000000);
    }

    else
    {
        printf("warning out of bounds read at %x\n",addr);
        return 0;
    }
}

template<typename access_type>
void Interpretter::write_mem(uint32_t addr, access_type v)
{
    // force algin access
    addr &= ~(sizeof(access_type) - 1);

    if(addr >= 0x20000000 && addr < 0x20000000 + stack.size())
    {
        handle_write<access_type>(stack.data(),addr - 0x20000000,v);
    }

    else
    {
        printf("warning out of bounds write at %x:%x\n",addr,v);
    }
}


void Interpretter::run(const uint8_t *program, uint32_t size)
{
    this->program = program;
    this->size = size;

    assert(program);

    stack.resize(16 * 1024 * 1024);

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


            case op_type::lw:
            {
                regs[opcode.v1] = read_mem<uint32_t>(regs[opcode.v2]+opcode.v3);
                break;
            }


            case op_type::sw:
            {
                write_mem<uint32_t>(regs[opcode.v2]+opcode.v3,regs[opcode.v1]);
                break;
            }

            // hard code as program exit for now
            // we will actually have to impl this when we add function calls
            case op_type::ret:
            {
                quit = true;
                break;
            }

        }

    }

    printf("exit code: %d\n",regs[0]);
}

