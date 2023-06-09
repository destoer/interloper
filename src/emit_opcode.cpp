

void add(Function& func,DstSlot dst, SrcSlot v1, SrcSlot v2)
{
    emit_reg3<op_type::add_reg>(func,dst,v1,v2);
}


void call(Function& func, LabelSlot slot)
{
    emit_label1<op_type::call>(func,slot);
}