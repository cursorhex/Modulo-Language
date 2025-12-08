pub const Opcode = enum(u8) {
    Const,   // push costante
    Add,     // add top 2 stack values
    Sub,
    Mul,
    Div,
    Print,   // print top of stack
};
pub const Instr = struct {
    op: Opcode,
    operand: i64 = 0,
};
