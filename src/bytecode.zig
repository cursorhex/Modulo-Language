pub const Opcode = enum(u8) {
    Const,
    ConstStr,
    Add,
    Sub,
    Mul,
    Div,
    Get,
    SetVar,
    SetConst,
    SetVarGlobal,
    SetConstGlobal,
    Mutate,
    Increment,
    Call,
};

pub const Instr = struct {
    op: Opcode,
    operand: union(enum) {
        Int: i64,
        Str: []const u8,
    } = .{ .Int = 0 },
};
