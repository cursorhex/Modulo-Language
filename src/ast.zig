pub const Expr = union(enum) {
    Number: i64,
    String: []const u8,
    Var: []const u8,
    Add: struct { left: *Expr, right: *Expr },
    Sub: struct { left: *Expr, right: *Expr },
    Mul: struct { left: *Expr, right: *Expr },
    Div: struct { left: *Expr, right: *Expr },
    Increment: *Expr,
    Call: struct { library: []const u8, function: []const u8, argument: *Expr },
};

pub const Stmt = union(enum) {
    VarDecl: struct { name: []const u8, value: *Expr, is_global: bool },
    ConstDecl: struct { name: []const u8, value: *Expr, is_global: bool },
    Mutation: struct { name: []const u8, value: *Expr },
    Expression: *Expr,
};

pub const Section = struct {
    name: []const u8,
    statements: []*Stmt,
};

pub const ProgramRun = struct {
    order: [][]const u8, // Array of section names
};

pub const Program = struct {
    sections: []*Section,
    loose_statements: []*Stmt, // NUOVO: codice fuori dalle sezioni
    program_run: ?*ProgramRun,
};
