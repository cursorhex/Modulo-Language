pub const Expr = union(enum) {
    Number: i64,
    String: []const u8,
    Var: []const u8,
    Add: struct { left: *Expr, right: *Expr },
    Sub: struct { left: *Expr, right: *Expr },
    Mul: struct { left: *Expr, right: *Expr },
    Div: struct { left: *Expr, right: *Expr },
    Increment: *Expr, // (y)++
    Call: struct { library: []const u8, function: []const u8, argument: *Expr },
};

pub const Stmt = union(enum) {
    VarDecl: struct { name: []const u8, value: *Expr, is_global: bool },
    ConstDecl: struct { name: []const u8, value: *Expr, is_global: bool },
    Mutation: struct { name: []const u8, value: *Expr }, // (y): expr
    Expression: *Expr,
};
