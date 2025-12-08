pub const Expr = union(enum) {
    Number: i64,
    Var: []const u8,
    Add: struct { left: *Expr, right: *Expr },
    Sub: struct { left: *Expr, right: *Expr },
    Mul: struct { left: *Expr, right: *Expr },
    Div: struct { left: *Expr, right: *Expr },
};

pub const Stmt = union(enum) {
    Let: struct { name: []const u8, value: *Expr },
    Print: *Expr,
};
