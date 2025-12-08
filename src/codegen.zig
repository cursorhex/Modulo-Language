const std = @import("std");
const Bytecode = @import("bytecode.zig");
const Ast = @import("ast.zig");
const InstrArrayList = @import("custom_array_list.zig").CustomArrayList(Bytecode.Instr);

pub fn codegen(stmts: []*Ast.Stmt, allocator: std.mem.Allocator) ![]Bytecode.Instr {
    var instructions = InstrArrayList.init(allocator);
    defer instructions.deinit();

    for (stmts) |stmt| {
        try genStmt(stmt, &instructions);
    }

    return instructions.toOwnedSlice();
}

pub fn genExpr(expr: *Ast.Expr, out: *InstrArrayList) !void {
    switch (expr.*) {
        .Number => |n| try out.append(.{ .op = .Const, .operand = n }),
        .Var => |_| @panic("no vars yet"),
        .Add => |a| {
            try genExpr(a.left, out);
            try genExpr(a.right, out);
            try out.append(.{ .op = .Add, .operand = 0 });
        },
        .Sub => |s| {
            try genExpr(s.left, out);
            try genExpr(s.right, out);
            try out.append(.{ .op = .Sub, .operand = 0 });
        },
        .Mul => |m| {
            try genExpr(m.left, out);
            try genExpr(m.right, out);
            try out.append(.{ .op = .Mul, .operand = 0 });
        },
        .Div => |d| {
            try genExpr(d.left, out);
            try genExpr(d.right, out);
            try out.append(.{ .op = .Div, .operand = 0 });
        },
    }
}

pub fn genStmt(stmt: *Ast.Stmt, out: *InstrArrayList) !void {
    switch (stmt.*) {
        .Print => |expr| {
            try genExpr(expr, out);
            try out.append(.{ .op = .Print, .operand = 0 });
        },
        .Let => @panic("todo let"),
    }
}