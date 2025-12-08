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
        .Number => |n| try out.append(.{ .op = .Const, .operand = .{ .Int = n } }),
        .String => |s| try out.append(.{ .op = .ConstStr, .operand = .{ .Str = s } }),
        .Var => |v| try out.append(.{ .op = .Get, .operand = .{ .Str = v } }),
        .Add => |a| {
            try genExpr(a.left, out);
            try genExpr(a.right, out);
            try out.append(.{ .op = .Add, .operand = .{ .Int = 0 } });
        },
        .Sub => |s| {
            try genExpr(s.left, out);
            try genExpr(s.right, out);
            try out.append(.{ .op = .Sub, .operand = .{ .Int = 0 } });
        },
        .Mul => |m| {
            try genExpr(m.left, out);
            try genExpr(m.right, out);
            try out.append(.{ .op = .Mul, .operand = .{ .Int = 0 } });
        },
        .Div => |d| {
            try genExpr(d.left, out);
            try genExpr(d.right, out);
            try out.append(.{ .op = .Div, .operand = .{ .Int = 0 } });
        },
        .Increment => |inner| {
            try genExpr(inner, out);
            try out.append(.{ .op = .Increment, .operand = .{ .Int = 1 } });
        },
        .Call => |call_expr| {
            try genExpr(call_expr.argument, out);
            const call_name = try std.fmt.allocPrint(out.allocator, "{s}.{s}", .{ call_expr.library, call_expr.function });
            try out.append(.{ .op = .Call, .operand = .{ .Str = call_name } });
        },
    }
}

pub fn genStmt(stmt: *Ast.Stmt, out: *InstrArrayList) !void {
    switch (stmt.*) {
        .VarDecl => |var_decl| {
            try genExpr(var_decl.value, out);
            if (var_decl.is_global) {
                try out.append(.{ .op = .SetVarGlobal, .operand = .{ .Str = var_decl.name } });
            } else {
                try out.append(.{ .op = .SetVar, .operand = .{ .Str = var_decl.name } });
            }
        },
        .ConstDecl => |const_decl| {
            try genExpr(const_decl.value, out);
            if (const_decl.is_global) {
                try out.append(.{ .op = .SetConstGlobal, .operand = .{ .Str = const_decl.name } });
            } else {
                try out.append(.{ .op = .SetConst, .operand = .{ .Str = const_decl.name } });
            }
        },
        .Mutation => |mutation| {
            try genExpr(mutation.value, out);
            try out.append(.{ .op = .Mutate, .operand = .{ .Str = mutation.name } });
        },
        .Expression => |expr| {
            try genExpr(expr, out);
        },
    }
}

// Add this function to free bytecode instructions that contain allocated strings
pub fn freeBytecode(bytecode: []Bytecode.Instr, allocator: std.mem.Allocator) void {
    for (bytecode) |instr| {
        switch (instr.op) {
            .Call => {
                // Free the allocated call_name string
                allocator.free(instr.operand.Str);
            },
            else => {},
        }
    }
    allocator.free(bytecode);
}
