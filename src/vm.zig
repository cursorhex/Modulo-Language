const std = @import("std");
const Bytecode = @import("bytecode.zig");
const CustomArrayList = @import("custom_array_list.zig").CustomArrayList;

const Value = union(enum) {
    Int: i64,
    Str: []const u8,
};

const Variable = struct {
    value: Value,
    is_const: bool,
    is_global: bool,
};

const ValueArrayList = CustomArrayList(Value);

// ANSI color codes
const RED = "\x1b[31m";
const RESET = "\x1b[0m";
const BOLD = "\x1b[1m";

pub fn run(bytecode: []const Bytecode.Instr, allocator: std.mem.Allocator) !void {
    var stack = ValueArrayList.init(allocator);
    defer stack.deinit();

    var variables = std.StringHashMap(Variable).init(allocator);
    defer variables.deinit();

    for (bytecode) |instr| {
        switch (instr.op) {
            .Const => {
                try stack.append(.{ .Int = instr.operand.Int });
            },
            .ConstStr => {
                try stack.append(.{ .Str = instr.operand.Str });
            },
            .Add => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int + b.Int;
                try stack.append(.{ .Int = result });
            },
            .Sub => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int - b.Int;
                try stack.append(.{ .Int = result });
            },
            .Mul => {
                const b = stack.pop();
                const a = stack.pop();
                const result = a.Int * b.Int;
                try stack.append(.{ .Int = result });
            },
            .Div => {
                const b = stack.pop();
                const a = stack.pop();
                const result = @divTrunc(a.Int, b.Int);
                try stack.append(.{ .Int = result });
            },
            .Get => {
                const name = instr.operand.Str;
                if (variables.get(name)) |variable| {
                    try stack.append(variable.value);
                } else {
                    std.debug.print("{s}{s}Error:{s} Variable '{s}' not found\n", .{ BOLD, RED, RESET, name });
                    return error.VariableNotFound;
                }
            },
            .SetVar => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try variables.put(name, .{ .value = value, .is_const = false, .is_global = false });
            },
            .SetConst => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try variables.put(name, .{ .value = value, .is_const = true, .is_global = false });
            },
            .SetVarGlobal => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try variables.put(name, .{ .value = value, .is_const = false, .is_global = true });
            },
            .SetConstGlobal => {
                const value = stack.pop();
                const name = instr.operand.Str;
                try variables.put(name, .{ .value = value, .is_const = true, .is_global = true });
            },
            .Mutate => {
                const value = stack.pop();
                const name = instr.operand.Str;

                if (variables.get(name)) |variable| {
                    if (variable.is_const) {
                        std.debug.print("   {s}{s}Error: Cannot mutate constant '{s}' {s}\n", .{ BOLD, RED, name, RESET });
                        return error.CannotMutateConstant;
                    }
                    try variables.put(name, .{ .value = value, .is_const = false, .is_global = variable.is_global });
                } else {
                    std.debug.print("{s}{s}Error:{s} Variable '{s}' not found\n", .{ BOLD, RED, RESET, name });
                    return error.VariableNotFound;
                }
            },
            .Increment => {
                const val = stack.pop();
                const result = val.Int + instr.operand.Int;
                try stack.append(.{ .Int = result });
            },
            .Call => {
                const arg = stack.pop();
                const call_name = instr.operand.Str;

                if (std.mem.eql(u8, call_name, "io.print")) {
                    switch (arg) {
                        .Int => |i| std.debug.print("{d}\n", .{i}),
                        .Str => |s| std.debug.print("{s}\n", .{s}),
                    }
                }
            },
        }
    }
}
