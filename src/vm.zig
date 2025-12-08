const std = @import("std");
const Bytecode = @import("bytecode.zig");

pub fn run(code: []const Bytecode.Instr) void {
    var stack: [256]i64 = undefined;
    var sp: usize = 0;

    for (code) |instr| {
        switch (instr.op) {
            .Const => {
                stack[sp] = instr.operand;
                sp += 1;
            },
            .Add => {
                sp -= 1;
                const b = stack[sp];
                sp -= 1;
                const a = stack[sp];
                stack[sp] = a + b;
                sp += 1;
            },
            .Sub => {
                sp -= 1;
                const b = stack[sp];
                sp -= 1;
                const a = stack[sp];
                stack[sp] = a - b;
                sp += 1;
            },
            .Mul => {
                sp -= 1;
                const b = stack[sp];
                sp -= 1;
                const a = stack[sp];
                stack[sp] = a * b;
                sp += 1;
            },
            .Div => {
                sp -= 1;
                const b = stack[sp];
                sp -= 1;
                const a = stack[sp];
                stack[sp] = @divTrunc(a, b);
                sp += 1;
            },
            .Print => {
                std.debug.print("{}\n", .{stack[sp - 1]});
            },

        }
    }
}
