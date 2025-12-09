const std = @import("std");

const lex = @import("lexer.zig").lex;
const parser = @import("parser.zig");
const codegen = @import("codegen.zig");
const run = @import("vm.zig").run;
const Ast = @import("ast.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var input_file: ?[]const u8 = null;
    var bytecode_file: ?[]const u8 = null;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--i")) {
            i += 1;
            if (i < args.len) {
                input_file = args[i];
            }
        } else if (std.mem.eql(u8, args[i], "--bytecode")) {
            i += 1;
            if (i < args.len) {
                bytecode_file = args[i];
            }
        }
    }

    const default_src: []const u8 =
        \\section Main{{
        \\  var(x): 10
        \\  io.print (x)
        \\}}
        \\program.run [order: {Main}]
    ;

    var src: []u8 = undefined;
    if (input_file) |file_path| {
        var file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        const file_size = try file.getEndPos();
        src = try allocator.alloc(u8, file_size);
        _ = try file.read(src);
    } else {
        src = try allocator.alloc(u8, default_src.len);
        @memcpy(src, default_src);
    }
    defer allocator.free(src);

    const tokens = try lex(src, allocator);
    defer allocator.free(tokens);

    const program = try parser.parseProgram(tokens, allocator);
    defer freeProgram(program, allocator);

    var sections_map = std.StringHashMap([]*Ast.Stmt).init(allocator);
    defer sections_map.deinit();

    for (program.sections) |section| {
        try sections_map.put(section.name, section.statements);
    }

    // ESEGUI LOOSE STATEMENTS PRIMA (se ce ne sono)
    if (program.loose_statements.len > 0) {
        const bytecode = try codegen.codegen(program.loose_statements, allocator);
        defer codegen.freeBytecode(bytecode, allocator);

        run(bytecode, allocator) catch |err| {
            switch (err) {
                error.CannotMutateConstant, error.VariableNotFound => std.process.exit(1),
                else => return err,
            }
        };
    }

    // POI esegui sections in ordine (se c'è program.run)
    if (program.program_run) |prog_run| {
        for (prog_run.order) |section_name| {
            if (sections_map.get(section_name)) |statements| {
                const bytecode = try codegen.codegen(statements, allocator);
                defer codegen.freeBytecode(bytecode, allocator);

                if (bytecode_file) |file_path| {
                    var file = try std.fs.cwd().createFile(file_path, .{});
                    defer file.close();
                    for (bytecode) |instr| {
                        const str = try std.fmt.allocPrint(allocator, "{any}\n", .{instr});
                        defer allocator.free(str);
                        try file.writeAll(str);
                    }
                }

                run(bytecode, allocator) catch |err| {
                    switch (err) {
                        error.CannotMutateConstant, error.VariableNotFound => std.process.exit(1),
                        else => return err,
                    }
                };
            } else {
                std.debug.print("Error: Section '{s}' not found\n", .{section_name});
                return error.SectionNotFound;
            }
        }
    }
}

fn freeProgram(program: Ast.Program, allocator: std.mem.Allocator) void {
    // Free sections
    for (program.sections) |section| {
        for (section.statements) |stmt| {
            parser.freeStmt(stmt, allocator);
        }
        allocator.free(section.statements);
        allocator.destroy(section);
    }
    allocator.free(program.sections);

    // NUOVO: Free loose statements
    for (program.loose_statements) |stmt| {
        parser.freeStmt(stmt, allocator);
    }
    allocator.free(program.loose_statements);

    // Free program_run
    if (program.program_run) |prog_run| {
        allocator.free(prog_run.order);
        allocator.destroy(prog_run);
    }
}
