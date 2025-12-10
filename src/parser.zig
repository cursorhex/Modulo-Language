const std = @import("std");
const TokenType = @import("lexer.zig").TokenType;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const StmtArrayList = @import("custom_array_list.zig").CustomArrayList(*Ast.Stmt);

const ParseError = error{
    UnsupportedStatement,
    ExpectedNumber,
    ExpectedExpression,
    SyntaxError,
    InvalidVarDeclaration,
    InvalidConstDeclaration,
    InvalidCallExpression,
    InvalidBytecodeBlock,
    UnterminatedString,
    OutOfMemory,
    InvalidCharacter,
    Overflow,
};

pub fn parse(tokens_slice: []const Token, allocator: std.mem.Allocator) ParseError![]*Ast.Stmt {
    var p = Parser.init(tokens_slice, allocator);
    return try p.parse();
}

pub fn parseProgram(tokens_slice: []const Token, allocator: std.mem.Allocator) ParseError!Ast.Program {
    var p = Parser.init(tokens_slice, allocator);
    return try p.parseFullProgram();
}

pub fn freeAst(stmts: []*Ast.Stmt, allocator: std.mem.Allocator) void {
    for (stmts) |stmt| {
        freeStmt(stmt, allocator);
    }
    allocator.free(stmts);
}

pub fn freeStmt(stmt: *Ast.Stmt, allocator: std.mem.Allocator) void {
    switch (stmt.*) {
        .VarDecl => |var_decl| {
            freeExpr(var_decl.value, allocator);
        },
        .Mutation => |mutation| {
            freeExpr(mutation.value, allocator);
        },
        .ExprStmt => |expr| freeExpr(expr, allocator),
        .BytecodeExec => |block| {
            allocator.free(block.data);
        },
        .If => |if_stmt| {
            freeExpr(if_stmt.condition, allocator);
            for (if_stmt.then_block) |s| {
                freeStmt(s, allocator);
            }
            for (if_stmt.elseif_branches) |branch| {
                freeExpr(branch.condition, allocator);
                for (branch.block) |s| {
                    freeStmt(s, allocator);
                }
            }
            allocator.free(if_stmt.elseif_branches);
            for (if_stmt.else_block) |s| {
                freeStmt(s, allocator);
            }
            allocator.free(if_stmt.then_block);
            allocator.free(if_stmt.else_block);
        },
    }
    allocator.destroy(stmt);
}

fn freeExpr(expr: *Ast.Expr, allocator: std.mem.Allocator) void {
    switch (expr.*) {
        .Number, .String, .Var => {},
        .Add, .Sub, .Mul, .Div, .Concat => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Increment => |inner| {
            freeExpr(inner, allocator);
        },
        .Call => |call_expr| {
            freeExpr(call_expr.argument, allocator);
        },
        .Comparison => |comp| {
            freeExpr(comp.left, allocator);
            freeExpr(comp.right, allocator);
        },
        .Choose => |arms| {
            for (arms) |arm| {
                freeExpr(arm.value, allocator);
            }
            allocator.free(arms);
        },
    }
    allocator.destroy(expr);
}

const Parser = struct {
    tokens: []const Token,
    allocator: std.mem.Allocator,
    current: usize,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .allocator = allocator,
            .current = 0,
        };
    }

    fn parseSection(self: *Parser) ParseError!*Ast.Section {
        try self.consume(.Section);
        try self.consume(.Identifier);
        const section_name = self.previous().text;

        try self.consume(.LBrace);
        try self.consume(.LBrace);

        var statements = StmtArrayList.init(self.allocator);
        defer statements.deinit();

        while (!self.check(.RBrace)) {
            try statements.append(try self.parseStatement());
        }

        try self.consume(.RBrace);
        try self.consume(.RBrace);

        const section = try self.allocator.create(Ast.Section);
        section.* = .{
            .name = section_name,
            .statements = try statements.toOwnedSlice(),
        };
        return section;
    }

    fn parseProgramRun(self: *Parser) ParseError!*Ast.ProgramRun {
        try self.consume(.Program);
        try self.consume(.Dot);
        try self.consume(.Run);
        try self.consume(.LBracket);

        const StringArrayList = @import("custom_array_list.zig").CustomArrayList([]const u8);
        var order_names = StringArrayList.init(self.allocator);
        defer order_names.deinit();

        var config = Ast.ProgramConfig{};

        while (!self.check(.RBracket)) {
            if (self.check(.Order)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.LBrace);

                while (!self.check(.RBrace)) {
                    if (self.check(.Identifier)) {
                        _ = self.advance();
                        try order_names.append(self.previous().text);

                        if (self.match(.{.Comma})) {
                            continue;
                        }
                    } else {
                        return error.SyntaxError;
                    }
                }

                try self.consume(.RBrace);
            } else if (self.check(.Mode)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Debug})) {
                    config.mode = .Debug;
                } else if (self.match(.{.Release})) {
                    config.mode = .Release;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Optimize)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Speed})) {
                    config.optimize = .Speed;
                } else if (self.match(.{.Size})) {
                    config.optimize = .Size;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Repeat)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.Number);
                const repeat_val = try std.fmt.parseInt(i64, self.previous().text, 10);
                config.repeat = repeat_val;
            } else if (self.check(.Parallel)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.True})) {
                    config.parallel = true;
                } else if (self.match(.{.False})) {
                    config.parallel = false;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Timeout)) {
                _ = self.advance();
                try self.consume(.Colon);
                try self.consume(.Number);
                const timeout_val = try std.fmt.parseInt(i64, self.previous().text, 10);
                config.timeout = timeout_val;
            } else if (self.check(.OnError)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.Continue})) {
                    config.on_error = .Continue;
                } else if (self.match(.{.Stop})) {
                    config.on_error = .Stop;
                } else {
                    return error.SyntaxError;
                }
            } else if (self.check(.Trace)) {
                _ = self.advance();
                try self.consume(.Colon);

                if (self.match(.{.True})) {
                    config.trace = true;
                } else if (self.match(.{.False})) {
                    config.trace = false;
                } else {
                    return error.SyntaxError;
                }
            } else {
                return error.SyntaxError;
            }

            _ = self.match(.{.Comma});
        }

        try self.consume(.RBracket);

        const program_run = try self.allocator.create(Ast.ProgramRun);
        program_run.* = .{
            .order = try order_names.toOwnedSlice(),
            .config = config,
        };
        return program_run;
    }

    fn parseFullProgram(self: *Parser) ParseError!Ast.Program {
        const SectionArrayList = @import("custom_array_list.zig").CustomArrayList(*Ast.Section);
        var sections = SectionArrayList.init(self.allocator);
        defer sections.deinit();

        var loose_statements = StmtArrayList.init(self.allocator);
        defer loose_statements.deinit();

        var program_run: ?*Ast.ProgramRun = null;
        var program_set: ?*Ast.ProgramSet = null;

        while (!self.isAtEnd()) {
            if (self.check(.Section)) {
                try sections.append(try self.parseSection());
            } else if (self.check(.Program)) {
                // Guarda avanti per vedere se è .run o .set
                const saved = self.current;
                _ = self.advance(); // program
                _ = self.advance(); // dot

                if (self.check(.Run)) {
                    self.current = saved;
                    program_run = try self.parseProgramRun();
                } else if (self.check(.Set)) {
                    self.current = saved;
                    program_set = try self.parseProgramSet();
                } else {
                    return error.SyntaxError;
                }
            } else {
                try loose_statements.append(try self.parseStatement());
            }
        }

        return Ast.Program{
            .sections = try sections.toOwnedSlice(),
            .loose_statements = try loose_statements.toOwnedSlice(),
            .program_run = program_run,
            .program_set = program_set, // ← nuovo
        };
    }

    pub fn parse(self: *Parser) ParseError![]*Ast.Stmt {
        var statements = StmtArrayList.init(self.allocator);
        defer statements.deinit();

        while (!self.isAtEnd()) {
            try statements.append(try self.parseStatement());
        }
        return statements.toOwnedSlice();
    }

    fn parseStatement(self: *Parser) ParseError!*Ast.Stmt {
        if (self.check(.Bytecode)) {
            _ = self.advance();
            try self.consume(.String);
            const hex_str = self.previous().text;
            const data = try hexToBytes(hex_str, self.allocator);

            const stmt = try self.allocator.create(Ast.Stmt);
            stmt.* = .{ .BytecodeExec = .{ .data = data } };
            return stmt;
        }

        if (self.match(.{ .Var, .VarGlobal })) {
            const is_global = self.previous().t == .VarGlobal;
            return self.parseVarDeclaration(is_global);
        }

        if (self.match(.{ .Const, .ConstGlobal })) {
            const is_global = self.previous().t == .ConstGlobal;
            return self.parseConstDeclaration(is_global);
        }

        // if statement
        if (self.check(.If)) {
            return self.parseIfStatement();
        }

        if (self.check(.LParen)) {
            const saved_pos = self.current;
            _ = self.advance();
            if (self.check(.Identifier)) {
                _ = self.advance();
                if (self.check(.RParen)) {
                    _ = self.advance();
                    if (self.check(.Colon)) {
                        self.current = saved_pos;
                        return self.parseMutation();
                    }
                }
            }
            self.current = saved_pos;
        }

        const expr = try self.parseExpression();
        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .ExprStmt = expr };
        return stmt;
    }

    fn parseIfStatement(self: *Parser) ParseError!*Ast.Stmt {
        try self.consume(.If);

        const condition = try self.parseComparison();

        try self.consume(.LBrace);

        var then_stmts = StmtArrayList.init(self.allocator);
        defer then_stmts.deinit();
        while (!self.check(.RBrace)) {
            try then_stmts.append(try self.parseStatement());
        }
        try self.consume(.RBrace);

        const ElseIfArrayList = @import("custom_array_list.zig").CustomArrayList(Ast.ElseIfBranch);
        var elseif_branches = ElseIfArrayList.init(self.allocator);
        defer elseif_branches.deinit();

        while (self.match(.{.ElseIf})) {
            const elif_cond = try self.parseComparison();
            try self.consume(.LBrace);

            var elif_stmts = StmtArrayList.init(self.allocator);
            defer elif_stmts.deinit();
            while (!self.check(.RBrace)) {
                try elif_stmts.append(try self.parseStatement());
            }
            try self.consume(.RBrace);

            try elseif_branches.append(.{
                .condition = elif_cond,
                .block = try elif_stmts.toOwnedSlice(),
            });
        }

        var else_stmts = StmtArrayList.init(self.allocator);
        defer else_stmts.deinit();

        if (self.match(.{.Else})) {
            try self.consume(.LBrace);
            while (!self.check(.RBrace)) {
                try else_stmts.append(try self.parseStatement());
            }
            try self.consume(.RBrace);
        }

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{
            .If = .{
                .condition = condition,
                .then_block = try then_stmts.toOwnedSlice(),
                .elseif_branches = try elseif_branches.toOwnedSlice(),
                .else_block = try else_stmts.toOwnedSlice(),
            },
        };
        return stmt;
    }

    fn parseComparison(self: *Parser) ParseError!*Ast.Expr {
        const left = try self.parseConcatenation();

        if (self.match(.{ .EqualEqual, .NotEqual, .Greater, .Less, .GreaterEq, .LessEq })) {
            const op_tok = self.previous();
            const op: Ast.ComparisonOp = switch (op_tok.t) {
                .EqualEqual => .Equal,
                .NotEqual => .NotEqual,
                .Greater => .Greater,
                .Less => .Less,
                .GreaterEq => .GreaterEq,
                .LessEq => .LessEq,
                else => unreachable,
            };

            const right = try self.parseConcatenation();

            const comp = try self.allocator.create(Ast.Expr);
            comp.* = .{
                .Comparison = .{
                    .left = left,
                    .op = op,
                    .right = right,
                },
            };
            return comp;
        }

        return left;
    }

    fn hexToBytes(hex: []const u8, allocator: std.mem.Allocator) ![]u8 {
        if (hex.len % 2 != 0) return error.InvalidCharacter;

        const len = hex.len / 2;
        var result = try allocator.alloc(u8, len);

        for (0..len) |i| {
            const hi = try std.fmt.charToDigit(hex[i * 2], 16);
            const lo = try std.fmt.charToDigit(hex[i * 2 + 1], 16);
            result[i] = (hi << 4) | lo;
        }

        return result;
    }

    fn parseVarDeclaration(self: *Parser, is_global: bool) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .VarDecl = .{ .name = name_token.text, .value = value, .is_const = false, .is_global = is_global } };
        return stmt;
    }

    fn parseConstDeclaration(self: *Parser, is_global: bool) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .VarDecl = .{ .name = name_token.text, .value = value, .is_const = true, .is_global = is_global } };
        return stmt;
    }

    fn parseMutation(self: *Parser) ParseError!*Ast.Stmt {
        try self.consume(.LParen);
        try self.consume(.Identifier);
        const name_token = self.previous();
        try self.consume(.RParen);
        try self.consume(.Colon);
        const value = try self.parseExpression();

        const stmt = try self.allocator.create(Ast.Stmt);
        stmt.* = .{ .Mutation = .{ .name = name_token.text, .value = value } };
        return stmt;
    }

    fn parseExpression(self: *Parser) ParseError!*Ast.Expr {
        return self.parseConcatenation();
    }

    fn parseConcatenation(self: *Parser) ParseError!*Ast.Expr {
        var expr = try self.parseAddition();

        while (self.match(.{.DotDot})) {
            const right = try self.parseAddition();
            const concat = try self.allocator.create(Ast.Expr);
            concat.* = .{ .Concat = .{ .left = expr, .right = right } };
            expr = concat;
        }

        return expr;
    }

    fn parseAddition(self: *Parser) ParseError!*Ast.Expr {
        var expr = try self.parseMultiplication();

        while (self.match(.{ .Plus, .Minus })) {
            const op_token = self.previous();
            const right = try self.parseMultiplication();
            const new_expr = try self.allocator.create(Ast.Expr);
            new_expr.* = switch (op_token.t) {
                .Plus => .{ .Add = .{ .left = expr, .right = right } },
                .Minus => .{ .Sub = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = new_expr;
        }
        return expr;
    }

    fn parseMultiplication(self: *Parser) ParseError!*Ast.Expr {
        var expr = try self.parsePrimary();

        while (self.match(.{ .Star, .Slash })) {
            const op_token = self.previous();
            const right = try self.parsePrimary();
            const new_expr = try self.allocator.create(Ast.Expr);
            new_expr.* = switch (op_token.t) {
                .Star => .{ .Mul = .{ .left = expr, .right = right } },
                .Slash => .{ .Div = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = new_expr;
        }
        return expr;
    }

    fn parseProgramSet(self: *Parser) ParseError!*Ast.ProgramSet {
        try self.consume(.Program);
        try self.consume(.Dot);
        try self.consume(.Set);
        try self.consume(.LBracket);

        var prog_set = Ast.ProgramSet{};

        const StringArrayList = @import("custom_array_list.zig").CustomArrayList([]const u8);

        while (!self.check(.RBracket)) {
            if (self.check(.Identifier)) {
                const key = self.advance().text;
                try self.consume(.Colon);

                if (std.mem.eql(u8, key, "name")) {
                    try self.consume(.String);
                    prog_set.name = self.previous().text;
                } else if (std.mem.eql(u8, key, "author")) {
                    try self.consume(.LBrace);
                    var authors = StringArrayList.init(self.allocator);
                    defer authors.deinit();

                    while (!self.check(.RBrace)) {
                        try self.consume(.String);
                        try authors.append(self.previous().text);
                        _ = self.match(.{.Comma});
                    }
                    try self.consume(.RBrace);
                    prog_set.authors = try authors.toOwnedSlice();
                } else if (std.mem.eql(u8, key, "version")) {
                    try self.consume(.LBrace);

                    while (!self.check(.RBrace)) {
                        // ❌ Questo fallisce se 'debug' è una keyword
                        // try self.consume(.Identifier);

                        // ✅ Fix: accetta Identifier O controlla il testo corrente
                        if (!self.check(.Identifier)) {
                            // Se non è Identifier, potrebbe essere una keyword usata come chiave
                            // Accetta qualsiasi token e prendi il testo
                        }
                        const ver_key = self.advance().text; // Prendi qualsiasi token

                        try self.consume(.Colon);
                        try self.consume(.String);
                        const ver_val = self.previous().text;

                        if (std.mem.eql(u8, ver_key, "debug")) {
                            prog_set.version_debug = ver_val;
                        } else if (std.mem.eql(u8, ver_key, "release")) {
                            prog_set.version_release = ver_val;
                        }

                        _ = self.match(.{.Comma});
                    }
                    try self.consume(.RBrace);
                } else if (std.mem.eql(u8, key, "description")) {
                    try self.consume(.String);
                    prog_set.description = self.previous().text;
                } else if (std.mem.eql(u8, key, "license")) {
                    try self.consume(.String);
                    prog_set.license = self.previous().text;
                } else if (std.mem.eql(u8, key, "homepage")) {
                    try self.consume(.String);
                    prog_set.homepage = self.previous().text;
                } else if (std.mem.eql(u8, key, "created")) {
                    try self.consume(.String);
                    prog_set.created = self.previous().text;
                } else if (std.mem.eql(u8, key, "tags")) {
                    try self.consume(.LBrace);
                    var tags = StringArrayList.init(self.allocator);
                    defer tags.deinit();

                    while (!self.check(.RBrace)) {
                        try self.consume(.String);
                        try tags.append(self.previous().text);
                        _ = self.match(.{.Comma});
                    }
                    try self.consume(.RBrace);
                    prog_set.tags = try tags.toOwnedSlice();
                }
            }

            _ = self.match(.{.Comma});
        }

        try self.consume(.RBracket);

        const set = try self.allocator.create(Ast.ProgramSet);
        set.* = prog_set;
        return set;
    }

    fn parsePrimary(self: *Parser) ParseError!*Ast.Expr {
        if (self.match(.{.Number})) {
            const token = self.previous();
            const value = try std.fmt.parseInt(i64, token.text, 10);
            const expr = try self.allocator.create(Ast.Expr);
            expr.* = .{ .Number = value };
            return expr;
        }

        if (self.match(.{.String})) {
            const token = self.previous();
            const expr = try self.allocator.create(Ast.Expr);
            expr.* = .{ .String = token.text };
            return expr;
        }

        if (self.check(.Choose)) {
            return self.parseChoose();
        }

        if (self.check(.LParen)) {
            const saved_pos = self.current;
            _ = self.advance();

            // Accetta sia Identifier che Program keyword
            if (self.match(.{.Identifier}) or self.match(.{.Program})) {
                const token = self.previous();

                //std.debug.print("DEBUG parsePrimary: Token '{s}'\n", .{token.text});

                if (std.mem.eql(u8, token.text, "program") and self.check(.Dot)) {
                    const StringArrayList = @import("custom_array_list.zig").CustomArrayList([]const u8);
                    var path_parts = StringArrayList.init(self.allocator);
                    defer path_parts.deinit();

                    try path_parts.append("program");

                    while (self.match(.{.Dot})) {
                        if (self.isAtEnd()) {
                            return error.ExpectedExpression;
                        }
                        const part_tok = self.advance();
                        try path_parts.append(part_tok.text);
                    }

                    try self.consume(.RParen);

                    const full_path = try std.mem.join(self.allocator, ".", path_parts.items);

                    const expr = try self.allocator.create(Ast.Expr);
                    expr.* = .{ .Var = full_path };
                    return expr;
                }

                try self.consume(.RParen);

                const expr = try self.allocator.create(Ast.Expr);
                expr.* = .{ .Var = token.text };
                return expr;
            }

            self.current = saved_pos;
            _ = self.advance();
            const expr = try self.parseExpression();
            try self.consume(.RParen);
            return expr;
        }

        if (self.peek().t == .Identifier and std.mem.eql(u8, self.peek().text, "io")) {
            _ = self.advance();
            try self.consume(.Dot);
            try self.consume(.Identifier);
            const func_name = self.previous().text;

            // io.input "placeholder"
            if (std.mem.eql(u8, func_name, "input")) {
                try self.consume(.String);
                const placeholder_text = self.previous().text;

                const placeholder_expr = try self.allocator.create(Ast.Expr);
                placeholder_expr.* = .{ .String = placeholder_text };

                const call_expr = try self.allocator.create(Ast.Expr);
                call_expr.* = .{
                    .Call = .{
                        .library = "io",
                        .function = "input",
                        .argument = placeholder_expr,
                        .type_hint = "",
                    },
                };
                return call_expr;
            }

            // io.print / io.warn / io.error
            var argument: *Ast.Expr = undefined;
            var type_hint: []const u8 = "";

            if (self.match(.{.As})) {
                try self.consume(.Identifier);
                type_hint = self.previous().text;

                if (self.check(.String)) {
                    _ = self.advance();
                    const str_expr = try self.allocator.create(Ast.Expr);
                    str_expr.* = .{ .String = self.previous().text };
                    argument = str_expr;
                } else if (self.check(.Number)) {
                    _ = self.advance();
                    const num = try std.fmt.parseInt(i64, self.previous().text, 10);
                    const num_expr = try self.allocator.create(Ast.Expr);
                    num_expr.* = .{ .Number = num };
                    argument = num_expr;
                } else if (self.match(.{.LParen})) {
                    argument = try self.parseExpression();
                    try self.consume(.RParen);
                } else {
                    return error.InvalidCallExpression;
                }
            } else if (self.match(.{.LParen})) {
                argument = try self.parseExpression();
                try self.consume(.RParen);
            } else {
                return error.InvalidCallExpression;
            }

            const call_expr = try self.allocator.create(Ast.Expr);
            call_expr.* = .{
                .Call = .{
                    .library = "io",
                    .function = func_name,
                    .argument = argument,
                    .type_hint = type_hint,
                },
            };
            return call_expr;
        }

        // Accetta sia Identifier che Program keyword (fuori da parentesi)
        if (self.match(.{.Identifier}) or self.match(.{.Program})) {
            const token = self.previous();

            // Se è 'program' e seguito da dot, costruisci il path
            if (std.mem.eql(u8, token.text, "program") and self.check(.Dot)) {
                const StringArrayList = @import("custom_array_list.zig").CustomArrayList([]const u8);
                var path_parts = StringArrayList.init(self.allocator);
                defer path_parts.deinit();

                try path_parts.append("program");

                while (self.match(.{.Dot})) {
                    if (self.isAtEnd()) {
                        return error.ExpectedExpression;
                    }
                    const part_tok = self.advance();
                    try path_parts.append(part_tok.text);
                }

                const full_path = try std.mem.join(self.allocator, ".", path_parts.items);

                const expr = try self.allocator.create(Ast.Expr);
                expr.* = .{ .Var = full_path };
                return expr;
            }

            const expr = try self.allocator.create(Ast.Expr);
            expr.* = .{ .Var = token.text };
            return expr;
        }

        return error.ExpectedExpression;
    }

    fn parseChoose(self: *Parser) ParseError!*Ast.Expr {
        try self.consume(.Choose);
        try self.consume(.LBrace);

        const ChoiceArmArrayList = @import("custom_array_list.zig").CustomArrayList(Ast.ChoiceArm);
        var arms = ChoiceArmArrayList.init(self.allocator);
        defer arms.deinit();

        while (!self.check(.RBrace)) {
            try self.consume(.Number);
            const weight = try std.fmt.parseInt(i64, self.previous().text, 10);

            try self.consume(.Arrow); // =>

            const value = try self.parseExpression();

            try arms.append(.{
                .weight = weight,
                .value = value,
            });

            // opzionale: consuma newline/comma se presente
            _ = self.match(.{.Comma});
        }

        try self.consume(.RBrace);

        const expr = try self.allocator.create(Ast.Expr);
        expr.* = .{ .Choose = try arms.toOwnedSlice() };
        return expr;
    }

    fn consume(self: *Parser, expected: TokenType) !void {
        if (self.peek().t != expected) {
            return error.SyntaxError;
        }
        self.current += 1;
    }

    fn match(self: *Parser, comptime types_tuple: anytype) bool {
        inline for (types_tuple) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().t == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().t == .Eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
};
