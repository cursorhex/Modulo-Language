const std = @import("std");
const TokenType = @import("lexer.zig").TokenType;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const StmtArrayList = @import("custom_array_list.zig").CustomArrayList(*Ast.Stmt);

pub fn parse(tokens_slice: []const Token, allocator: std.mem.Allocator) ![]*Ast.Stmt {
    var p = Parser.init(tokens_slice, allocator);
    return try p.parse();
}

pub fn freeAst(stmts: []*Ast.Stmt, allocator: std.mem.Allocator) void {
    for (stmts) |stmt| {
        freeStmt(stmt, allocator);
    }
    allocator.free(stmts);
}

fn freeStmt(stmt: *Ast.Stmt, allocator: std.mem.Allocator) void {
    switch (stmt.*) {
        .Print => |expr| freeExpr(expr, allocator),
        .Let => |let_stmt| {
            // name is a slice of the original source, no need to free
            freeExpr(let_stmt.value, allocator);
        },
    }
    allocator.destroy(stmt);
}

fn freeExpr(expr: *Ast.Expr, allocator: std.mem.Allocator) void {
    switch (expr.*) {
        .Number, .Var => {},
        .Add => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Sub => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Mul => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
        },
        .Div => |bin_op| {
            freeExpr(bin_op.left, allocator);
            freeExpr(bin_op.right, allocator);
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

    pub fn parse(self: *Parser) ![]*Ast.Stmt {
        var statements = StmtArrayList.init(self.allocator);
        defer statements.deinit();

        while (!self.isAtEnd()) {
            try statements.append(try self.parseStatement());
        }
        return statements.toOwnedSlice();
    }

    fn parseStatement(self: *Parser) !*Ast.Stmt {
        if (self.match(.{.Print})) {
            return self.parsePrintStatement();
        }
        // For now, only Print statements are supported
        return error.UnsupportedStatement;
    }

    fn parsePrintStatement(self: *Parser) !*Ast.Stmt {
        const expr = try self.parseExpression();
        const stmt = try self.allocator.alloc(Ast.Stmt, 1);
        stmt[0] = .{ .Print = expr };
        return &stmt[0];
    }

    fn parseExpression(self: *Parser) !*Ast.Expr {
        return self.parseAddition();
    }

    fn parseAddition(self: *Parser) !*Ast.Expr {
        var expr = try self.parseMultiplication();

        while (self.match(.{.Plus, .Minus})) {
            const op_token = self.previous();
            const right = try self.parseMultiplication();
            const new_expr = try self.allocator.alloc(Ast.Expr, 1);
            new_expr[0] = switch (op_token.t) {
                .Plus => .{ .Add = .{ .left = expr, .right = right } },
                .Minus => .{ .Sub = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = &new_expr[0];
        }
        return expr;
    }

    fn parseMultiplication(self: *Parser) !*Ast.Expr {
        var expr = try self.parsePrimary();

        while (self.match(.{.Star, .Slash})) {
            const op_token = self.previous();
            const right = try self.parsePrimary();
            const new_expr = try self.allocator.alloc(Ast.Expr, 1);
            new_expr[0] = switch (op_token.t) {
                .Star => .{ .Mul = .{ .left = expr, .right = right } },
                .Slash => .{ .Div = .{ .left = expr, .right = right } },
                else => unreachable,
            };
            expr = &new_expr[0];
        }
        return expr;
    }

    fn parsePrimary(self: *Parser) !*Ast.Expr {
        if (self.match(.{.Number})) {
            const token = self.previous();
            const value = try std.fmt.parseInt(i64, token.text, 10);
            const expr = try self.allocator.alloc(Ast.Expr, 1);
            expr[0] = .{ .Number = value };
            return &expr[0];
        }

        // For now, only numbers are supported as primary expressions
        return error.ExpectedNumber;
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