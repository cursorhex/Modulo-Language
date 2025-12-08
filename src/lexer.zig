const std = @import("std");
const TokenArrayList = @import("custom_array_list.zig").CustomArrayList(Token);

pub const TokenType = enum {
    Identifier,
    Number,
    Plus, Minus, Star, Slash,
    Equal,
    Let,
    Print,
    Eof,
};

pub const Token = struct {
    t: TokenType,
    text: []const u8,
};

pub fn lex(src: []const u8, allocator: std.mem.Allocator) ![]Token {
    var tokens = TokenArrayList.init(allocator);
    defer tokens.deinit();

    var i: usize = 0;
    while (i < src.len) : (i += 1) {
        switch (src[i]) {
            ' ', '\n', '\t' => continue,

            '+' => try tokens.append(.{ .t = .Plus, .text = src[i..i+1] }),
            '-' => try tokens.append(.{ .t = .Minus, .text = src[i..i+1] }),
            '*' => try tokens.append(.{ .t = .Star, .text = src[i..i+1] }),
            '/' => try tokens.append(.{ .t = .Slash, .text = src[i..i+1] }),
            '=' => try tokens.append(.{ .t = .Equal, .text = src[i..i+1] }),

            else => |c| {
                if (std.ascii.isDigit(c)) {
                    const start = i;
                    while (i < src.len and std.ascii.isDigit(src[i])) : (i += 1) {}
                    try tokens.append(.{
                        .t = .Number,
                        .text = src[start .. i],
                    });
                    i -= 1;
                } else if (std.ascii.isAlphabetic(c)) {
                    const start = i;
                    while (i < src.len and std.ascii.isAlphabetic(src[i])) : (i += 1) {}
                    const word = src[start..i];
                    const tok = if (std.mem.eql(u8, word, "let")) TokenType.Let
                        else if (std.mem.eql(u8, word, "print")) TokenType.Print
                        else TokenType.Identifier;
                    try tokens.append(.{ .t = tok, .text = word });
                    i -= 1;
                }
            },
        }
    }

    try tokens.append(.{ .t = .Eof, .text = "" });

    return tokens.toOwnedSlice();
}