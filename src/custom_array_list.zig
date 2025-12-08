const std = @import("std");

pub fn CustomArrayList(comptime T: type) type {
    return struct {
        const Self = @This();

        items: []T,
        len: usize,
        capacity: usize,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .items = &[_]T{},
                .len = 0,
                .capacity = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.items);
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.len >= self.capacity) {
                const new_capacity = if (self.capacity == 0) 8 else self.capacity * 2;
                const new_items = try self.allocator.alloc(T, new_capacity);
                @memcpy(new_items[0..self.len], self.items);
                self.allocator.free(self.items);
                self.items = new_items;
                self.capacity = new_capacity;
            }
            self.items[self.len] = item;
            self.len += 1;
        }

        pub fn toOwnedSlice(self: *Self) ![]T {
            const result = try self.allocator.alloc(T, self.len);
            @memcpy(result, self.items[0..self.len]);
            return result;
        }
    };
}
