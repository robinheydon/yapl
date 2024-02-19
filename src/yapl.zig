///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ValueType = enum(u8) {
    nil,
    boolean,
    integer,
    number,
    string,
};

///////////////////////////////////////////////////////////////////////////////////////////////

pub const Value = union(ValueType) {
    nil,
    boolean: bool,
    integer: i64,
    number: f64,
    string: String,

    pub fn add(lhs: Value, rhs: Value) Value {
        return Value{ .integer = lhs.integer + rhs.integer };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const String = packed struct {
    index: u32,
    len: u32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const YAPL = struct {
    allocator: std.mem.Allocator,

    stack: std.ArrayListUnmanaged(Value) = .{},
    top_of_stack: usize = 0,

    interned_strings: std.ArrayListUnmanaged(u8) = .{},

    pub fn init(allocator: std.mem.Allocator) !YAPL {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *YAPL) void {
        self.stack.deinit(self.allocator);
        self.interned_strings.deinit(self.allocator);
    }

    pub fn push_nil(self: *YAPL) !void {
        const val = Value{ .nil = {} };
        try self.stack.append(self.allocator, val);
    }

    pub fn push_boolean(self: *YAPL, value: bool) !void {
        const val = Value{ .boolean = value };
        try self.stack.append(self.allocator, val);
    }

    pub fn push_integer(self: *YAPL, value: i64) !void {
        const val = Value{ .integer = value };
        try self.stack.append(self.allocator, val);
    }

    pub fn push_number(self: *YAPL, value: f64) !void {
        const val = Value{ .number = value };
        try self.stack.append(self.allocator, val);
    }

    pub fn push_string (self: *YAPL, value: String) !void {
        const val = Value{ .string = value };
        try self.stack.append(self.allocator, val);
    }

    pub fn push_tos (self: *YAPL) usize {
        const old_tos = self.top_of_stack;
        self.top_of_stack = self.stack.items.len;
        return old_tos;
    }

    pub fn pop_tos (self: *YAPL, old_tos: usize) void {
        self.top_of_stack = old_tos;
    }

    pub fn add(self: *YAPL) !void {
        if (self.stack.items.len < 2) {
            return error.StackUnderflow;
        }

        const lhs = self.stack.items[self.stack.items.len - 2];
        const rhs = self.stack.items[self.stack.items.len - 1];
        self.stack.items.len -= 1;

        const result = lhs.add(rhs);
        self.stack.items[self.stack.items.len - 1] = result;
    }

    pub fn sub(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn mul(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn div(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn idiv(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn mod(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn negate(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn left_shift(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn right_shift(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn bitwise_not(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn bitwise_and(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn bitwise_or(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn bitwise_xor(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    pub fn to_integer(self: *YAPL, index: isize) !i64 {
        if (index >= 0) {
            const uindex: usize = @intCast(index);
            if (self.top_of_stack + uindex >= self.stack.items.len) {
                return error.StackOffsetInvalid;
            }
            return self.stack.items[self.top_of_stack + uindex].integer;
        } else {
            return error.NotImplemented;
        }
    }

    pub fn pop (self: *YAPL, num: usize) !void
    {
        if (num == 0)
        {
            return error.InvalidParameter;
        }
        else if (num <= self.stack.items.len - self.top_of_stack)
        {
            self.stack.items.len -= num;
        }
        else
        {
            self.stack.items.len = 0;
            return error.StackUnderflow;
        }
    }

    pub fn load (self: *YAPL, source: []const u8, label: []const u8) !void
    {
        const source_str = try self.intern (source);
        const label_str = try self.intern (label);

        try self.push_string (source_str);
        try self.push_string (label_str);

        try self.push_nil ();
    }

    pub fn call (self: *YAPL) !void
    {
        _ = self;
    }

    pub fn intern (self: *YAPL, slice: []const u8) !String
    {
        if (slice.len >= std.math.maxInt (u32))
        {
            return error.InvalidParameter;
        }

        if (std.mem.indexOf (u8, self.interned_strings.items, slice)) |index|
        {
            return .{
                .index = @intCast (index),
                .len = @intCast (slice.len),
            };
        }
        else
        {
            const index = self.interned_strings.items.len;
            try self.interned_strings.appendSlice (self.allocator, slice);

            return .{
                .index = @intCast (index),
                .len = @intCast (slice.len),
            };
        }
    }

    pub fn string (self: *YAPL, str: String) ![]const u8
    {
        if (str.index + str.len <= self.interned_strings.items.len)
        {
            return self.interned_strings.items[str.index .. str.index + str.len];
        }
        return error.InvalidString;
    }

    pub fn dump_stack(self: *YAPL, label: []const u8) !void {
        std.debug.print("YAPL {s}\n", .{label});
        for (self.top_of_stack .. self.stack.items.len) |i| {
            const value = self.stack.items[i];
            switch (value)
            {
                .nil =>
                {
                    std.debug.print("  {}: nil\n", .{ i - self.top_of_stack });
                },
                .boolean => |val|
                {
                    std.debug.print("  {}: {} : boolean\n", .{ i - self.top_of_stack, val });
                },
                .integer => |val|
                {
                    std.debug.print("  {}: {d} : integer\n", .{ i - self.top_of_stack, val });
                },
                .number => |val|
                {
                    std.debug.print("  {}: {d} : number\n", .{ i - self.top_of_stack, val });
                },
                .string => |val|
                {
                    const slice = try self.string (val);
                    std.debug.print("  {}: '{'}' : string\n", .{ i - self.top_of_stack, std.zig.fmtEscapes (slice) });
                },
            }
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
