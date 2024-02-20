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
    function,
};

///////////////////////////////////////////////////////////////////////////////////////////////

const nil = Value{ .nil = {} };

///////////////////////////////////////////////////////////////////////////////////////////////

pub const Value = union(ValueType) {
    nil,
    boolean: bool,
    integer: i64,
    number: f64,
    string: String,
    function: Function,

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn add(lhs: Value, rhs: Value) Value {
        if (lhs == .integer and rhs == .integer) {
            return Value{ .integer = lhs.integer + rhs.integer };
        } else {
            return nil;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn sub(lhs: Value, rhs: Value) Value {
        if (lhs == .integer and rhs == .integer) {
            return Value{ .integer = lhs.integer - rhs.integer };
        } else {
            return nil;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn mul(lhs: Value, rhs: Value) Value {
        if (lhs == .integer and rhs == .integer) {
            return Value{ .integer = lhs.integer * rhs.integer };
        } else {
            return nil;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn div(lhs: Value, rhs: Value) Value {
        if (lhs == .integer and rhs == .integer) {
            return Value{
                .number = @as(f64, @floatFromInt(lhs.integer)) / @as(f64, @floatFromInt(rhs.integer)),
            };
        } else {
            return nil;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn mod(lhs: Value, rhs: Value) Value {
        if (lhs == .integer and rhs == .integer) {
            return Value{ .number = lhs.integer % rhs.integer };
        } else {
            return nil;
        }
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

pub const Function = struct {
    name: []const u8,
    code: std.ArrayListUnmanaged(Instruction) = .{},
    data: std.ArrayListUnmanaged(u8) = .{},
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Opcode = enum(u8) {
    nop,
    call,
    const_integer,
    const_number,
    true,
    false,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Instruction = packed union {
    opcode: Opcode,
    _padding0: u24,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const YAPL = struct {
    allocator: std.mem.Allocator,

    stack: std.ArrayListUnmanaged(Value) = .{},
    top_of_stack: usize = 0,

    debug_tokens: bool = false,
    debug_ast: bool = false,

    interned_strings: std.ArrayListUnmanaged(u8) = .{},

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub const Options = struct {
        debug_tokens: bool = false,
        debug_ast: bool = false,
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn init(allocator: std.mem.Allocator, options: Options) !YAPL {
        return .{
            .allocator = allocator,
            .debug_tokens = options.debug_tokens,
            .debug_ast = options.debug_ast,
        };
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn deinit(self: *YAPL) void {
        self.stack.deinit(self.allocator);
        self.interned_strings.deinit(self.allocator);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_nil(self: *YAPL) !void {
        try self.stack.append(self.allocator, nil);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_boolean(self: *YAPL, value: bool) !void {
        const val = Value{ .boolean = value };
        try self.stack.append(self.allocator, val);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_integer(self: *YAPL, value: i64) !void {
        const val = Value{ .integer = value };
        try self.stack.append(self.allocator, val);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_number(self: *YAPL, value: f64) !void {
        const val = Value{ .number = value };
        try self.stack.append(self.allocator, val);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_string(self: *YAPL, value: String) !void {
        const val = Value{ .string = value };
        try self.stack.append(self.allocator, val);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn push_tos(self: *YAPL) usize {
        const old_tos = self.top_of_stack;
        self.top_of_stack = self.stack.items.len;
        return old_tos;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn pop_tos(self: *YAPL, old_tos: usize) void {
        self.top_of_stack = old_tos;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn sub(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn mul(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn div(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn idiv(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn mod(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn negate(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn left_shift(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn right_shift(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn bitwise_not(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn bitwise_and(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn bitwise_or(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn bitwise_xor(self: *YAPL) !void {
        _ = self;
        return error.NotImplemented;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn pop(self: *YAPL, num: usize) !void {
        if (num == 0) {
            return error.InvalidParameter;
        } else if (num <= self.stack.items.len - self.top_of_stack) {
            self.stack.items.len -= num;
        } else {
            self.stack.items.len = 0;
            return error.StackUnderflow;
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn load(self: *YAPL, source: []const u8, label: []const u8) !void {
        const source_str = try self.intern(source);
        const label_str = try self.intern(label);

        try self.compile(source_str, label_str);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    const TokenData = struct {
        yapl: *YAPL,
        tk: Token,
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn fmtToken(self: *YAPL, tk: Token) std.fmt.Formatter(tokenFormatter) {
        const data = TokenData{ .yapl = self, .tk = tk };
        return .{ .data = data };
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn tokenFormatter(data: TokenData, _: anytype, _: anytype, writer: anytype) !void {
        const tk = data.tk;
        const slice = try data.yapl.string(tk.string);
        try writer.writeAll("Token(");
        try writer.print("{s} '{'}'", .{
            @tagName(tk.kind),
            std.zig.fmtEscapes(slice),
        });
        try writer.writeAll(")");
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    const Kind = enum {
        add,
        add_assign,
        assign,
        at_identifier,
        bang,
        binop_add,
        binop_deref,
        binop_div,
        binop_eq,
        binop_gt,
        binop_gte,
        binop_index,
        binop_logical_and,
        binop_logical_or,
        binop_lsh,
        binop_lt,
        binop_lte,
        binop_mod,
        binop_mul,
        binop_neq,
        binop_rsh,
        binop_sub,
        block,
        boolean_false,
        boolean_true,
        break_statement,
        call,
        close_block,
        close_paren,
        close_square,
        colon,
        comma,
        continue_statement,
        div,
        div_assign,
        dot,
        eq,
        file,
        gt,
        gte,
        identifier,
        if_condition,
        if_else,
        if_statement,
        integer_literal,
        invalid_literal,
        keyword_and,
        keyword_break,
        keyword_const,
        keyword_continue,
        keyword_elif,
        keyword_else,
        keyword_if,
        keyword_not,
        keyword_or,
        keyword_return,
        keyword_var,
        keyword_while,
        lsh,
        lsh_assign,
        lt,
        lte,
        mod,
        mod_assign,
        mul,
        mul_assign,
        neq,
        none,
        number_literal,
        open_block,
        open_paren,
        open_square,
        return_statement,
        rsh,
        rsh_assign,
        semicolon,
        string_literal,
        sub,
        sub_assign,
        unary_logical_not,
        unary_neg,
        unary_pos,
        unknown,
        while_statement,
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    const Token = struct {
        kind: Kind,
        string: String,
        label: String,
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    const TokenIterator = struct {
        yapl: *YAPL,
        index: u32,
        source: String,
        label: String,

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn reset(self: *TokenIterator) void {
            self.index = 0;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn save(self: *TokenIterator) u32 {
            return self.index;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn peek(self: *TokenIterator) ?Token {
            const save_point = self.save();
            const token = self.next_internal(@returnAddress(), "peek");
            self.restore(save_point);
            return token;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn restore(self: *TokenIterator, savepoint: u32) void {
            self.index = savepoint;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn next(self: *TokenIterator) ?Token {
            return self.next_internal(@returnAddress(), "next");
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn parse_string_literal(self: *TokenIterator, original: String) !String {
            const str = try self.yapl.string(original);

            if (original.len < 2 or str[0] != '"' or str[original.len - 1] != '"') {
                return error.IncompleteString;
            }

            var difficult = false;

            for (str) |ch| {
                if (ch == '\\') {
                    difficult = true;
                }
            }

            if (!difficult) {
                return .{
                    .index = original.index + 1,
                    .len = original.len - 2,
                };
            }

            var buf = std.ArrayList(u8).init(self.yapl.allocator);
            defer buf.deinit();

            var index: usize = 1;
            while (index < str.len - 1) {
                const ch = str[index];
                if (ch == '\\') {
                    index += 1;
                    if (index < str.len - 1) {
                        switch (str[index]) {
                            'n' => {
                                try buf.append('\n');
                                index += 1;
                            },
                            'r' => {
                                try buf.append('\r');
                                index += 1;
                            },
                            't' => {
                                try buf.append('\t');
                                index += 1;
                            },
                            '\\' => {
                                try buf.append('\\');
                                index += 1;
                            },
                            'x' => {
                                index += 1;
                                if (index < str.len - 2) {
                                    const n0 = str[index];
                                    const n1 = str[index + 1];
                                    index += 2;
                                    var codepoint: u8 = 0;
                                    if (n0 >= '0' and n0 <= '9') {
                                        codepoint |= (n0 - '0') << 4;
                                    } else if (n0 >= 'a' and n0 <= 'f') {
                                        codepoint |= (n0 - 'a' + 10) << 4;
                                    } else if (n0 >= 'A' and n0 <= 'F') {
                                        codepoint |= (n0 - 'A' + 10) << 4;
                                    }
                                    if (n1 >= '0' and n1 <= '9') {
                                        codepoint |= n1 - '0';
                                    } else if (n1 >= 'a' and n1 <= 'f') {
                                        codepoint |= n1 - 'a' + 10;
                                    } else if (n1 >= 'A' and n1 <= 'F') {
                                        codepoint |= n1 - 'A' + 10;
                                    }
                                    try buf.append(codepoint);
                                }
                            },
                            'u' => {
                                index += 1;
                                if (index < str.len - 4) {
                                    var codepoint: u16 = 0;
                                    for (0..4) |_| {
                                        const n = str[index];
                                        index += 1;
                                        codepoint <<= 4;
                                        if (n >= '0' and n <= '9') {
                                            codepoint |= (n - '0');
                                        } else if (n >= 'a' and n <= 'f') {
                                            codepoint |= (n - 'a' + 10) << 4;
                                        } else if (n >= 'A' and n <= 'F') {
                                            codepoint |= (n - 'A' + 10) << 4;
                                        }
                                    }
                                    var temp: [4]u8 = undefined;
                                    const len = try std.unicode.utf8Encode(codepoint, &temp);
                                    try buf.appendSlice(temp[0..len]);
                                }
                            },
                            else => return error.UnknownEscapeCode,
                        }
                    }
                } else {
                    try buf.append(ch);
                    index += 1;
                }
            }

            const updated = try self.yapl.intern(buf.items);

            return updated;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn next_internal(self: *TokenIterator, addr: usize, name: []const u8) ?Token {
            if (self.index >= self.source.len) {
                return null;
            }

            var start = self.index;
            const str = self.yapl.string(self.source) catch {
                self.index += 1;
                return null;
            };

            var kind: Kind = .unknown;

            while (self.index < self.source.len) {
                switch (str[self.index]) {
                    ' ', '\n', '\r', '\t' => {
                        kind = .none;
                        self.index += 1;
                        start = self.index;
                        while (self.index < self.source.len) {
                            switch (str[self.index]) {
                                ' ', '\r', '\n', '\t' => {
                                    self.index += 1;
                                    start = self.index;
                                },
                                else => break,
                            }
                        }
                    },
                    '0' => {
                        kind = .integer_literal;
                        self.index += 1;
                        if (str[self.index] == '.') {
                            kind = .number_literal;
                            self.index += 1;
                            while (self.index < self.source.len) {
                                switch (str[self.index]) {
                                    '0'...'9', '_' => {
                                        self.index += 1;
                                    },
                                    else => break,
                                }
                            }
                        } else if (str[self.index] == 'b') {
                            kind = .number_literal;
                            self.index += 1;
                            while (self.index < self.source.len) {
                                switch (str[self.index]) {
                                    '0'...'1', '_' => {
                                        self.index += 1;
                                    },
                                    else => break,
                                }
                            }
                        } else if (str[self.index] == 'x') {
                            kind = .number_literal;
                            self.index += 1;
                            while (self.index < self.source.len) {
                                switch (str[self.index]) {
                                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {
                                        self.index += 1;
                                    },
                                    else => break,
                                }
                            }
                        }
                        break;
                    },
                    '1'...'9' => {
                        kind = .integer_literal;
                        self.index += 1;
                        while (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '0'...'9', '_' => {
                                    self.index += 1;
                                },
                                else => break,
                            }
                        }
                        if (str[self.index] == '.') {
                            kind = .number_literal;
                            self.index += 1;
                            while (self.index < self.source.len) {
                                switch (str[self.index]) {
                                    '0'...'9', '_' => {
                                        self.index += 1;
                                    },
                                    else => break,
                                }
                            }
                        }
                        break;
                    },
                    '@' => {
                        kind = .at_identifier;
                        self.index += 1;
                        while (self.index < self.source.len) {
                            switch (str[self.index]) {
                                'a'...'z', 'A'...'Z', '_', '0'...'9' => {
                                    self.index += 1;
                                },
                                else => break,
                            }
                        }
                        break;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        kind = .identifier;
                        self.index += 1;
                        while (self.index < self.source.len) {
                            switch (str[self.index]) {
                                'a'...'z', 'A'...'Z', '_', '0'...'9' => {
                                    self.index += 1;
                                },
                                else => break,
                            }
                        }
                        break;
                    },
                    '!' => {
                        kind = .bang;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .neq;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '+' => {
                        kind = .add;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .add_assign;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '-' => {
                        kind = .sub;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .sub_assign;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '*' => {
                        kind = .mul;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .mul_assign;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '/' => {
                        kind = .div;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .div_assign;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '%' => {
                        kind = .mod;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .mod_assign;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '(' => {
                        kind = .open_paren;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        kind = .close_paren;
                        self.index += 1;
                        break;
                    },
                    '[' => {
                        kind = .open_square;
                        self.index += 1;
                        break;
                    },
                    ']' => {
                        kind = .close_square;
                        self.index += 1;
                        break;
                    },
                    '{' => {
                        kind = .open_block;
                        self.index += 1;
                        break;
                    },
                    '}' => {
                        kind = .close_block;
                        self.index += 1;
                        break;
                    },
                    ',' => {
                        kind = .comma;
                        self.index += 1;
                        break;
                    },
                    '.' => {
                        kind = .dot;
                        self.index += 1;
                        break;
                    },
                    ':' => {
                        kind = .colon;
                        self.index += 1;
                        break;
                    },
                    ';' => {
                        kind = .semicolon;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        kind = .gt;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .gte;
                                    self.index += 1;
                                },
                                '>' => {
                                    kind = .rsh;
                                    self.index += 1;
                                    if (self.index < self.source.len) {
                                        switch (str[self.index]) {
                                            '=' => {
                                                kind = .rsh_assign;
                                                self.index += 1;
                                            },
                                            else => {},
                                        }
                                    }
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '<' => {
                        kind = .lt;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .lte;
                                    self.index += 1;
                                },
                                '<' => {
                                    kind = .lsh;
                                    self.index += 1;
                                    if (self.index < self.source.len) {
                                        switch (str[self.index]) {
                                            '=' => {
                                                kind = .lsh_assign;
                                                self.index += 1;
                                            },
                                            else => {},
                                        }
                                    }
                                },
                                else => {},
                            }
                        }
                        break;
                    },
                    '=' => {
                        kind = .assign;
                        self.index += 1;
                        if (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '=' => {
                                    kind = .eq;
                                    self.index += 1;
                                },
                                else => {},
                            }
                        }
                        break;
                    },

                    '"' => {
                        kind = .string_literal;
                        self.index += 1;
                        while (self.index < self.source.len) {
                            switch (str[self.index]) {
                                '"' => {
                                    self.index += 1;
                                    break;
                                },
                                '\\' => {
                                    self.index += 1;
                                    if (self.index < self.source.len) {
                                        self.index += 1;
                                    }
                                },
                                else => {
                                    self.index += 1;
                                },
                            }
                        }
                        break;
                    },
                    else => {
                        kind = .unknown;
                        self.index += 1;
                        break;
                    },
                }
            }

            var word: String = .{
                .index = start,
                .len = self.index - start,
            };

            const keyword = str[start..self.index];

            if (kind == .identifier) {
                if (std.mem.eql(u8, "const", keyword)) {
                    kind = .keyword_const;
                } else if (std.mem.eql(u8, "true", keyword)) {
                    kind = .boolean_true;
                } else if (std.mem.eql(u8, "false", keyword)) {
                    kind = .boolean_false;
                } else if (std.mem.eql(u8, "and", keyword)) {
                    kind = .keyword_and;
                } else if (std.mem.eql(u8, "or", keyword)) {
                    kind = .keyword_or;
                } else if (std.mem.eql(u8, "not", keyword)) {
                    kind = .keyword_not;
                } else if (std.mem.eql(u8, "if", keyword)) {
                    kind = .keyword_if;
                } else if (std.mem.eql(u8, "elif", keyword)) {
                    kind = .keyword_elif;
                } else if (std.mem.eql(u8, "else", keyword)) {
                    kind = .keyword_else;
                } else if (std.mem.eql(u8, "while", keyword)) {
                    kind = .keyword_while;
                } else if (std.mem.eql(u8, "break", keyword)) {
                    kind = .keyword_break;
                } else if (std.mem.eql(u8, "continue", keyword)) {
                    kind = .keyword_continue;
                } else if (std.mem.eql(u8, "return", keyword)) {
                    kind = .keyword_return;
                } else if (std.mem.eql(u8, "var", keyword)) {
                    kind = .keyword_var;
                } else if (std.mem.eql(u8, "const", keyword)) {
                    kind = .keyword_const;
                }
            } else if (kind == .string_literal) {
                word = self.parse_string_literal(word) catch blk: {
                    kind = .invalid_literal;
                    break :blk word;
                };
            }

            if (kind != .none) {
                if (false) {
                    const debug_info = std.debug.getSelfDebugInfo() catch {
                        return .{
                            .kind = kind,
                            .string = word,
                            .label = self.label,
                        };
                    };
                    const module = debug_info.getModuleForAddress(addr) catch {
                        return .{
                            .kind = kind,
                            .string = word,
                            .label = self.label,
                        };
                    };
                    const symbol = module.getSymbolAtAddress(self.yapl.allocator, addr) catch {
                        return .{
                            .kind = kind,
                            .string = word,
                            .label = self.label,
                        };
                    };
                    defer symbol.deinit(self.yapl.allocator);
                    std.debug.print("Token {s} {s} {} '{'}' {s}\n", .{
                        name,
                        @tagName(kind),
                        word.index,
                        std.zig.fmtEscapes(str[word.index .. word.index + word.len]),
                        symbol.symbol_name,
                    });
                }
                return .{
                    .kind = kind,
                    .string = word,
                    .label = self.label,
                };
            }
            return null;
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn tokenize(self: *YAPL, source: String, label: String) TokenIterator {
        return .{
            .yapl = self,
            .index = 0,
            .source = source,
            .label = label,
        };
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn compile(self: *YAPL, source: String, label: String) !void {
        var token_iter = self.tokenize(source, label);

        if (self.debug_tokens) {
            while (token_iter.next()) |tk| {
                std.debug.print("{}\n", .{tk});
            }

            token_iter.reset();
        }

        var ast = Ast.init(self);
        defer ast.deinit();

        const file = try ast.append(.file, label);
        while (try ast.parse_statement(&token_iter)) |stmt| {
            ast.add_child(file, stmt);
        }

        if (token_iter.next()) |tk| {
            try ast.add_error("Syntax error at '{'}'", .{
                std.zig.fmtEscapes(try self.string(tk.string)),
            });
        }

        if (self.debug_ast) {
            ast.dump(file);
        }

        if (ast.errors.items.len > 0) {
            std.debug.print("{s}\n", .{ast.errors.items});
            return;
        }

        try self.push_nil();
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub const Node = struct {
        kind: Kind,
        string: String,
        first_child: NodeIndex = 0,
        last_child: NodeIndex = 0,
        next_child: NodeIndex = 0,
    };

    pub const NodeIndex = u32;

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub const Ast = struct {
        nodes: std.ArrayListUnmanaged(Node) = .{},
        errors: std.ArrayListUnmanaged(u8) = .{},
        yapl: *YAPL,

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn init(yapl: *YAPL) Ast {
            return .{
                .yapl = yapl,
            };
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        pub fn deinit(self: *Ast) void {
            const allocator = self.yapl.allocator;

            self.nodes.deinit(allocator);
            self.errors.deinit(allocator);
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn add_error(self: *Ast, comptime msg: []const u8, args: anytype) !void {
            const allocator = self.yapl.allocator;

            const buf = try std.fmt.allocPrint(allocator, msg ++ "\n", args);
            try self.errors.appendSlice(allocator, buf);
            defer allocator.free(buf);
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn append(self: *Ast, kind: Kind, str: String) !?NodeIndex {
            const allocator = self.yapl.allocator;
            const index = self.nodes.items.len;

            try self.nodes.append(allocator, .{
                .kind = kind,
                .string = str,
            });

            return @intCast(index);
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn add_child(self: *Ast, _index: ?NodeIndex, _child: ?NodeIndex) void {
            if (_index == null or _child == null) {
                return;
            }

            const index = _index.?;
            const child = _child.?;

            if (index >= self.nodes.items.len) {
                return;
            }
            if (child == 0 or child >= self.nodes.items.len) {
                return;
            }

            var node = &self.nodes.items[index];
            if (node.first_child == 0) {
                node.first_child = child;
                node.last_child = child;
            } else {
                self.nodes.items[node.last_child].next_child = child;
                node.last_child = child;
            }
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        const ParseErrors = error{
            OutOfMemory,
            InvalidString,
        };

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_expression_list(self: *Ast, iter: *TokenIterator, func: ?NodeIndex) ParseErrors!void {
            while (iter.peek()) |tk| {
                if (tk.kind == .close_paren) {
                    _ = iter.next();
                    return;
                } else {
                    const expr = try self.parse_expression(iter);
                    if (expr != 0) {
                        self.add_child(func, expr);
                    }
                    if (iter.peek()) |ctk| {
                        if (ctk.kind == .comma) {
                            _ = iter.next();
                        }
                    }
                }
            }
            try self.add_error("Expected ')', found end of file", .{});
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_prefix(self: *Ast, iter: *TokenIterator, name: String) ParseErrors!?NodeIndex {
            var lhs = try self.append(.identifier, name);

            while (iter.peek()) |tk| {
                switch (tk.kind) {
                    .open_square => {
                        _ = iter.next();
                        const rhs = try self.parse_expression(iter);
                        if (iter.peek()) |ctk| {
                            if (ctk.kind == .close_square) {
                                _ = iter.next();
                                const op = try self.append(.binop_index, tk.string);
                                self.add_child(op, lhs);
                                self.add_child(op, rhs);
                                lhs = op;
                            } else {
                                const str = try self.yapl.string(ctk.string);
                                try self.add_error("Expected ']', found '{'}'", .{
                                    std.zig.fmtEscapes(str),
                                });
                                return null;
                            }
                        } else {
                            try self.add_error("Expected ']', found end of file", .{});
                            return null;
                        }
                    },
                    .open_paren => {
                        _ = iter.next();
                        const op = try self.append(.call, tk.string);
                        self.add_child(op, lhs);
                        try self.parse_expression_list(iter, op);
                        lhs = op;
                    },
                    .dot => {
                        _ = iter.next();
                        if (iter.peek()) |itk| {
                            if (itk.kind == .identifier) {
                                _ = iter.next();
                                const op = try self.append(.binop_deref, tk.string);
                                const rhs = try self.append(.identifier, itk.string);
                                self.add_child(op, lhs);
                                self.add_child(op, rhs);
                                lhs = op;
                            }
                        } else {
                            try self.add_error("Expected identifier, found end of file", .{});
                            return null;
                        }
                    },
                    else => {
                        break;
                    },
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_atom(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            if (iter.next()) |tk| {
                switch (tk.kind) {
                    .boolean_true, .boolean_false => {
                        return self.append(tk.kind, tk.string);
                    },
                    .integer_literal => {
                        return self.append(.integer_literal, tk.string);
                    },
                    .number_literal => {
                        return self.append(.number_literal, tk.string);
                    },
                    .string_literal => {
                        return self.append(.string_literal, tk.string);
                    },
                    .identifier => {
                        return self.parse_prefix(iter, tk.string);
                    },
                    .at_identifier => {
                        return self.parse_prefix(iter, tk.string);
                    },
                    .open_paren => {
                        const expr = self.parse_expression(iter);
                        if (iter.peek()) |ctk| {
                            if (ctk.kind == .close_paren) {
                                _ = iter.next();
                                return expr;
                            } else {
                                const str = try self.yapl.string(ctk.string);
                                try self.add_error("Expected ')', found '{'}'", .{
                                    std.zig.fmtEscapes(str),
                                });
                                return null;
                            }
                        } else {
                            try self.add_error("Expected ')', found end of file", .{});
                            return null;
                        }
                    },
                    else => {
                        const str = try self.yapl.string(tk.string);
                        try self.add_error("Expected atom, found '{'}'", .{
                            std.zig.fmtEscapes(str),
                        });
                        return null;
                    },
                }
            } else {
                try self.add_error("Expected atom, found end of file", .{});
                return null;
            }
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_unary(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            if (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .sub => .unary_neg,
                    .add => .unary_pos,
                    .keyword_not => .unary_logical_not,
                    else => null,
                };

                if (op) |uop| {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    const unop = try self.append(uop, tk.string);
                    self.add_child(unop, rhs);
                    return unop;
                }
            }

            return try self.parse_atom(iter);
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_multiplicative(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            var lhs = try self.parse_unary(iter);
            while (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .mul => .binop_mul,
                    .div => .binop_div,
                    .mod => .binop_mod,
                    .lsh => .binop_lsh,
                    .rsh => .binop_rsh,
                    else => null,
                };

                if (op) |bop| {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    const binop = try self.append(bop, tk.string);
                    self.add_child(binop, lhs);
                    self.add_child(binop, rhs);
                    lhs = binop;
                } else {
                    break;
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_additive(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            var lhs = try self.parse_multiplicative(iter);
            while (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .add => .binop_add,
                    .sub => .binop_sub,
                    else => null,
                };

                if (op) |bop| {
                    _ = iter.next();
                    const rhs = try self.parse_multiplicative(iter);
                    const binop = try self.append(bop, tk.string);
                    self.add_child(binop, lhs);
                    self.add_child(binop, rhs);
                    lhs = binop;
                } else {
                    break;
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_comparitive(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            var lhs = try self.parse_additive(iter);
            if (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .lt => .binop_lt,
                    .lte => .binop_lte,
                    .eq => .binop_eq,
                    .neq => .binop_neq,
                    .gte => .binop_gte,
                    .gt => .binop_gt,
                    else => null,
                };

                if (op) |bop| {
                    _ = iter.next();
                    const rhs = try self.parse_additive(iter);
                    const binop = try self.append(bop, tk.string);
                    self.add_child(binop, lhs);
                    self.add_child(binop, rhs);
                    lhs = binop;
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_logical_and(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            var lhs = try self.parse_comparitive(iter);
            while (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .keyword_and => .binop_logical_and,
                    else => null,
                };

                if (op) |bop| {
                    _ = iter.next();
                    const rhs = try self.parse_comparitive(iter);
                    const binop = try self.append(bop, tk.string);
                    self.add_child(binop, lhs);
                    self.add_child(binop, rhs);
                    lhs = binop;
                } else {
                    break;
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_logical_or(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            var lhs = try self.parse_logical_and(iter);
            while (iter.peek()) |tk| {
                const op: ?Kind = switch (tk.kind) {
                    .keyword_or => .binop_logical_or,
                    else => null,
                };

                if (op) |bop| {
                    _ = iter.next();
                    const rhs = try self.parse_logical_and(iter);
                    const binop = try self.append(bop, tk.string);
                    self.add_child(binop, lhs);
                    self.add_child(binop, rhs);
                    lhs = binop;
                } else {
                    break;
                }
            }
            return lhs;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_expression(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            return self.parse_logical_or(iter);
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_paren_expression(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            if (iter.peek()) |tk| {
                if (tk.kind == .open_paren) {
                    _ = iter.next();
                } else {
                    const str = try self.yapl.string(tk.string);
                    try self.add_error("Expected '(', found '{'}'", .{
                        std.zig.fmtEscapes(str),
                    });
                    return null;
                }
            } else {
                try self.add_error("Expected '(', found end of file", .{});
                return null;
            }

            const expr = try self.parse_expression(iter);

            if (iter.peek()) |tk| {
                if (tk.kind == .close_paren) {
                    _ = iter.next();
                    return expr;
                } else {
                    const str = try self.yapl.string(tk.string);
                    try self.add_error("Expected ')', found '{'}'", .{
                        std.zig.fmtEscapes(str),
                    });
                    return null;
                }
            } else {
                try self.add_error("Expected ')', found end of file", .{});
                return null;
            }

            return null;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_if_statement(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            const itk = iter.peek().?;
            _ = iter.next();

            const if_stmt = try self.append(.if_statement, itk.string);

            {
                const if_condition = try self.append(.if_condition, itk.string);
                self.add_child(if_stmt, if_condition);

                const expr = try self.parse_paren_expression(iter);
                self.add_child(if_condition, expr);

                const blk = try self.parse_block(iter);
                self.add_child(if_condition, blk);
            }

            while (iter.peek()) |tk| {
                if (tk.kind == .keyword_elif) {
                    _ = iter.next();
                    const if_condition = try self.append(.if_condition, tk.string);
                    self.add_child(if_stmt, if_condition);

                    const expr = try self.parse_paren_expression(iter);
                    self.add_child(if_condition, expr);

                    const blk = try self.parse_block(iter);
                    self.add_child(if_condition, blk);
                } else if (tk.kind == .keyword_else) {
                    _ = iter.next();
                    const blk = try self.parse_block(iter);
                    self.add_child(if_stmt, blk);
                    break;
                } else {
                    break;
                }
            }
            return if_stmt;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_while_statement(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            const wtk = iter.peek().?;
            _ = iter.next();
            const while_stmt = try self.append(.while_statement, wtk.string);
            const expr = try self.parse_paren_expression(iter);
            self.add_child(while_stmt, expr);
            if (iter.peek()) |tk| {
                if (tk.kind == .open_block) {
                    const blk = try self.parse_block(iter);
                    self.add_child(while_stmt, blk);
                } else {
                    const str = try self.yapl.string(tk.string);
                    try self.add_error("Expected '{{', found '{'}'", .{
                        std.zig.fmtEscapes(str),
                    });
                    return null;
                }
            } else {
                try self.add_error("Expected '{{', found end of file", .{});
                return null;
            }
            return while_stmt;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_block(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            if (iter.peek()) |tk| {
                if (tk.kind == .open_block) {
                    _ = iter.next();

                    const blk = try self.append(.block, tk.string);

                    while (iter.peek()) |btk| {
                        if (btk.kind == .close_block) {
                            _ = iter.next();
                            return blk;
                        } else {
                            const stmt = try self.parse_statement(iter);
                            self.add_child(blk, stmt);
                        }
                    }
                    try self.add_error("Expected '}}', found end of file", .{});
                    return null;
                } else {
                    const str = try self.yapl.string(tk.string);
                    try self.add_error("Expected '{{', found '{'}'", .{
                        std.zig.fmtEscapes(str),
                    });
                    return null;
                }
            } else {
                try self.add_error("Expected '{{', found end of file", .{});
                return null;
            }
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn parse_statement(self: *Ast, iter: *TokenIterator) ParseErrors!?NodeIndex {
            while (iter.peek()) |tk| {
                switch (tk.kind) {
                    .keyword_if => {
                        return self.parse_if_statement(iter);
                    },
                    .keyword_while => {
                        return self.parse_while_statement(iter);
                    },
                    .keyword_break => {
                        _ = iter.next();
                        const stmt = self.append(.break_statement, tk.string);
                        return stmt;
                    },
                    .keyword_continue => {
                        _ = iter.next();
                        const stmt = self.append(.continue_statement, tk.string);
                        return stmt;
                    },
                    .keyword_return => {
                        _ = iter.next();
                        const stmt = try self.append(.return_statement, tk.string);
                        if (iter.peek()) |etk| {
                            switch (etk.kind) {
                                .close_block,
                                .semicolon,
                                => return stmt,
                                else => {
                                    const expr = try self.parse_expression(iter);
                                    self.add_child(stmt, expr);
                                },
                            }
                        }
                        return stmt;
                    },
                    .open_block => {
                        const blk = self.parse_block(iter);
                        return blk;
                    },
                    .identifier, .at_identifier => {
                        const lhs = try self.parse_expression(iter);
                        if (iter.peek()) |atk| {
                            switch (atk.kind) {
                                .assign,
                                .add_assign,
                                .sub_assign,
                                => {
                                    _ = iter.next();
                                    const op = try self.append(.assign, atk.string);
                                    const rhs = try self.parse_expression(iter);
                                    self.add_child(op, lhs);
                                    self.add_child(op, rhs);
                                    return op;
                                },
                                else => {
                                    return lhs;
                                },
                            }
                        } else {
                            return lhs;
                        }
                    },
                    .semicolon => {
                        _ = iter.next();
                    },
                    else => {
                        _ = iter.next();
                        const str = try self.yapl.string(tk.string);
                        try self.add_error("Invalid statement at '{'}'", .{
                            std.zig.fmtEscapes(str),
                        });
                    },
                }
            }
            return null;
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn dump_internal(self: *Ast, index: NodeIndex, depth: usize) void {
            const node = self.nodes.items[index];
            const lots_of_spaces = "  " ** 32;
            if (self.yapl.string(node.string)) |slice| {
                std.debug.print("{s}{s} '{'}'\n", .{
                    lots_of_spaces[0 .. depth * 2],
                    @tagName(node.kind),
                    std.zig.fmtEscapes(slice),
                });
            } else |_| {
                std.debug.print("{s}{s}\n", .{
                    lots_of_spaces[0 .. depth * 2],
                    @tagName(node.kind),
                });
            }

            var child = node.first_child;
            while (child != 0) {
                self.dump_internal(child, depth + 1);
                child = self.nodes.items[child].next_child;
            }
        }

        ///////////////////////////////////////////////////////////////////////////////////////

        fn dump(self: *Ast, _index: ?NodeIndex) void {
            std.debug.print("AST\n", .{});
            if (_index) |index| {
                self.dump_internal(index, 1);
            }
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn call(self: *YAPL) !void {
        _ = self;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn intern(self: *YAPL, slice: []const u8) !String {
        if (slice.len >= std.math.maxInt(u32)) {
            return error.InvalidParameter;
        }

        if (std.mem.indexOf(u8, self.interned_strings.items, slice)) |index| {
            return .{
                .index = @intCast(index),
                .len = @intCast(slice.len),
            };
        } else {
            const index = self.interned_strings.items.len;
            try self.interned_strings.appendSlice(self.allocator, slice);

            return .{
                .index = @intCast(index),
                .len = @intCast(slice.len),
            };
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn string(self: *YAPL, str: String) ![]const u8 {
        if (str.index + str.len <= self.interned_strings.items.len) {
            return self.interned_strings.items[str.index .. str.index + str.len];
        }
        return error.InvalidString;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn dump_stack(self: *YAPL, label: []const u8) !void {

        if (true) {
            const addr = @returnAddress ();
            const debug_info = try std.debug.getSelfDebugInfo();
            const module = try debug_info.getModuleForAddress(addr);
            const symbol = try module.getSymbolAtAddress(self.allocator, addr);
            defer symbol.deinit(self.allocator);
            std.debug.print("YAPL stack: '{'}' {s}\n", .{
                std.zig.fmtEscapes(label),
                symbol.symbol_name,
            });
        }
        else
        {
            std.debug.print("YAPL stack: '{'}'\n", .{std.zig.fmtEscapes(label)});
        }

        for (self.top_of_stack..self.stack.items.len) |i| {
            const value = self.stack.items[i];
            switch (value) {
                .nil => {
                    std.debug.print("  {}: nil\n", .{i - self.top_of_stack});
                },
                .boolean => |val| {
                    std.debug.print("  {}: {} : boolean\n", .{ i - self.top_of_stack, val });
                },
                .integer => |val| {
                    std.debug.print("  {}: {d} : integer\n", .{ i - self.top_of_stack, val });
                },
                .number => |val| {
                    std.debug.print("  {}: {d} : number\n", .{ i - self.top_of_stack, val });
                },
                .string => |val| {
                    const slice = try self.string(val);
                    std.debug.print("  {}: '{'}' : string\n", .{
                        i - self.top_of_stack,
                        std.zig.fmtEscapes(slice),
                    });
                },
                .function => {
                    std.debug.print("  {}: function\n", .{i - self.top_of_stack});
                },
            }
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
