///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const yapl_zig = @import("yapl.zig");
const YAPL = yapl_zig.YAPL;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const allocator = gpa.allocator();

    std.debug.print("yapl\n", .{});

    var args = try parse_command_line(allocator);
    defer args.deinit();

    if (args.errors.items.len != 0) {
        for (args.errors.items) |item| {
            std.debug.print("Error: {s}\n", .{item});
        }
        std.process.exit(1);
    }

    if (args.debug_args) {
        std.debug.print("args: {}\n", .{args});
    }

    if (args.input_filenames.items.len == 0) {
        std.debug.print("No input filenames given\n", .{});
        std.process.exit(1);
    } else if (args.input_filenames.items.len == 0) {
        std.debug.print("Too many input filenames given\n", .{});
        std.process.exit(1);
    }

    var yapl = try YAPL.init(allocator, .{
        .debug_tokens = args.debug_tokens,
        .debug_ast = args.debug_ast,
    });
    defer yapl.deinit();

    const cwd = std.fs.cwd();

    const source_code = cwd.readFileAlloc(
        allocator,
        args.input_filenames.items[0],
        std.math.maxInt(u32),
    ) catch |err|
        {
        switch (err) {
            error.FileNotFound => {
                std.debug.print("Cannot open file '{'}'\n", .{
                    std.zig.fmtEscapes(args.input_filenames.items[0]),
                });
                std.process.exit(1);
            },
            else => {
                return err;
            },
        }
    };
    defer allocator.free(source_code);

    try yapl.load(source_code, "test.yapl");

    try yapl.dump_stack("load");

    try yapl.call();

    try yapl.dump_stack("end");
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "main test" {}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const CommandLineArguments = struct {
    allocator: std.mem.Allocator,
    quiet: bool = false,
    verbosity: u8 = 0,
    debug_args: bool = false,
    debug_tokens: bool = false,
    debug_ast: bool = false,
    input_filenames: std.ArrayListUnmanaged([]const u8) = .{},
    output_filenames: std.ArrayListUnmanaged([]const u8) = .{},
    errors: std.ArrayListUnmanaged([]const u8) = .{},

    pub fn deinit(self: *CommandLineArguments) void {
        for (self.input_filenames.items) |item| {
            self.allocator.free(item);
        }
        self.input_filenames.deinit(self.allocator);

        for (self.output_filenames.items) |item| {
            self.allocator.free(item);
        }
        self.output_filenames.deinit(self.allocator);

        for (self.errors.items) |item| {
            self.allocator.free(item);
        }
        self.errors.deinit(self.allocator);
    }

    pub fn format(self: CommandLineArguments, _: anytype, _: anytype, writer: anytype) !void {
        try writer.writeAll("CommandLineArguments\n");
        try writer.print("  quiet: {}\n", .{self.quiet});
        try writer.print("  verbosity: {}\n", .{self.verbosity});
        try writer.print("  Dargs: {}\n", .{self.debug_args});
        try writer.print("  Dtokens: {}\n", .{self.debug_tokens});
        try writer.print("  Dast: {}\n", .{self.debug_ast});
        for (0.., self.input_filenames.items) |i, item| {
            try writer.print("  input_filenames[{}]: '{'}'\n", .{
                i,
                std.zig.fmtEscapes(item),
            });
        }
        for (0.., self.output_filenames.items) |i, item| {
            try writer.print("  output_filenames[{}]: '{'}'\n", .{
                i,
                std.zig.fmtEscapes(item),
            });
        }
        for (0.., self.errors.items) |i, item| {
            try writer.print("  errors[{}]: '{'}'\n", .{
                i,
                std.zig.fmtEscapes(item),
            });
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn parse_command_line(allocator: std.mem.Allocator) !CommandLineArguments {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var cla: CommandLineArguments = .{
        .allocator = allocator,
    };

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
            cla.quiet = true;
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            cla.verbosity = 1;
        } else if (std.mem.eql(u8, arg, "-vv") or std.mem.eql(u8, arg, "--veryverbose")) {
            cla.verbosity = 2;
        } else if (std.mem.eql(u8, arg, "-Dargs")) {
            cla.debug_args = true;
        } else if (std.mem.eql(u8, arg, "-Dtokens")) {
            cla.debug_tokens = true;
        } else if (std.mem.eql(u8, arg, "-Dast")) {
            cla.debug_ast = true;
        } else if (arg[0] == '-') {
            const msg = try std.fmt.allocPrint(allocator, "Unknown '{'}'", .{
                std.zig.fmtEscapes(arg),
            });
            try cla.errors.append(allocator, msg);
        } else {
            const item = try allocator.dupe(u8, arg);
            try cla.input_filenames.append(allocator, item);
        }
    }

    return cla;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
