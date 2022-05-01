const std = @import("std");
const testing = std.testing;
const meta = std.meta;

pub const serializer = @import("./main.zig").serializer;
pub const deserializer = @import("./main.zig").deserializer;
pub const Timestamp = @import("./main.zig").Timestamp;

test "serialization" {
    const test_cases = .{
        .{
            .type = struct { compact: bool, schema: u7 },
            .value = .{ .compact = true, .schema = 0 },
            .expected = "\x82\xa7\x63\x6f\x6d\x70\x61\x63\x74\xc3\xa6\x73\x63\x68\x65\x6d\x61\x00",
        },
        .{
            .type = []const u8,
            .value = "Hello world!",
            .expected = "\xac\x48\x65\x6c\x6c\x6f\x20\x77\x6f\x72\x6c\x64\x21",
        },
        .{
            .type = []const []const u8,
            .value = &[_][]const u8{ "one", "two", "three" },
            .expected = "\x93\xa3\x6f\x6e\x65\xa3\x74\x77\x6f\xa5\x74\x68\x72\x65\x65",
        },
        .{
            .type = ?u32,
            .value = null,
            .expected = "\xc0",
        },
        .{
            .type = ?u32,
            .value = 12389567,
            .expected = "\xce\x00\xbd\x0c\xbf",
        },
        .{
            .type = i32,
            .value = -12389567,
            .expected = "\xd2\xff\x42\xf3\x41",
        },
        .{
            .type = f64,
            .value = 1.25,
            .expected = "\xcb\x3f\xf4\x00\x00\x00\x00\x00\x00",
        },
    };

    inline for (test_cases) |case| {
        var buffer: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        var _serializer = serializer(stream.writer());

        try _serializer.serialize(@as(case.type, case.value));
        try testing.expectEqualSlices(u8, case.expected, stream.getWritten());
    }
}

test "deserialization" {
    const test_cases = .{
        .{
            .type = []const u8,
            .value = "Hello world!",
        },
        .{
            .type = u32,
            .value = @as(u32, 21049),
        },
        .{
            .type = i64,
            .value = @as(i64, 9223372036854775807),
        },
        .{
            .type = struct { compact: bool, schema: u7 },
            .value = .{ .compact = true, .schema = 5 },
        },
        .{
            .type = []const []const u8,
            .value = &[_][]const u8{ "one", "two", "three" },
        },
        .{
            .type = [5]u32,
            .value = .{ 0, 2, 3, 5, 6 },
        },
        .{
            .type = f32,
            .value = 1.25,
        },
        .{
            .type = f64,
            .value = 1.5,
        },
        .{
            .type = []const f64,
            .value = &[_]f64{ -100.455, 2.0, 3.1415 },
        },
        .{
            .type = struct { us: []const u8, fs: []const f64 },
            .value = .{ .us = &[_]u8{
                1,
                2,
                15,
            }, .fs = &[_]f64{ 1.2, 3.14 } },
        },
    };

    inline for (test_cases) |case| {
        var buffer: [4096]u8 = undefined;
        var out = std.io.fixedBufferStream(&buffer);
        var _serializer = serializer(out.writer());

        try _serializer.serialize(@as(case.type, case.value));

        var in = std.io.fixedBufferStream(&buffer);

        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        var _deserializer = deserializer(in.reader(), arena.allocator());

        const result = try _deserializer.deserialize(case.type);

        try expectEqualDeep(case.type, case.value, result);
    }
}

fn expectEqualDeep(comptime T: type, value: T, result: T) !void {
    switch (@typeInfo(T)) {
        .Pointer => |info| switch (info.size) {
            .Slice => switch (@typeInfo(info.child)) {
                .Pointer => for (value) |val, i| {
                    try expectEqualDeep(info.child, val, result[i]);
                },
                else => try testing.expectEqualSlices(info.child, value, result),
            },
            else => @panic("TODO: Testing for Pointer types"),
        },
        .Array => |info| {
            try testing.expectEqualSlices(info.child, value[0..], result[0..]);
        },
        .Struct => |st| inline for (st.fields) |field| {
            try expectEqualDeep(field.field_type, @field(value, field.name), @field(result, field.name));
        },
        else => try testing.expectEqual(value, result),
    }
}

test "(de)serialize timestamp" {
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    var in = std.io.fixedBufferStream(&buffer);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var _deserializer = deserializer(in.reader(), arena.allocator());

    const timestamp = Timestamp{ .sec = 50, .nsec = 200 };
    try _serializer.serializeTimestamp(timestamp);

    const result = try _deserializer.deserializeTimestamp();

    try testing.expectEqual(timestamp, result);
}

test "(de)serialize bin" {
    var input = [_]u8{ 0x01, 0x02, 0x08, 0x09 };

    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    try _serializer.serialize(@as([]u8, &input));

    var in = std.io.fixedBufferStream(&buffer);

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var _deserializer = deserializer(in.reader(), arena.allocator());

    const result = try _deserializer.deserialize([]u8);

    try expectEqualDeep([]u8, &input, result);
}

test "(de)serialize ext format" {
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    var in = std.io.fixedBufferStream(&buffer);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var _deserializer = deserializer(in.reader(), arena.allocator());

    try _serializer.serializeExt(2, "Hello world!");

    var type_result: i8 = undefined;
    const result = try _deserializer.deserializeExt(&type_result);
    try testing.expectEqual(@as(i8, 2), type_result);
    try testing.expectEqualStrings("Hello world!", result);
}

pub const Decl = struct {
    x: u32,
    y: u32,

    pub fn serialize(self: Decl, _serializer: anytype) !void {
        var data: [8]u8 = undefined;
        std.mem.writeIntBig(u32, data[0..4], self.x);
        std.mem.writeIntBig(u32, data[4..8], self.y);
        try _serializer.serializeArray([8]u8, data);
    }

    pub fn deserialize(self: *Decl, _deserializer: anytype) !void {
        const data = try _deserializer.deserializeArray([8]u8);
        self.* = .{
            .x = std.mem.readIntBig(u32, data[0..4]),
            .y = std.mem.readIntBig(u32, data[4..8]),
        };
    }
};

test "(de)serialize with custom declaration" {
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    var in = std.io.fixedBufferStream(&buffer);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var _deserializer = deserializer(in.reader(), arena.allocator());

    var decl = Decl{ .x = 10, .y = 50 };
    try _serializer.serialize(decl);

    const result = try _deserializer.deserialize(Decl);
    try testing.expectEqual(decl, result);
}

test "deserialize no memory leaks" {
    const TestSubStruct = struct {
        str_1: []const u8,
        float: f64,
        str_2: []const u8,
    };

    const TestStruct = struct {
        str: []const u8,
        array: []*TestSubStruct,
    };

    // Prepare the input values
    var input_1 = TestSubStruct{
        .str_1 = "ab",
        .float = 42,
        .str_2 = "cd",
    };

    var input_2 = TestSubStruct{
        .str_1 = "ef",
        .float = 100,
        .str_2 = "gh",
    };

    var input_array = [_]*TestSubStruct{ &input_1, &input_2 };

    var input = TestStruct{
        .str = "foobar",
        .array = &input_array,
    };

    // Create the allocator
    var allocator = std.testing.allocator;

    // Create the buffer to hold the serialized values
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);

    // Create a serializer and serialize the input into the buffer
    var _serializer = serializer(out.writer());
    try _serializer.serialize(input);

    const max_size = out.pos - 1;

    _ = allocator;

    // Test every possible IO failure scenario and make sure no memory leaks at the end
    var i: usize = 0;
    while (i < max_size) : (i += 1) {
        // Create the deserializer
        var in = std.io.fixedBufferStream(&buffer);
        var in_limited = std.io.limitedReader(in.reader(), i);

        var _deserializer = deserializer(in_limited.reader(), allocator);

        _ = _deserializer.deserialize(TestStruct) catch continue;

        // Deserialize should always fail here, so if it didn't then the test itself should fail
        try testing.expect(false);
    }
}
