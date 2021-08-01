const std = @import("std");
const testing = std.testing;
const meta = std.meta;

usingnamespace @import("main.zig");

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
    };

    inline for (test_cases) |case| {
        var buffer: [4096]u8 = undefined;
        var out = std.io.fixedBufferStream(&buffer);
        var _serializer = serializer(out.writer());

        try _serializer.serialize(@as(case.type, case.value));

        var in = std.io.fixedBufferStream(&buffer);
        var _deserializer = deserializer(in.reader(), 4096);

        const result = try _deserializer.deserialize(case.type);

        switch (case.type) {
            []const u8 => try testing.expectEqualStrings(case.value, result),
            []u8 => try testing.expectEqualSlices(u8, case.value, result),
            else => switch (@typeInfo(case.type)) {
                .Pointer => |info| switch (info.size) {
                    .Slice => for (case.value) |val, j| {
                        try testing.expectEqualSlices(meta.Child(info.child), val, result[j]);
                    },
                    else => @panic("TODO: Testing for Pointer types"),
                },
                else => try testing.expectEqual(@as(case.type, case.value), result),
            },
        }
    }
}

test "(de)serialize timestamp" {
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    var in = std.io.fixedBufferStream(&buffer);
    var _deserializer = deserializer(in.reader(), 4096);

    const timestamp = Timestamp{ .sec = 50, .nsec = 200 };
    try _serializer.serializeTimestamp(timestamp);

    const result = try _deserializer.deserializeTimestamp();

    try testing.expectEqual(timestamp, result);
}

test "(de)serialize ext format" {
    var buffer: [4096]u8 = undefined;
    var out = std.io.fixedBufferStream(&buffer);
    var _serializer = serializer(out.writer());

    var in = std.io.fixedBufferStream(&buffer);
    var _deserializer = deserializer(in.reader(), 4096);

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
    var _deserializer = deserializer(in.reader(), 4096);

    var decl = Decl{ .x = 10, .y = 50 };
    try _serializer.serialize(decl);

    const result = try _deserializer.deserialize(Decl);
    try testing.expectEqual(decl, result);
}
