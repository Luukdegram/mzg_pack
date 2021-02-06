const std = @import("std");
const testing = std.testing;
const max = std.math.maxInt;

/// Supported types to be serialized
const Format = enum(u8) {
    fixint_base = 0x00,
    fixint_max = 0x7F,
    fixmap_base = 0x80,
    fixmap_max = 0x8F,
    fixarray_base = 0x90,
    fixarray_max = 0x9F,
    fixstr_base = 0xA0,
    fixstr_max = 0xBF,
    nil = 0xC0,
    @"false" = 0xC2,
    @"true" = 0xC3,
    bin_8 = 0xC4,
    bin_16 = 0xC5,
    bin_32 = 0xC6,
    ext_8 = 0xC7,
    ext_16 = 0xC8,
    ext_32 = 0xC9,
    float_32 = 0xCA,
    float_64 = 0xCB,
    uint_8 = 0xCC,
    uint_16 = 0xCD,
    uint_32 = 0xCE,
    uint_64 = 0xCF,
    int_8 = 0xD0,
    int_16 = 0xD1,
    int_32 = 0xD2,
    int_64 = 0xD3,
    fixext_1 = 0xD4,
    fixext_2 = 0xD5,
    fixext_4 = 0xD6,
    fixext_8 = 0xD7,
    fixext_16 = 0xD8,
    str_8 = 0xD9,
    str_16 = 0xDA,
    str_32 = 0xDB,
    array_16 = 0xDC,
    array_32 = 0xDD,
    map_16 = 0xDE,
    map_32 = 0xDF,
    negfixint_base = 0xE0,
    negfixint_max = 0xFF,
    _,
};

/// Returns the integer value of `Format`.
/// allows you to use format(.bin_8) for readability.
fn format(format_enum: Format) u8 {
    return @enumToInt(format_enum);
}

/// Returns the fixmap value based on the input `value`
fn fixmap(value: u4) u8 {
    return value + @enumToInt(Format.fixmap_base);
}

/// Returns the fixstr value based on the input `value`
fn fixstr(value: u5) u8 {
    return value + @enumToInt(Format.fixstr_base);
}

/// Returns the fixarray value based on the input `value`
fn fixarray(value: u4) u8 {
    return value + @enumToInt(Format.fixarray_base);
}

/// Returns the negative fixint value based on the input `value`
fn negint(value: i5) u8 {
    return @intCast(u8, value) + @enumToInt(Format.negfixint_base);
}

pub const SerializeError = error{SliceTooLong};

pub fn MsgPack(comptime T: type) type {
    return struct {
        pub fn serialize(value: T, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            try writeType(T, value, writer);
        }
        pub fn deserialize(reader: anytype) @TypeOf(reader).Error!T {
            const byte = try reader.readByte();
            switch (byte) {
                Format.fixint_base...Format.fixint_max => {},
            }
        }

        fn writeType(comptime S: type, value: S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            switch (S) {
                []const u8 => try writeString(value, writer),
                []u8 => try writeBin(value, writer),
                else => switch (@typeInfo(S)) {
                    .Int => try writeInt(S, value, writer),
                    .Bool => try writeBool(value, writer),
                    .Float => try writeFloat(S, value, writer),
                    .Null => try writer.writeByte(format(.nil)),
                    .Struct => try writeMap(S, value, writer),
                    .Array => try writeArray(S, value, writer),
                    .Pointer => try writePointer(S, value, writer),
                    .Optional => |opt| try writeOptional(opt.child, value, writer),
                    else => @compileError("Unsupported type '" ++ @typeName(S) ++ "'"),
                },
            }
        }

        /// Serializes a string or byte array and writes it to the given `writer`
        fn writeString(value: []const u8, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            switch (len) {
                0...max(u5) => try writer.writeByte(fixstr(@intCast(u5, len))),
                max(u5) + 1...max(u8) => {
                    try writer.writeByte(format(.str_8));
                    try writer.writeByte(@intCast(u8, len));
                },
                max(u8) + 1...max(u16) => {
                    try writer.writeByte(format(.str_16));
                    try writer.writeIntBig(u16, @intCast(u16, len));
                },
                max(u16) + 1...max(u32) => {
                    try writer.writeByte(format(.str_32));
                    try writer.writeIntBig(u32, @intCast(u32, len));
                },
                else => unreachable,
            }
            try writer.writeAll(value);
        }

        /// Serializes an integer and writes it to the given `writer`
        fn writeInt(comptime S: type, value: S, writer: anytype) @TypeOf(writer).Error!void {
            const info = @typeInfo(S);
            const int = info.Int;
            const sign = int.signedness;

            if (sign == .unsigned and int.bits <= 7) return writer.writeByte(value);
            if (sign == .signed and int.bits <= 5) return writer.writeByte(negint(value));

            switch (int.bits) {
                0...8 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_8) else format(int_8));
                    try writer.writeByte(@intCast(u8, value));
                },
                9...16 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_16) else format(int_16));
                    try writer.writeIntBig(u16, @intCast(u16, value));
                },
                17...32 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_32) else format(int_32));
                    try writer.writeIntBig(u32, @intCast(u32, value));
                },
                33...64 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_64) else format(int_64));
                    try writer.writeIntBig(u64, @intCast(u64, value));
                },
            }
        }

        /// Serializes and writes the booleans value to the `writer`
        fn writeBool(value: bool, writer: anytype) @TypeOf(writer).Error!void {
            try writer.writeByte(if (value) format(.@"true") else format(.@"false"));
        }

        /// Serializes a 32 -or 64bit float and writes it to the `writer`
        /// TODO ensure big-endian byte order
        fn writeFloat(comptime S: type, value: S, writer: anytype) @TypeOf(writer).Error!void {
            try writer.writeByte(if (@typeInfo(S).Float.bits == 32)
                format(.float_32)
            else
                format(.float_64));

            try writer.writeAll(std.mem.asBytes(&value));
        }

        /// Serializes and writes a raw byte slice to the given `writer`
        fn writeBin(value: []const u8, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            switch (len) {
                0...max(u8) => try writer.writeByte(@intCast(u8, len)),
                max(u8) + 1...max(u8) => {
                    try writer.writeByte(format(.bin_8));
                    try writer.writeByte(@intCast(u8, len));
                },
                max(u8) + 1...max(u16) => {
                    try writer.writeByte(format(.bin_16));
                    try writer.writeIntBig(u16, @intCast(u16, len));
                },
                max(u16) + 1...max(u32) => {
                    try writer.writeByte(format(.bin_32));
                    try writer.writeIntBig(u32, @intCast(u32, len));
                },
                else => unreachable,
            }
            try writer.writeAll(value);
        }

        /// Serializes the value as an array and writes each element to the `writer`
        fn writeArray(comptime S: type, value: []const S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;

            switch (len) {
                0...max(u4) => try writer.writeByte(fixarray(@intCast(u4, len))),
                max(u4) + 1...max(u16) => {
                    try writer.writeByte(format(.array_16));
                    try writer.writeIntBig(u16, @intCast(u16, len));
                },
                max(u16) + 1...max(u32) => {
                    try writer.writeByte(format(.array_32));
                    try writer.writeIntBig(u32, @intCast(u32, len));
                },
                else => unreachable,
            }

            for (value) |val| try writeType(S, val, writer);
        }

        /// Serializes a pointer and writes its internal value to the `writer`
        fn writePointer(comptime S: type, value: S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const info = @typeInfo(S).Pointer;
            switch (info.size) {
                .One => switch (@typeInfo(info.child)) {
                    .Array => |array_info| try writeArray(array_info.child, value, writer),
                    .Vector => |vector_info| try writeArray(vector_info.child, value, writer),
                    else => {},
                },
                .Many, .C, .Slice => try writeArray(info.child, value, writer),
            }
        }

        fn writeOptional(comptime S: type, value: ?S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            if (value) |val| try writeType(S, val, writer) else try writer.writeByte(format(.nil));
        }

        /// Serializes a struct into a map type and writes its to the given `writer`
        fn writeMap(comptime S: type, value: S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const fields = std.meta.fields(S);
            const fields_len = fields.len;
            if (fields_len == 0) return;
            if (fields_len > max(u32)) return error.SliceTooLong;

            switch (fields_len) {
                0...max(u4) => try writer.writeByte(fixmap(@intCast(u4, fields_len))),
                max(u4) + 1...max(u16) => {
                    try writer.writeByte(format(.map_16));
                    try writer.writeIntBig(u16, @intCast(u16, len));
                },
                max(u16) + 1...max(u32) => {
                    try writer.writeByte(format(.array_32));
                    try writer.writeIntBig(u32, @intCast(u32, len));
                },
            }
            inline for (fields) |field| {
                try writeString(field.name, writer);
                try writeType(field.field_type, @field(value, field.name), writer);
            }
        }
    };
}

test "Serialization" {
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
    };

    inline for (test_cases) |case, i| {
        const msg_pack = MsgPack(case.type);
        var buffer: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);

        try msg_pack.serialize(case.value, stream.writer());
        testing.expectEqualSlices(u8, case.expected, stream.getWritten());
    }
}
