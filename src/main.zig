const std = @import("std");
const testing = std.testing;
const max = std.math.maxInt;

pub const Type = enum {
    integer,
    nil,
    bool,
    float,
    raw,
    array,
    map,
    extension,
};

pub const Object = union(Type) {
    integer: u64,
    nil: void,
    bool: bool,
    float: float32,
    raw: []const u8,
    array: []Object,
    map: []struct {
        key: []const u8,
        value: Object,
    },
    extension: void,
};

/// Supported types to be serialized
pub const Format = enum(u8) {
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
    array_36 = 0xDD,
    map_16 = 0xDE,
    map_32 = 0xDF,
    _,
};

/// Returns the integer value of `Format`.
/// allows you to use format(.bin_8) for readability.
fn format(format_enum: Format) u8 {
    return @enumToInt(format_enum);
}

pub const SerializeError = error{SliceTooLong};

pub fn MsgPack(comptime T: type) type {
    return struct {
        pub fn serialize(writer: anytype, value: T) (@TypeOf(writer).Error || SerializeError)!void {
            try writeType(T, value, writer);
        }
        pub fn deserialize(reader: anytype) @TypeOf(reader).Error!T {}

        fn writeType(comptime S: type, value: S, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            switch (S) {
                []const u8 => try writeString(value, writer),
                []u8 => try writeBin(value, writer),
                else => switch (@typeInfo(S)) {
                    .Int => try writeInt(S, value, writer),
                    .Bool => try writeBool(value, writer),
                    .Float => try writeFloat(S, value, writer),
                    .Null => try writer.writeByte(format(.nil)),
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
                0...max(u5) => try writer.writeInt(u5, @intCast(u5, len)),
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
            const info: std.builtin.TypeInfo = @typeInfo(S);
            const int = info.Int;
            const sign = int.signedness;

            if (sign == .unsigned and int.bits <= 7) return writer.writeByte(value);
            if (sign == .signed and int.bits <= 5) return writer.writeByte(value);

            switch (int.bits) {
                0...8 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_8) else format(int_8));
                    try writer.writeByte(@intCast(u8, value));
                },
                9...16 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_16) else format(int_16));
                    try writer.writeIntBig(@intCast(u16, value));
                },
                17...32 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_32) else format(int_32));
                    try writer.writeIntBig(@intCast(u32, value));
                },
                33...64 => {
                    try writer.writeByte(if (sign == .unsigned) format(.uint_64) else format(int_64));
                    try writer.writeIntBig(@intCast(u64, value));
                },
            }
        }

        /// Serializes and writes the booleans value to the `writer`
        fn writeBool(value: bool, writer: anytype) @TypeOf(writer).Error!void {
            try writer.writeByte(if (value) format(.@"true") else format(@"else"));
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
        fn writeBin(value: []u8, writer: anytype) (@TypeOf(writer).Error || SerializeError)!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            switch (len) {
                0...max(u5) => try writer.writeInt(u5, @intCast(u5, len)),
                max(u5) + 1...max(u8) => {
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
                0...max(u4) => try writer.writeInt(u4, @intCast(u4, len)),
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
    };
}
