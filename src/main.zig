const std = @import("std");
const meta = std.meta;
const trait = meta.trait;
const max = std.math.maxInt;
const assert = std.debug.assert;

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

fn fromByte(byte: u8) Format {
    return @intToEnum(Format, byte);
}

/// Represents a timestamp that can be (de)serialized
pub const Timestamp = struct { sec: u32, nsec: i64 };

/// Creates a generic Deserializater over the given ReaderType.
/// Allows for deserializing msgpack data streams into given types using a buffer
/// of given `size`.
///
/// It is possible to define your custom deserialization function for a given type
/// by defining a `deserialize(*Self, anytype)` function
pub fn Deserializer(comptime ReaderType: type) type {
    return struct {
        reader: ReaderType,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub const ReadError = error{
            MismatchingFormatType,
            EndOfStream,
        } || ReaderType.Error || std.mem.Allocator.Error;

        /// Initializes a new instance wrapper around the given `reader`
        pub fn init(reader: ReaderType, allocator: std.mem.Allocator) Self {
            return Self{ .reader = reader, .allocator = allocator };
        }

        /// Deserializes the msgpack stream into a value of type `T`
        /// use `deserializeInto` if the length written to `buffer` is required
        /// Note: Sets `index` to 0 to reuse the buffer and will overwrite all old data
        pub fn deserialize(self: *Self, comptime T: type) ReadError!T {
            var value: T = undefined;
            _ = try self.deserializeInto(&value);
            return value;
        }

        /// Deserializes the msgpack data stream into the given pointer.
        /// asserts `ptr` is a pointer
        pub fn deserializeInto(self: *Self, ptr: anytype) ReadError!void {
            const T = @TypeOf(ptr);
            comptime assert(trait.is(.Pointer)(T));

            const C = comptime meta.Child(T);

            if (comptime trait.hasFn("deserialize")(C)) return C.deserialize(ptr, self);

            ptr.* = switch (C) {
                []const u8 => try self.deserializeString(),
                []u8 => try self.deserializeBin(),
                else => switch (@typeInfo(C)) {
                    .Struct => try self.deserializeStruct(C),
                    .Bool => try self.deserializeBool(),
                    .Int => try self.deserializeInt(C),
                    .Float => try self.deserializeFloat(C),
                    .Array => try self.deserializeArray(C),
                    .Pointer => try self.deserializePointer(C),
                    .Null => try self.deserializeNull(),
                    else => @compileError("Unsupported deserialization type " ++ @typeName(C) ++ "\n"),
                },
            };
        }

        /// Deserializes a pointer to one or multiple items
        pub fn deserializePointer(self: *Self, comptime T: type) ReadError!T {
            return switch (@typeInfo(T).Pointer.size) {
                .One => try self.deserialize(T),
                .Slice => try self.deserializeArray(T),
                .Many => try self.deserializeArray(T),
                .C => @compileError("Unsupported pointer type C"),
            };
        }

        /// Deserializes a timestamp into a struct of sec(u32) and nsec(i64)
        pub fn deserializeTimestamp(self: *Self) ReadError!Timestamp {
            const byte = try self.reader.readByte();

            switch (byte) {
                format(.fixext_4) => {
                    _ = try self.reader.readByte(); // skip '-1' byte
                    const sec = try self.reader.readIntBig(u32);
                    return Timestamp{ .sec = sec, .nsec = 0 };
                },
                format(.fixext_8) => {
                    _ = try self.reader.readByte(); // skip '-1' byte
                    const data = try self.reader.readIntBig(u64);
                    return Timestamp{ .sec = @intCast(u32, data & 0x00000003ffffffff), .nsec = @intCast(i64, data >> 34) };
                },
                format(.ext_8) => {
                    _ = try self.reader.readIntBig(u16); // skip first 2 bytes
                    const sec = try self.reader.readIntBig(u32);
                    const nsec = try self.reader.readIntBig(i64);
                    return Timestamp{ .sec = sec, .nsec = nsec };
                },
                else => return error.MismatchingFormatType,
            }
        }

        /// Deserializes the data stream to `null`. Returns an error
        /// if the data does not correspond to the right format
        pub fn deserializeNull(self: *Self) ReadError!null {
            const byte = try self.reader.readByte();
            if (byte != format(.nil)) return error.MismatchingFormatType;
            return null;
        }

        /// Deserializes a msgpack map into the given struct of type `T`
        pub fn deserializeStruct(self: *Self, comptime T: type) ReadError!T {
            const info = @typeInfo(T);
            if (info != .Struct) @compileError("Given type '" ++ @typeName(T) ++ "' is not a struct");

            const byte = try self.reader.readByte();
            const len: u32 = switch (byte) {
                format(.fixmap_base)...format(.fixmap_max) => byte - format(.fixmap_base),
                format(.map_16) => try self.reader.readIntBig(u16),
                format(.map_32) => try self.reader.readIntBig(u32),
                else => return error.MismatchingFormatType,
            };
            var value: T = undefined;
            var i: usize = 0;
            loop: while (i < len) : (i += 1) {
                const key = try self.deserializeString();

                inline for (meta.fields(T)) |struct_field| {
                    if (std.mem.eql(u8, struct_field.name, key)) {
                        try self.deserializeInto(&@field(value, struct_field.name));
                        continue :loop;
                    }
                }
                return error.MismatchingFormatType;
            }
            return value;
        }

        /// Deserializes a msgpack array into the given type `T`.
        pub fn deserializeArray(self: *Self, comptime T: type) ReadError!T {
            const info = @typeInfo(T);
            if (comptime !trait.isSlice(T) and info != .Array)
                @compileError("Expected given type to be an array or slice, but instead got '" ++ @typeName(T) ++ "'");

            const byte = try self.reader.readByte();
            const len: u32 = switch (byte) {
                format(.fixarray_base)...format(.fixarray_max) => byte - format(.fixarray_base),
                format(.array_16) => try self.reader.readIntBig(u16),
                format(.array_32) => try self.reader.readIntBig(u32),
                else => return error.MismatchingFormatType,
            };

            if (comptime trait.isSlice(T)) {
                var t_buf = try self.allocator.alloc(meta.Child(T), len);
                for (t_buf) |*v| {
                    try self.deserializeInto(v);
                }
                return t_buf;
            } else {
                const array_len = info.Array.len;
                var t_buf: [array_len]info.Array.child = undefined;
                for (t_buf) |*v| try self.deserializeInto(v);
                return t_buf;
            }
        }

        /// Deserializes a msgpack string into a string
        pub fn deserializeString(self: *Self) ReadError![]u8 {
            const string_byte = try self.reader.readByte();

            const len: u32 = switch (string_byte) {
                format(.fixstr_base)...format(.fixstr_max) => string_byte - format(.fixstr_base),
                format(.str_8) => try self.reader.readByte(),
                format(.str_16) => try self.reader.readIntBig(u16),
                format(.str_32) => try self.reader.readIntBig(u32),
                else => return error.MismatchingFormatType,
            };

            var buffer = try self.allocator.alloc(u8, len);
            var actual_len = try self.reader.readAll(buffer);

            if (actual_len < len) return error.EndOfStream;

            return buffer;
        }

        /// Deserializes a msgpack binary data format serialized stream into a slice of bytes
        pub fn deserializeBin(self: *Self) ReadError![]u8 {
            const byte = try self.reader.readByte();

            const len: u32 = switch (byte) {
                format(.bin_8) => try self.reader.readByte(),
                format(.bin_16) => try self.reader.readIntBig(u16),
                format(.bin_32) => try self.reader.readIntBig(u32),
                else => return error.MismatchingFormatType,
            };

            var buffer = try self.allocator.alloc(u8, len);
            var actual_len = try self.reader.readAll(buffer);

            if (actual_len < len) return error.EndOfStream;

            return buffer;
        }

        /// Deserializes the msgpack data into a boolean.
        pub fn deserializeBool(self: *Self) ReadError!bool {
            return switch (try self.reader.readByte()) {
                format(.@"true") => true,
                format(.@"false") => false,
                else => return error.MismatchingFormatType,
            };
        }

        /// Deserializes the data stream into an integer of type `T`.
        /// Returns `error.MismatchingFormatType` when the stream contains a different
        /// data type than `T`.
        pub fn deserializeInt(self: *Self, comptime T: type) ReadError!T {
            if (@typeInfo(T) != .Int) @compileError("Expected integer type, but found type '" ++ @typeName(T) ++ "'");

            return if (comptime trait.isSignedInt(T))
                self.deserializeSignedInt(T)
            else
                self.deserializeUnsignedInt(T);
        }

        /// Deserializes the data into an unsigned integer of type `T`
        pub fn deserializeUnsignedInt(self: *Self, comptime T: type) ReadError!T {
            if (comptime !trait.isUnsignedInt(T)) @compileError("Given type '" ++ @typeName(T) ++ "' is not an unsigned integer");

            const byte = try self.reader.readByte();
            const bits = meta.bitCount(T);

            switch (byte) {
                format(.fixint_base)...format(.fixint_max) => return if (bits > 7) @intCast(T, byte) else @truncate(T, byte),
                format(.uint_8) => {
                    const result = try self.reader.readByte();
                    return if (bits > 8) @intCast(T, result) else @truncate(T, result);
                },
                format(.uint_16) => {
                    const result = try self.reader.readIntBig(u16);
                    return if (bits > 16) @intCast(T, result) else @truncate(T, result);
                },
                format(.uint_32) => {
                    const result = try self.reader.readIntBig(u32);
                    return if (bits > 32) @intCast(T, result) else @truncate(T, result);
                },
                format(.uint_64) => {
                    const result = try self.reader.readIntBig(u64);
                    return if (bits > 64) @intCast(T, result) else @truncate(T, result);
                },
                else => return error.MismatchingFormatType,
            }
        }

        /// Deserializes the data into a signed integer of type `T`
        pub fn deserializeSignedInt(self: *Self, comptime T: type) ReadError!T {
            if (comptime !trait.isSignedInt(T)) @compileError("Given type '" ++ @typeName(T) ++ "' is not a signed integer");

            const byte = try self.reader.readByte();
            const bits = meta.bitCount(T);

            switch (byte) {
                format(.negfixint_base)...format(.negfixint_max) => return if (bits > 5) @intCast(T, byte) else @truncate(T, byte),
                format(.int_8) => {
                    const result = try self.reader.readIntBig(i8);
                    return if (bits > 8) @intCast(T, result) else @truncate(T, result);
                },
                format(.int_16) => {
                    const result = try self.reader.readIntBig(i16);
                    return if (bits > 16) @intCast(T, result) else @truncate(T, result);
                },
                format(.int_32) => {
                    const result = try self.reader.readIntBig(i32);
                    return if (bits > 32) @intCast(T, result) else @truncate(T, result);
                },
                format(.int_64) => {
                    const result = try self.reader.readIntBig(i64);
                    return if (bits > 64) @intCast(T, result) else @truncate(T, result);
                },
                else => return error.MismatchingFormatType,
            }
        }

        /// Desiserializes the serialized data into `T` which must be of type `f32` or `f64`
        pub fn deserializeFloat(self: *Self, comptime T: type) ReadError!T {
            comptime assert(trait.isFloat(T));
            const float = try self.reader.readByte();

            switch (float) {
                format(.float_32) => {
                    if (T != f32) return error.MismatchingFormatType;
                    return @bitCast(T, try self.reader.readIntBig(u32));
                },
                format(.float_64) => {
                    if (T != f64) return error.MismatchingFormatType;
                    return @bitCast(T, try self.reader.readIntBig(u64));
                },
                else => return error.MismatchingFormatType,
            }
        }

        /// Deserializes extension data and sets the given `type` value
        pub fn deserializeExt(self: *Self, data_type: *i8) ReadError![]const u8 {
            const reader = self.reader;
            const byte = try reader.readByte();

            const len: u32 = switch (byte) {
                format(.fixext_1) => 1,
                format(.fixext_2) => 2,
                format(.fixext_4) => 4,
                format(.fixext_8) => 8,
                format(.fixext_16) => 16,
                format(.ext_8) => try reader.readByte(),
                format(.ext_16) => try reader.readIntBig(u16),
                format(.ext_32) => try reader.readIntBig(u32),
                else => return error.MismatchingFormatType,
            };

            data_type.* = try reader.readIntBig(i8);
            var buffer = try self.allocator.alloc(u8, len);
            var actual_len = try reader.readAll(buffer);

            if (actual_len < len) return error.EndOfStream;

            return buffer;
        }

        /// Resets the internal buffer of `Self`. Calling any of the deserializing functions
        /// after this will rewrite the buffer starting at index 0.
        pub fn reset(self: *Self) void {
            self.index = 0;
        }
    };
}

/// returns a new `Deserializer` for the type of the given `reader`
pub fn deserializer(reader: anytype, alloc: std.mem.Allocator) Deserializer(@TypeOf(reader)) {
    return Deserializer(@TypeOf(reader)).init(reader, alloc);
}

/// Generic function that wraps around the given `WriterType`.
/// Serializes given values into msgpack format
///
/// Custom serialization functions can be provided by declaring
/// `serialize(*Self, anytype)!void` function
pub fn Serializer(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        writer: WriterType,

        pub const WriteError = error{ SliceTooLong, integerTooBig, InvalidFloatSize } || WriterType.Error;

        /// Initializes a new instance of `Serializer(WriterType)`
        pub fn init(writer: WriterType) Self {
            return .{ .writer = writer };
        }

        /// Serializes the given value into msgpack format and writes it `WriterType`
        pub fn serialize(self: *Self, value: anytype) WriteError!void {
            try self.serializeTyped(@TypeOf(value), value);
        }

        /// Serializes the given type `S` into msgpack format and writes it to the writer
        /// of type `WriterType`
        pub fn serializeTyped(self: *Self, comptime S: type, value: S) WriteError!void {
            if (comptime trait.hasFn("serialize")(S))
                return value.serialize(self);

            switch (S) {
                []const u8 => try self.serializeString(value),
                []u8 => try self.serializeBin(value),
                else => switch (@typeInfo(S)) {
                    .Int => try self.serializeInt(S, value),
                    .Bool => try self.serializeBool(value),
                    .Float => try self.serializeFloat(S, value),
                    .Null => try self.serializeNull(),
                    .Struct => try self.serializeStruct(S, value),
                    .Array => try self.serializeArray(S, value),
                    .Pointer => try self.serializePointer(S, value),
                    .Optional => |opt| try self.serializeOptional(opt.child, value),
                    else => @compileError("Unsupported type '" ++ @typeName(S) ++ "'"),
                },
            }
        }

        /// Serializes a 'nil' byte
        pub fn serializeNull(self: *Self) WriteError!void {
            try self.writer.writeByte(format(.nil));
        }

        /// Serializes a timestamp for the given `timestamp` value
        pub fn serializeTimestamp(self: *Self, timestamp: Timestamp) WriteError!void {
            const writer = self.writer;
            if (@as(u64, timestamp.sec) >> 34 == 0) {
                const data: u64 = (@intCast(u64, timestamp.nsec) << 34) | timestamp.sec;

                if (data & 0xffffffff00000000 == 0) {
                    try writer.writeByte(format(.fixext_4));
                    try writer.writeIntBig(i8, -1);
                    try writer.writeIntBig(u32, @intCast(u32, data));
                } else {
                    try writer.writeByte(format(.fixext_8));
                    try writer.writeIntBig(i8, -1);
                    try writer.writeIntBig(u64, data);
                }
            } else {
                try writer.writeByte(format(.ext_8));
                try writer.writeByte(12);
                try writer.writeIntBig(i8, -1);
                try writer.writeIntBig(u32, timestamp.sec);
                try writer.writeIntBig(i64, timestamp.nsec);
            }
        }

        /// Serializes a string or byte array and writes it to the given `writer`
        pub fn serializeString(self: *Self, value: []const u8) WriteError!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            switch (len) {
                0...max(u5) => try self.writer.writeByte(fixstr(@intCast(u5, len))),
                max(u5) + 1...max(u8) => {
                    try self.writer.writeByte(format(.str_8));
                    try self.writer.writeByte(@intCast(u8, len));
                },
                max(u8) + 1...max(u16) => {
                    try self.writer.writeByte(format(.str_16));
                    try self.writer.writeIntBig(u16, @intCast(u16, len));
                },
                max(u16) + 1...max(u32) => {
                    try self.writer.writeByte(format(.str_32));
                    try self.writer.writeIntBig(u32, @intCast(u32, len));
                },
                else => unreachable,
            }
            try self.writer.writeAll(value);
        }

        /// Serializes a signed or unsigned integer
        pub fn serializeInt(self: *Self, comptime S: type, value: S) WriteError!void {
            return if (comptime trait.isSignedInt(S))
                self.serializeSignedInt(S, value)
            else
                self.serializeUnsignedInt(S, value);
        }

        /// Serializes an unsigned integer
        pub fn serializeUnsignedInt(self: *Self, comptime S: type, value: S) WriteError!void {
            if (comptime !trait.isUnsignedInt(S))
                @compileError("Expected unsigned integer, but instead found type '" ++ @typeName(S) ++ "'");
            switch (comptime meta.bitCount(S)) {
                0...7 => try self.writer.writeByte(value),
                8 => {
                    try self.writer.writeByte(format(.uint_8));
                    try self.writer.writeByte(value);
                },
                9...16 => {
                    try self.writer.writeByte(format(.uint_16));
                    try self.writer.writeIntBig(u16, value);
                },
                17...32 => {
                    try self.writer.writeByte(format(.uint_32));
                    try self.writer.writeIntBig(u32, value);
                },
                33...64 => {
                    try self.writer.writeByte(format(.uint_64));
                    try self.writer.writeIntBig(u64, value);
                },
                else => return error.integerTooBig,
            }
        }

        /// Serializes and checks a signed integer
        pub fn serializeSignedInt(self: *Self, comptime S: type, value: S) WriteError!void {
            if (comptime !trait.isSignedInt(S))
                @compileError("Expected signed integer, but instead found type '" ++ @typeName(S) ++ "'");

            switch (comptime meta.bitCount(S)) {
                0...5 => try self.writer.writeByte(negint(value)),
                5...8 => {
                    try self.writer.writeByte(format(.int_8));
                    try self.writer.writeIntBig(i8, value);
                },
                8...16 => {
                    try self.writer.writeByte(format(.int_16));
                    try self.writer.writeIntBig(i16, value);
                },
                16...32 => {
                    try self.writer.writeByte(format(.int_32));
                    try self.writer.writeIntBig(i32, value);
                },
                32...64 => {
                    try self.writer.writeByte(format(.int_64));
                    try self.writer.writeIntBig(i64, value);
                },
                else => return error.integerTooBig,
            }
        }

        /// Serializes and writes the booleans value to the `writer`
        pub fn serializeBool(self: *Self, value: bool) WriteError!void {
            try self.writer.writeByte(if (value) format(.@"true") else format(.@"false"));
        }

        /// Serializes a 32 -or 64bit float and writes it to the `writer`
        /// TODO ensure big-endian byte order
        pub fn serializeFloat(self: *Self, comptime S: type, value: S) WriteError!void {
            comptime assert(trait.isFloat(S));
            switch (@typeInfo(S).Float.bits) {
                32 => {
                    try self.writer.writeByte(format(.float_32));
                    try self.writer.writeIntBig(u32, @bitCast(u32, value));
                },
                64 => {
                    try self.writer.writeByte(format(.float_64));
                    try self.writer.writeIntBig(u64, @bitCast(u64, value));
                },
                else => {
                    return error.InvalidFloatSize;
                },
            }
        }

        /// Serializes and writes a raw byte slice to the given `writer`
        pub fn serializeBin(self: *Self, value: []const u8) WriteError!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            const writer = self.writer;

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
        pub fn serializeArray(self: *Self, comptime S: type, value: S) WriteError!void {
            const len = value.len;
            if (len == 0) return;
            if (len > max(u32)) return error.SliceTooLong;
            const writer = self.writer;

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

            for (value) |val| try self.serializeTyped(meta.Child(S), val);
        }

        /// Serializes a pointer and writes its internal value to the `writer`
        pub fn serializePointer(self: *Self, comptime S: type, value: S) WriteError!void {
            switch (@typeInfo(S).Pointer.size) {
                .One => try self.serializeTyped(S, value),
                .Many, .C, .Slice => try self.serializeArray(S, value),
            }
        }

        /// Serializes the given value to 'nil' if `null` or typed if the optional is not `null`
        pub fn serializeOptional(self: *Self, comptime S: type, value: ?S) WriteError!void {
            if (value) |val| try self.serializeTyped(S, val) else try self.writer.writeByte(format(.nil));
        }

        /// Serializes a struct into a map type and writes its to the given `writer`
        pub fn serializeStruct(self: *Self, comptime S: type, value: S) WriteError!void {
            comptime assert(@typeInfo(S) == .Struct);
            const fields = meta.fields(S);
            const fields_len = fields.len;
            comptime assert(fields_len <= max(u32));

            if (fields_len == 0) return;
            const writer = self.writer;

            switch (fields_len) {
                0...max(u4) => try writer.writeByte(fixmap(@intCast(u4, fields_len))),
                max(u4) + 1...max(u16) => {
                    try writer.writeByte(format(.map_16));
                    try writer.writeIntBig(u16, @intCast(u16, fields_len));
                },
                max(u16) + 1...max(u32) => {
                    try writer.writeByte(format(.array_32));
                    try writer.writeIntBig(u32, @intCast(u32, fields_len));
                },
            }
            inline for (fields) |field| {
                try self.serializeString(field.name);
                try self.serializeTyped(field.field_type, @field(value, field.name));
            }
        }

        /// Serializes extension data
        pub fn serializeExt(
            self: *Self,
            data_type: i8,
            data: []const u8,
        ) WriteError!void {
            const writer = self.writer;
            const format_byte = if (data.len <= 16 and std.math.isPowerOfTwo(data.len)) switch (data.len) {
                1 => format(.fixext_1),
                2 => format(.fixext_2),
                4 => format(.fixext_4),
                8 => format(.fixext_8),
                16 => format(.fixext_16),
                else => unreachable,
            } else switch (data.len) {
                0...max(u8) => format(.ext_8),
                max(u8) + 1...max(u16) => format(.ext_16),
                max(u16) + 1...max(u32) => format(.ext_32),
                else => return error.SliceTooLong,
            };

            try writer.writeByte(format_byte);
            if (data.len > 16 or !std.math.isPowerOfTwo(data.len)) {
                switch (data.len) {
                    0...max(u8) => try writer.writeByte(@intCast(u8, data.len)),
                    max(u8) + 1...max(u16) => try writer.writeIntBig(u16, @intCast(u16, data.len)),
                    max(u16) + 1...max(u32) => try writer.writeIntBig(u32, @intCast(u32, data.len)),
                    else => unreachable,
                }
            }
            try writer.writeIntBig(i8, data_type);
            try writer.writeAll(data);
        }
    };
}

/// Function that creates a new generic that wraps around the given writer.
/// Serializes given values into msgpack format
///
/// Custom serialization functions can be provided by declaring
/// `serialize(*Self, anytype)!void` function
pub fn serializer(writer: anytype) Serializer(@TypeOf(writer)) {
    return Serializer(@TypeOf(writer)).init(writer);
}

test {
    _ = @import("tests.zig");
}
