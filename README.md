# mzg_pack

[Message pack](https://https://msgpack.org/) (de)serialization library for Zig.

## progress
- [x] serialization
	- [x] String
	- [x] Integers
	- [x] Nil
	- [x] Floats
	- [x] Bin
	- [x] Array
	- [x] Map
	- [x] Bool
	- [x] Ext
	- [x] Timestamp
- [x] deserialization
	- [x] String
	- [x] Integers
	- [x] Nil
	- [x] Floats
	- [x] Bin
	- [x] Array
	- [x] Map
	- [x] Bool
	- [x] Ext
	- [x] Timestamp

## Example
```zig
const msg_pack = @import("mzg_pack");

const MyStruct = struct {
    x: u64,
    string: []const u8,
};

pub fn main() !void {
    const serializer = msg_pack.serializer(some_given_writer);
    const deserializer = msg_pack.deserializer(some_given_reader, 4096); // 4096 is the internal buffer size
    
    // define our data
    const to_serialize = MyStruct{
        .x = 23059,
        .string = "Hello, world!",
    };

    // Finally, serialize the data
    try serializer.serialize(to_serialize);
    
    const result = try deserializer.deserialize(MyStruct); // result == to_serialize
}
```

It is also possible to provide custom (de)serializer functions on a container
which will be called instead:
```zig
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
    testing.expectEqual(decl, result);
}
```

## API
Serialization
```zig
/// Generic function that wraps around the given `WriterType`.
/// Serializes given values into msgpack format
///
/// Custom serialization functions can be provided by declaring
/// `serialize(*Self, anytype)!void` function
pub fn Serializer(comptime WriterType: type) type

/// Initializes a new instance of `Serializer(WriterType)`
pub fn init(writer: WriterType) Self

/// Serializes the given value into msgpack format and writes it `WriterType`
pub fn serialize(self: *Self, value: anytype) WriteError!void

/// Serializes the given type `S` into msgpack format and writes it to the writer
/// of type `WriterType`
pub fn serializeTyped(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes a 'nil' byte
pub fn serializeNull(self: *Self) WriteError!void

/// Serializes a timestamp for the given `timestamp` value
pub fn serializeTimestamp(self: *Self, timestamp: Timestamp) WriteError!void

/// Serializes a string or byte array and writes it to the given `writer`
pub fn serializeString(self: *Self, value: []const u8) WriteError!void

/// Serializes a signed or unsigned integer
pub fn serializeInt(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes an unsigned integer
pub fn serializeUnsignedInt(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes and checks a signed integer
pub fn serializeSignedInt(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes and writes the booleans value to the `writer`
pub fn serializeBool(self: *Self, value: bool) WriteError!void

/// Serializes a 32 -or 64bit float and writes it to the `writer`
/// TODO ensure big-endian byte order
pub fn serializeFloat(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes and writes a raw byte slice to the given `writer`
pub fn serializeBin(self: *Self, value: []const u8) WriteError!void

/// Serializes the value as an array and writes each element to the `writer`
pub fn serializeArray(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes a pointer and writes its internal value to the `writer`
pub fn serializePointer(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes the given value to 'nil' if `null` or typed if the optional is not `null`
pub fn serializeOptional(self: *Self, comptime S: type, value: ?S) WriteError!void

/// Serializes a struct into a map type and writes its to the given `writer`
pub fn serializeStruct(self: *Self, comptime S: type, value: S) WriteError!void

/// Serializes extension data
pub fn serializeExt(self: *Self, data_type: i8, data: []const u8) WriteError!void

/// Function that creates a new generic that wraps around the given writer.
/// Serializes given values into msgpack format
///
/// Custom serialization functions can be provided by declaring
/// `serialize(*Self, anytype)!void` function
pub fn serializer(writer: anytype) Serializer(@TypeOf(writer))
```

Deserialization
```zig
/// Creates a generic Deserializater over the given ReaderType.
/// Allows for deserializing msgpack data streams into given types using a buffer
/// of given `size`.
///
/// It is possible to define your custom deserialization function for a given type
/// by defining a `deserialize(*Self, anytype)` function
pub fn Deserializer(comptime ReaderType: type, comptime size: usize) type 

/// Initializes a new instance wrapper around the given `reader`
pub fn init(reader: ReaderType) Self

/// Deserializes the msgpack stream into a value of type `T`
/// use `deserializeInto` if the length written to `buffer` is required
/// Note: Sets `index` to 0 to reuse the buffer and will overwrite all old data
pub fn deserialize(self: *Self, comptime T: type) ReadError!T

/// Deserializes the msgpack data stream into the given pointer.
/// asserts `ptr` is a pointer
pub fn deserializeInto(self: *Self, ptr: anytype) ReadError!void

/// Deserializes a pointer to one or multiple items
pub fn deserializePointer(self: *Self, comptime T: type) ReadError!T

/// Deserializes a timestamp into a struct of sec(u32) and nsec(i64)
pub fn deserializeTimestamp(self: *Self) ReadError!Timestamp

/// Deserializes the data stream to `null`. Returns an error
/// if the data does not correspond to the right format
pub fn deserializeNull(self: *Self) ReadError!null

/// Deserializes a msgpack map into the given struct of type `T`
pub fn deserializeStruct(self: *Self, comptime T: type) ReadError!T

/// Deserializes a msgpack array into the given type `T`.
pub fn deserializeArray(self: *Self, comptime T: type) ReadError!T

/// Deserializes a msgpack string into a string
pub fn deserializeString(self: *Self) ReadError![]const u8

/// Deserializes a msgpack binary data format serialized stream into a slice of bytes
pub fn deserializeBin(self: *Self) ReadError![]u8

/// Deserializes the msgpack data into a boolean.
pub fn deserializeBool(self: *Self) ReadError!bool

/// Deserializes the data stream into an integer of type `T`.
/// Returns `error.MismatchingFormatType` when the stream contains a different
/// data type than `T`.
pub fn deserializeInt(self: *Self, comptime T: type) ReadError!T

/// Deserializes the data into an unsigned integer of type `T`
pub fn deserializeUnsignedInt(self: *Self, comptime T: type) ReadError!T

/// Deserializes the data into a signed integer of type `T`
pub fn deserializeSignedInt(self: *Self, comptime T: type) ReadError!T

/// Desiserializes the serialized data into `T` which must be of type `f32` or `f64`
pub fn deserializeFloat(self: *Self, comptime T: type) ReadError!T

/// Deserializes extension data and sets the given `type` value
pub fn deserializeExt(self: *Self, data_type: *i8) ReadError![]const u8

/// Resets the internal buffer of `Self`. Calling any of the deserializing functions
/// after this will rewrite the buffer starting at index 0.
pub fn reset(self: *Self) void

/// returns a new `Deserializer` for the type of the given `reader`
pub fn deserializer(reader: anytype, comptime size: usize) Deserializer(@TypeOf(reader), size)
```
