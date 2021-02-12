# mzg_pack

[Message pack](https://https://msgpack.org/) (de)serialization library for Zig.

## progress
- [ ] serialization
	- [x] String
	- [x] Integers
	- [x] Nil
	- [x] Floats
	- [x] Bin
	- [x] Array
	- [x] Map
	- [x] Bool
	- [ ] Ext
	- [ ] Timestamp
	- [ ] Uf8 string 
- [ ] deserialization
	- [x] String
	- [x] Integers
	- [x] Nil
	- [x] Floats
	- [x] Bin
	- [x] Array
	- [x] Map
	- [x] Bool
	- [ ] Ext
	- [ ] Timestamp
	- [ ] Uf8 string 

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

