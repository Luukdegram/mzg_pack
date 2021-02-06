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
	- [ ] Uf8 string (out of scope)
- [ ] deserialization
	- [ ] String
	- [ ] Integers
	- [ ] Nil
	- [ ] Floats
	- [ ] Bin
	- [ ] Array
	- [ ] Map
	- [ ] Bool
	- [ ] Ext
	- [ ] Timestamp
	- [ ] Uf8 string (out of scope)

## Example
Firstly, add `mzg_pack` to your `build.zig` file. Then you can serialize your Zig data using the following API.

```zig
const mp = @import("mzg_pack");

const MyStruct = struct {
	x: u64,
	string: []const u8,
};

pub fn main() !void {
	// create our msg pack object
	const msg_pack = mp.MsgPack(MyStruct);
	
	// use a writer, in this case we write to stdout
	const writer = std.io.getStdOut().writer();
	
	// define our data
	const to_serialize = MyStruct{
		.x = 23059,
		.string = "Hello, world!",
	};
	
	// Finally, serialize the data
	try msg_pack.serialize(to_serialize, writer);
}
```

