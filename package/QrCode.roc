## QR codes (ISO/IEC 18004) in pure Roc.
##
## ```roc
## qr = QrCode.encode("https://www.roc-lang.org", Medium)?
## qr.size() # 25, the modules on each side
## qr.dark(0, 0) # Bool.True, the corner of a finder pattern
## ```
##
## `encode` picks the most compact mode the whole text fits: numeric for
## digits only, alphanumeric for digits, capital letters, space and
## `$%*+-./:`, and bytes for anything else. It then picks the smallest version
## that holds the data at the chosen error correction level, and the mask with
## the lowest penalty score of the standard. It follows Nayuki's reference
## implementation (https://www.nayuki.io/page/qr-code-generator-library).
##
## A `QrCode` is the grid of modules and nothing else: how large to draw a
## module and the light border around the code, at least four modules wide,
## are up to whoever draws it.
QrCode :: { ver : U64, correction : [Low, Medium, Quartile, High], pattern : U8, grid : Grid, modules : List(Bool) }.{

	## Two codes are equal when they have the same version, level, mask and
	## modules.
	is_eq : _

	## How much of the code may be damaged or covered before it stops being
	## readable: about 7 % at `Low`, 15 % at `Medium`, 25 % at `Quartile` and
	## 30 % at `High`. A higher level makes a larger code for the same data.
	Level : [Low, Medium, Quartile, High]

	## The QR code of `text` at error correction `level`, in the most compact
	## mode the whole text fits. `TooLong` says how many bytes of UTF-8 the
	## text has and the most that fit in the largest code, version 40, in that
	## mode at that level.
	encode : Str, Level -> Try(QrCode, [TooLong({ bytes : U64, most : U64 })])
	encode = |text, correction| {
		bytes = text.to_utf8()
		build(bytes, mode_of(bytes), correction)
	}

	## The QR code of `bytes` in byte mode, whatever they are.
	encode_bytes : List(U8), Level -> Try(QrCode, [TooLong({ bytes : U64, most : U64 })])
	encode_bytes = |bytes, correction| build(bytes, Byte, correction)

	## The same code with mask `pattern`, from 0 to 7, in place of the one
	## with the lowest penalty. Every mask gives a valid code. Encoders differ
	## in the mask they pick, so this is for comparing with another encoder.
	with_mask : QrCode, U8 -> Try(QrCode, [NoSuchMask(U8)])
	with_mask = |qr, pattern|
		if pattern > 7 {
			Err(NoSuchMask(pattern))
		} else {
			code = fields(qr)
			Ok(masked(code.grid, code.ver, code.correction, pattern))
		}

	## The version, from 1 to 40. A code of version `n` has `4 * n + 17`
	## modules on each side.
	version : QrCode -> U64
	version = |qr| fields(qr).ver

	## The error correction level.
	level : QrCode -> Level
	level = |qr| fields(qr).correction

	## The mask, from 0 to 7.
	mask : QrCode -> U8
	mask = |qr| fields(qr).pattern

	## The number of modules on each side.
	size : QrCode -> U64
	size = |qr| fields(qr).grid.side

	## Whether the module in column `x` of row `y` is dark, counting from the
	## top left corner. Outside the code it is light.
	dark : QrCode, U64, U64 -> Bool
	dark = |qr, x, y| {
		code = fields(qr)
		side = code.grid.side
		if x < side and y < side code.modules.get(y * side + x) ?? Bool.False else Bool.False
	}

	## The rows, top first, `#` for a dark module and `.` for a light one.
	rows : QrCode -> List(Str)
	rows = |qr| {
		code = fields(qr)
		side = code.grid.side
		indices(side).map(
			|y| Str.from_utf8_lossy(code.modules.sublist({ start: y * side, len: side }).map(|on| if on '#' else '.')),
		)
	}
}

fields : QrCode -> { ver : U64, correction : [Low, Medium, Quartile, High], pattern : U8, grid : Grid, modules : List(Bool) }
fields = |QrCode.(code)| code

Mode : [Numeric, Alphanumeric, Byte]

build : List(U8), Mode, QrCode.Level -> Try(QrCode, [TooLong({ bytes : U64, most : U64 })])
build = |bytes, mode, correction| {
	ver = smallest_version(bytes.len(), mode, correction) ? |_| TooLong({ bytes: bytes.len(), most: most_characters(mode, correction) })
	grid = place_data(function_patterns(ver), codewords(ver, correction, mode, bytes))
	penalties = indices(8).map(|m| (m.to_u8_wrap(), penalty(fields(masked(grid, ver, correction, m.to_u8_wrap())).modules, grid.side)))
	(best, _) = penalties.fold((0, U64.highest), |(best_pattern, best_score), (pattern, score)| if score < best_score (pattern, score) else (best_pattern, best_score))
	Ok(masked(grid, ver, correction, best))
}

## The most compact mode for all of `bytes`.
mode_of : List(U8) -> Mode
mode_of = |bytes|
	if bytes.all(|byte| byte >= '0' and byte <= '9') {
		Numeric
	} else if bytes.all(|byte| alphanumeric_value(byte).is_ok()) {
		Alphanumeric
	} else {
		Byte
	}

alphanumeric_value : U8 -> Try(U64, [NotAlphanumeric])
alphanumeric_value = |byte|
	if byte >= '0' and byte <= '9' {
		Ok((byte - '0').to_u64())
	} else if byte >= 'A' and byte <= 'Z' {
		Ok((byte - 'A').to_u64() + 10)
	} else {
		match byte {
			' ' => Ok(36)
			'$' => Ok(37)
			'%' => Ok(38)
			'*' => Ok(39)
			'+' => Ok(40)
			'-' => Ok(41)
			'.' => Ok(42)
			'/' => Ok(43)
			':' => Ok(44)
			_ => Err(NotAlphanumeric)
		}
	}

mode_indicator : Mode -> U64
mode_indicator = |mode|
	match mode {
		Numeric => 1
		Alphanumeric => 2
		Byte => 4
	}

## The bits of the character count, which grow with the version.
count_bits : Mode, U64 -> U64
count_bits = |mode, ver| {
	group = if ver < 10 0 else if ver < 27 1 else 2
	widths =
		match mode {
			Numeric => [10, 12, 14]
			Alphanumeric => [9, 11, 13]
			Byte => [8, 16, 16]
		}
	widths.get(group) ?? 16
}

## The bits `count` characters take in `mode`, without the header.
data_bits : Mode, U64 -> U64
data_bits = |mode, count|
	match mode {
		Numeric => 10 * (count // 3) + ([0, 4, 7].get(count % 3) ?? 0)
		Alphanumeric => 11 * (count // 2) + 6 * (count % 2)
		Byte => 8 * count
	}

smallest_version : U64, Mode, QrCode.Level -> Try(U64, [TooLong])
smallest_version = |count, mode, correction| {
	for ver in between(1, 41) {
		width = count_bits(mode, ver)
		if count < 1.U64.shl_wrap(width.to_u8_wrap()) and 4 + width + data_bits(mode, count) <= data_codewords(ver, correction) * 8 {
			return Ok(ver)
		}
	}
	Err(TooLong)
}

## The most characters version 40 holds in `mode` at `correction`.
most_characters : Mode, QrCode.Level -> U64
most_characters = |mode, correction| {
	room = data_codewords(40, correction) * 8 - 4 - count_bits(mode, 40)
	match mode {
		Numeric => 3 * (room // 10) + (if room % 10 >= 7 2 else if room % 10 >= 4 1 else 0)
		Alphanumeric => 2 * (room // 11) + (if room % 11 >= 6 1 else 0)
		Byte => room // 8
	}
}

## The error correction codewords of each block, by version.
ec_per_block : QrCode.Level, U64 -> U64
ec_per_block = |correction, ver| {
	table =
		match correction {
			Low => [0, 7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30]
			Medium => [0, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28]
			Quartile => [0, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30]
			High => [0, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30]
		}
	table.get(ver) ?? 0
}

## The number of error correction blocks, by version.
block_count : QrCode.Level, U64 -> U64
block_count = |correction, ver| {
	table =
		match correction {
			Low => [0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25]
			Medium => [0, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49]
			Quartile => [0, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68]
			High => [0, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81]
		}
	table.get(ver) ?? 1
}

## The modules left for data and error correction once the function
## patterns are in place, a few of which may not fill a whole codeword.
raw_modules : U64 -> U64
raw_modules = |ver| {
	all = (16 * ver + 128) * ver + 64
	if ver < 2 {
		all
	} else {
		count = ver // 7 + 2
		without_alignment = all - ((25 * count - 10) * count - 55)
		if ver >= 7 without_alignment - 36 else without_alignment
	}
}

data_codewords : U64, QrCode.Level -> U64
data_codewords = |ver, correction| raw_modules(ver) // 8 - ec_per_block(correction, ver) * block_count(correction, ver)

## The centres of the alignment patterns on each axis.
alignment_positions : U64 -> List(U64)
alignment_positions = |ver|
	if ver == 1 {
		[]
	} else {
		count = ver // 7 + 2
		step = (ver * 8 + count * 3 + 5) // (count * 4 - 4) * 2
		last = side_of(ver) - 7
		[6].concat(indices(count - 1).map(|i| last - (count - 2 - i) * step))
	}

side_of : U64 -> U64
side_of = |ver| ver * 4 + 17

## The data codewords: the mode indicator, the character count, the data,
## the terminator and the padding, split into blocks. Then each block's error
## correction. Both are interleaved across the blocks.
codewords : U64, QrCode.Level, Mode, List(U8) -> List(U8)
codewords = |ver, correction, mode, bytes| {
	capacity = data_codewords(ver, correction) * 8
	header = bits_of(mode_indicator(mode), 4).concat(bits_of(bytes.len(), count_bits(mode, ver)))
	content = header.concat(data_of(mode, bytes))
	terminated = content.concat(List.repeat(Bool.False, (capacity - content.len()).min(4)))
	aligned = terminated.concat(List.repeat(Bool.False, (8 - terminated.len() % 8) % 8))
	packed = pack(aligned)
	data = indices(data_codewords(ver, correction) - packed.len()).fold(packed, |acc, i| acc.append(if i % 2 == 0 0xEC else 0x11))

	ec = ec_per_block(correction, ver)
	blocks = block_count(correction, ver)
	total = raw_modules(ver) // 8
	short = blocks - total % blocks
	lengths = indices(blocks).map(|i| total // blocks - ec + (if i < short 0 else 1))
	divisor = rs_divisor(ec)
	(split, _) = lengths.fold(([], 0), |(acc, start), length| (acc.append(data.sublist({ start, len: length })), start + length))
	corrections = split.map(|block| rs_remainder(block, divisor))
	interleave(split).concat(interleave(corrections))
}

## The data in `mode`: digits three at a time in 10 bits, characters two at
## a time in 11 bits, or each byte in 8 bits, with a shorter group last.
data_of : Mode, List(U8) -> List(Bool)
data_of = |mode, bytes|
	match mode {
		Numeric =>
			chunks(bytes, 3).fold(
				[],
				|bits, group| {
					value = group.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())
					bits.concat(bits_of(value, group.len() * 3 + 1))
				},
			)

		Alphanumeric =>
			chunks(bytes, 2).fold(
				[],
				|bits, pair| {
					value = pair.fold(0, |sum, byte| sum * 45 + (alphanumeric_value(byte) ?? 0))
					bits.concat(bits_of(value, pair.len() * 5 + 1))
				},
			)

		Byte => bytes.fold([], |bits, byte| bits.concat(bits_of(byte.to_u64(), 8)))
	}

chunks : List(U8), U64 -> List(List(U8))
chunks = |bytes, width|
	indices((bytes.len() + width - 1) // width).map(|i| bytes.sublist({ start: i * width, len: width }))

bits_of : U64, U64 -> List(Bool)
bits_of = |value, count|
	indices(count).map(|i| value.shr_zf_wrap((count - 1 - i).to_u8_wrap()).bitwise_and(1) == 1)

pack : List(Bool) -> List(U8)
pack = |bits|
	indices(bits.len() // 8).map(
		|i| bits.sublist({ start: i * 8, len: 8 }).fold(0.U8, |byte, bit| byte.shl_wrap(1).bitwise_or(if bit 1 else 0)),
	)

interleave : List(List(U8)) -> List(U8)
interleave = |blocks| {
	longest = blocks.fold(0, |most, block| most.max(block.len()))
	indices(longest).fold(
		[],
		|acc, i| blocks.fold(acc, |inner, block| match block.get(i) {
			Ok(byte) => inner.append(byte)
			Err(_) => inner
		}),
	)
}

## Multiplication in GF(2^8) modulo x^8 + x^4 + x^3 + x^2 + 1.
gf_multiply : U8, U8 -> U8
gf_multiply = |x, y| {
	var $z = 0.U64
	var $i = 8.U64
	while $i > 0 {
		$i = $i - 1
		$z = $z.shl_wrap(1).bitwise_xor($z.shr_zf_wrap(7) * 0x11D)
		$z = $z.bitwise_xor(y.to_u64().shr_zf_wrap($i.to_u8_wrap()).bitwise_and(1) * x.to_u64())
	}
	$z.to_u8_wrap()
}

## The Reed-Solomon generator polynomial of `degree`, without its leading
## term, highest power first.
rs_divisor : U64 -> List(U8)
rs_divisor = |degree| {
	var $result = List.repeat(0.U8, degree - 1).append(1)
	var $root = 1.U8
	for _ in indices(degree) {
		for j in indices(degree) {
			current = gf_multiply($result.get(j) ?? 0, $root)
			next = if j + 1 < degree $result.get(j + 1) ?? 0 else 0
			$result = $result.set(j, current.bitwise_xor(next)) ?? $result
		}
		$root = gf_multiply($root, 2)
	}
	$result
}

rs_remainder : List(U8), List(U8) -> List(U8)
rs_remainder = |data, divisor|
	data.fold(
		List.repeat(0.U8, divisor.len()),
		|result, byte| {
			factor = byte.bitwise_xor(result.get(0) ?? 0)
			shifted = result.drop_first(1).append(0)
			List.map2(shifted, divisor, |r, d| r.bitwise_xor(gf_multiply(d, factor)))
		},
	)

## A code being built: its modules before masking, and which of them belong
## to function patterns, which data and masks leave alone.
Grid : { side : U64, modules : List(Bool), function : List(Bool) }

set_function : Grid, U64, U64, Bool -> Grid
set_function = |grid, x, y, on| {
	index = y * grid.side + x
	{ ..grid, modules: grid.modules.set(index, on) ?? grid.modules, function: grid.function.set(index, Bool.True) ?? grid.function }
}

## The finder, timing and alignment patterns, the version information and
## room for the format information.
function_patterns : U64 -> Grid
function_patterns = |ver| {
	side = side_of(ver)
	empty = { side, modules: List.repeat(Bool.False, side * side), function: List.repeat(Bool.False, side * side) }
	timed = indices(side).fold(empty, |grid, i| set_function(set_function(grid, 6, i, i % 2 == 0), i, 6, i % 2 == 0))
	finders = [(3, 3), (side - 4, 3), (3, side - 4)].fold(timed, |grid, (cx, cy)| finder(grid, cx, cy))
	positions = alignment_positions(ver)
	last = if positions.is_empty() 0 else positions.len() - 1
	aligned = positions.fold_with_index(
		finders,
		|outer, x, i| positions.fold_with_index(
			outer,
			|grid, y, j|
				# The three corners with a finder pattern get no alignment
				# pattern.
				if (i == 0 and j == 0) or (i == 0 and j == last) or (i == last and j == 0)
					grid
				else
					alignment(grid, x, y),
		),
	)
	versioned(format(aligned, Low, 0), ver)
}

finder : Grid, U64, U64 -> Grid
finder = |grid, cx, cy| {
	offsets = signed(4)
	offsets.fold(
		grid,
		|outer, dy| offsets.fold(
			outer,
			|inner, dx| {
				x = cx.to_i64_wrap() + dx
				y = cy.to_i64_wrap() + dy
				distance = dx.abs().max(dy.abs())
				if x >= 0 and y >= 0 and x < inner.side.to_i64_wrap() and y < inner.side.to_i64_wrap()
					set_function(inner, x.to_u64_wrap(), y.to_u64_wrap(), distance != 2 and distance != 4)
				else
					inner
			},
		),
	)
}

alignment : Grid, U64, U64 -> Grid
alignment = |grid, cx, cy| {
	offsets = signed(2)
	offsets.fold(
		grid,
		|outer, dy| offsets.fold(
			outer,
			|inner, dx| set_function(inner, (cx.to_i64_wrap() + dx).to_u64_wrap(), (cy.to_i64_wrap() + dy).to_u64_wrap(), dx.abs().max(dy.abs()) != 1),
		),
	)
}

## The format information for `correction` and `pattern`, both copies, and
## the dark module.
format : Grid, QrCode.Level, U8 -> Grid
format = |grid, correction, pattern| {
	data = level_bits(correction).shl_wrap(3).bitwise_or(pattern.to_u64())
	var $rem = data
	for _ in indices(10) {
		$rem = $rem.shl_wrap(1).bitwise_xor($rem.shr_zf_wrap(9) * 0x537)
	}
	bits = data.shl_wrap(10).bitwise_or($rem).bitwise_xor(0x5412)
	bit = |i| bit_at(bits, i)
	side = grid.side
	top = indices(6).fold(grid, |g, i| set_function(g, 8, i, bit(i)))
	first = set_function(set_function(set_function(top, 8, 7, bit(6)), 8, 8, bit(7)), 7, 8, bit(8))
	first_done = between(9, 15).fold(first, |g, i| set_function(g, 14 - i, 8, bit(i)))
	second = indices(8).fold(first_done, |g, i| set_function(g, side - 1 - i, 8, bit(i)))
	both = between(8, 15).fold(second, |g, i| set_function(g, 8, side - 15 + i, bit(i)))
	set_function(both, 8, side - 8, Bool.True)
}

## The two bits the format information gives each level.
level_bits : QrCode.Level -> U64
level_bits = |correction|
	match correction {
		Low => 1
		Medium => 0
		Quartile => 3
		High => 2
	}

## Bit `i` of `bits`, counting from the lowest.
bit_at : U64, U64 -> Bool
bit_at = |bits, i| bits.shr_zf_wrap(i.to_u8_wrap()).bitwise_and(1) == 1

## The version information, for version 7 and up.
versioned : Grid, U64 -> Grid
versioned = |grid, ver|
	if ver < 7 {
		grid
	} else {
		var $rem = ver
		for _ in indices(12) {
			$rem = $rem.shl_wrap(1).bitwise_xor($rem.shr_zf_wrap(11) * 0x1F25)
		}
		bits = ver.shl_wrap(12).bitwise_or($rem)
		indices(18).fold(
			grid,
			|g, i| {
				on = bits.shr_zf_wrap(i.to_u8_wrap()).bitwise_and(1) == 1
				a = g.side - 11 + i % 3
				b = i // 3
				set_function(set_function(g, a, b, on), b, a, on)
			},
		)
	}

## The codewords' bits in the zigzag order of the standard, two columns at a
## time from the right, skipping the vertical timing pattern. The few modules
## left over after the last codeword stay light.
place_data : Grid, List(U8) -> Grid
place_data = |grid, data| {
	side = grid.side
	total = data.len() * 8
	var $modules = grid.modules
	var $i = 0
	var $right = side - 1
	while $right >= 1 {
		if $right == 6 {
			$right = 5
		}
		for vert in indices(side) {
			for j in [0, 1] {
				x = $right - j
				upward = ($right + 1).bitwise_and(2) == 0
				y = if upward side - 1 - vert else vert
				index = y * side + x
				if !(grid.function.get(index) ?? Bool.True) and $i < total {
					byte = data.get($i // 8) ?? 0
					on = byte.shr_zf_wrap((7 - $i % 8).to_u8_wrap()).bitwise_and(1) == 1
					$modules = $modules.set(index, on) ?? $modules
					$i = $i + 1
				}
			}
		}
		$right = if $right >= 2 $right - 2 else 0
	}
	{ ..grid, modules: $modules }
}

masked : Grid, U64, QrCode.Level, U8 -> QrCode
masked = |grid, ver, correction, pattern| {
	side = grid.side
	flipped =
		List.map2(grid.modules, grid.function, |on, fixed| (on, fixed)).map_with_index(
			|(on, fixed), index| if fixed on else on != mask_bit(pattern, index % side, index // side),
		)
	formatted = format({ ..grid, modules: flipped }, correction, pattern)
	QrCode.({ ver, correction, pattern, grid, modules: formatted.modules })
}

mask_bit : U8, U64, U64 -> Bool
mask_bit = |pattern, x, y|
	match pattern {
		0 => (x + y) % 2 == 0
		1 => y % 2 == 0
		2 => x % 3 == 0
		3 => (x + y) % 3 == 0
		4 => (x // 3 + y // 2) % 2 == 0
		5 => (x * y) % 2 + (x * y) % 3 == 0
		6 => ((x * y) % 2 + (x * y) % 3) % 2 == 0
		_ => ((x + y) % 2 + (x * y) % 3) % 2 == 0
	}

## The standard's penalty: runs of five or more, 2x2 blocks, finder-like
## patterns and the imbalance of dark and light.
penalty : List(Bool), U64 -> U64
penalty = |modules, side| {
	at = |x, y| modules.get(y * side + x) ?? Bool.False
	lines = indices(side)
	runs = lines.fold(
		0,
		|sum, i| sum + line_penalty(modules.sublist({ start: i * side, len: side }), side) + line_penalty(lines.map(|y| at(i, y)), side),
	)
	blocks = indices(side - 1).fold(
		0,
		|outer, y| indices(side - 1).fold(
			outer,
			|sum, x| {
				on = at(x, y)
				if on == at(x + 1, y) and on == at(x, y + 1) and on == at(x + 1, y + 1) sum + 3 else sum
			},
		),
	)
	dark_count = modules.count_if(|on| on)
	total = side * side
	difference = if dark_count * 20 >= total * 10 dark_count * 20 - total * 10 else total * 10 - dark_count * 20
	balance = ((difference + total - 1) // total - 1) * 10
	runs + blocks + balance
}

line_penalty : List(Bool), U64 -> U64
line_penalty = |line, side| {
	var $score = 0
	var $color = Bool.False
	var $run = 0
	var $history = List.repeat(0, 7)
	for on in line {
		if on == $color {
			$run = $run + 1
			if $run == 5 {
				$score = $score + 3
			} else if $run > 5 {
				$score = $score + 1
			}
		} else {
			$history = add_history($history, $run, side)
			if !$color {
				$score = $score + count_finder_patterns($history) * 40
			}
			$color = on
			$run = 1
		}
	}
	# The light border after the line ends the last run.
	(ended, last_run) = if $color (add_history($history, $run, side), 0) else ($history, $run)
	$score + count_finder_patterns(add_history(ended, last_run + side, side)) * 40
}

add_history : List(U64), U64, U64 -> List(U64)
add_history = |history, run, side| {
	length = if (history.get(0) ?? 0) == 0 run + side else run
	[length].concat(history.drop_last(1))
}

count_finder_patterns : List(U64) -> U64
count_finder_patterns = |history| {
	h = |i| history.get(i) ?? 0
	n = h(1)
	core = n > 0 and h(2) == n and h(3) == n * 3 and h(4) == n and h(5) == n
	(if core and h(0) >= n * 4 and h(6) >= n 1 else 0) + (if core and h(6) >= n * 4 and h(0) >= n 1 else 0)
}

## 0, 1, … up to but not including `count`.
indices : U64 -> List(U64)
indices = |count| between(0, count)

## `start`, `start` + 1, … up to but not including `end`.
between : U64, U64 -> List(U64)
between = |start, end| {
	var $list = List.with_capacity(if end > start end - start else 0)
	var $i = start
	while $i < end {
		$list = $list.append($i)
		$i = $i + 1
	}
	$list
}

## -`reach` to `reach`.
signed : I64 -> List(I64)
signed = |reach| {
	var $list = []
	var $i = -reach
	while $i <= reach {
		$list = $list.append($i)
		$i = $i + 1
	}
	$list
}

# The references are what `qrencode -m 0 -t ASCII` 4.1.1 prints, with the
# mask qrencode picked.
expect QrCode.encode_bytes("HELLO".to_utf8(), Medium).map_ok(|qr| qr.with_mask(3).map_ok(|masked_qr| masked_qr.rows())) == Ok(Ok([
	"#######.#..#..#######",
	"#.....#.####..#.....#",
	"#.###.#...#.#.#.###.#",
	"#.###.#.#.#.#.#.###.#",
	"#.###.#....#..#.###.#",
	"#.....#....##.#.....#",
	"#######.#.#.#.#######",
	"........#..##........",
	"#.##.###.#.##.#..#.##",
	".##.##.#.######..##..",
	"#...#.#..#.#.......##",
	"#.##...#...#..####.#.",
	".#.######...#..#..#.#",
	"........####..#...#.#",
	"#######.#..##..#.....",
	"#.....#.#.#....#####.",
	"#.###.#.....######.##",
	"#.###.#.#.##..#.####.",
	"#.###.#.##..#.##..#..",
	"#.....#...#..#.##...#",
	"#######.#.#..#.#.....",
]))

expect QrCode.encode("https://www.bosthlm.se/vara-maklare/shaquille-hilaire", Medium).map_ok(|qr| qr.rows()) == Ok([
	"#######...#.....#.####.#..#######",
	"#.....#..#..#..##...###.#.#.....#",
	"#.###.#.####..###..#.####.#.###.#",
	"#.###.#.#####.....#.....#.#.###.#",
	"#.###.#.#..##.###.###..#..#.###.#",
	"#.....#.#.............#.#.#.....#",
	"#######.#.#.#.#.#.#.#.#.#.#######",
	"........#...#.#.#..#.##..........",
	"#.#####..#.#.##..##.#.###.#####..",
	".###.#...#.###..#####.##..##.####",
	".##.#####..#.#####...##.#...#.##.",
	"#.#..#......##.#########.#..####.",
	"#######...##..#.#..##.####.###...",
	"#.##.....#.#.#.#.#####.#.##...###",
	".##.#.##......####..#...####...#.",
	".#.....##..#.#.#.....##..######..",
	".#.#..#..#.....#.#.#..####.##...#",
	"..####.###..###....#..######.##.#",
	".#.#..###..###.##.#.##.....##.##.",
	".#..##..#...#.##....##.#.#.####..",
	"....#.##.....#.#..##..#.##.###.##",
	"##..##...#...#.##..###.#.##..##.#",
	"#..#.##...#..##..#..###..#..####.",
	"#.#......#...#..#.#..#..####..##.",
	"#..#.##....#.....#....#######..#.",
	"........#.#...#.#####..##...#.#.#",
	"#######..#....##.#...#.##.#.#.##.",
	"#.....#.#........##.#####...#####",
	"#.###.#.#####.###..#..#.######..#",
	"#.###.#.##.##..#..###..#.#..##.##",
	"#.###.#.###.#.###.#..#.#####.#...",
	"#.....#....###.#....##...#..###..",
	"#######.#.####.###.#..####.#.#.#.",
])

# The capacities of version 40 in the standard's table 7.
expect QrCode.encode(Str.repeat("1", 7089), Low).map_ok(|qr| qr.version()) == Ok(40)
expect QrCode.encode(Str.repeat("1", 7090), Low).map_ok(|qr| qr.version()) == Err(TooLong({ bytes: 7090, most: 7089 }))
expect QrCode.encode(Str.repeat("A", 4296), Low).map_ok(|qr| qr.version()) == Ok(40)
expect QrCode.encode(Str.repeat("A", 4297), Low).map_ok(|qr| qr.version()) == Err(TooLong({ bytes: 4297, most: 4296 }))
expect QrCode.encode(Str.repeat("a", 2953), Low).map_ok(|qr| qr.size()) == Ok(177)
expect QrCode.encode(Str.repeat("a", 2954), Low).map_ok(|qr| qr.size()) == Err(TooLong({ bytes: 2954, most: 2953 }))
expect QrCode.encode(Str.repeat("a", 1273), High).map_ok(|qr| qr.version()) == Ok(40)
expect QrCode.encode(Str.repeat("a", 1274), High).map_ok(|qr| qr.version()) == Err(TooLong({ bytes: 1274, most: 1273 }))

# The capacities of version 1 at each level.
expect QrCode.encode(Str.repeat("1", 41), Low).map_ok(|qr| qr.version()) == Ok(1)
expect QrCode.encode(Str.repeat("1", 42), Low).map_ok(|qr| qr.version()) == Ok(2)
expect QrCode.encode(Str.repeat("A", 20), Medium).map_ok(|qr| qr.version()) == Ok(1)
expect QrCode.encode(Str.repeat("A", 21), Medium).map_ok(|qr| qr.version()) == Ok(2)
expect QrCode.encode(Str.repeat("a", 11), Quartile).map_ok(|qr| qr.version()) == Ok(1)
expect QrCode.encode(Str.repeat("a", 12), Quartile).map_ok(|qr| qr.version()) == Ok(2)
expect QrCode.encode(Str.repeat("1", 17), High).map_ok(|qr| qr.version()) == Ok(1)
expect QrCode.encode(Str.repeat("1", 18), High).map_ok(|qr| qr.version()) == Ok(2)

# Byte mode for one character outside the alphanumeric set, which lowercase
# letters are.
expect QrCode.encode(Str.repeat("A", 20).concat("a"), Medium).map_ok(|qr| qr.version()) == Ok(2)
expect QrCode.encode_bytes(Str.repeat("1", 17).to_utf8(), High).map_ok(|qr| qr.version()) == Ok(3)

# Empty text fits in version 1.
expect QrCode.encode("", Medium).map_ok(|qr| qr.size()) == Ok(21)

expect QrCode.encode("A", Medium).map_ok(|qr| qr.level()) == Ok(Medium)

# Masks.
expect match QrCode.encode("HELLO", Quartile) {
	Ok(qr) => [0, 1, 2, 3, 4, 5, 6, 7].map(|pattern| qr.with_mask(pattern).map_ok(|other| other.mask())) == [Ok(0), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)]
	Err(_) => Bool.False
}
expect QrCode.encode("HELLO", Quartile).map_ok(|qr| qr.with_mask(8).map_ok(|other| other.mask())) == Ok(Err(NoSuchMask(8)))
expect match QrCode.encode("HELLO", Quartile) {
	Ok(qr) => qr.with_mask(qr.mask()) == Ok(qr)
	Err(_) => Bool.False
}

# The alignment pattern centres of the standard's annex E.
expect alignment_positions(2) == [6, 18]
expect alignment_positions(7) == [6, 22, 38]
expect alignment_positions(32) == [6, 34, 60, 86, 112, 138]
expect alignment_positions(36) == [6, 24, 50, 76, 102, 128, 154]
expect alignment_positions(40) == [6, 30, 58, 86, 114, 142, 170]

# Outside the code every module is light.
expect QrCode.encode("A", Low).map_ok(|qr| (qr.dark(0, 0), qr.dark(21, 0), qr.dark(0, 21))) == Ok((Bool.True, Bool.False, Bool.False))
