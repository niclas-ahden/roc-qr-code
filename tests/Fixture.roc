import crc32.Crc32
import Inputs

## One line of tests/qrencode.txt: a text, named by its mode, length and seed
## (see Inputs.roc), the level it was encoded at, and what qrencode made of
## it: the version, the mask, and the CRC-32 of its rows written the way
## `QrCode.rows` writes them, joined by newlines.
Fixture :: [].{

	Case : { level : [Low, Medium, Quartile, High], mode : Inputs.Mode, ver : U64, length : U64, seed : U64, pattern : U8, checksum : Str }

	to_line : Case -> Str
	to_line = |c| "${level_name(c.level)} ${mode_name(c.mode)} ${c.ver.to_str()} ${c.length.to_str()} ${c.seed.to_str()} ${c.pattern.to_str()} ${c.checksum}"

	from_line : Str -> Try(Case, [BadLine(Str)])
	from_line = |line| {
		words = line.split_on(" ")
		word = |i| words.get(i) ?? ""
		number = |i| U64.from_str(word(i)).map_err(|_| BadLine(line))
		level = match word(0) {
			"L" => Low
			"M" => Medium
			"Q" => Quartile
			"H" => High
			_ => return Err(BadLine(line))
		}
		mode = match word(1) {
			"numeric" => Numeric
			"alphanumeric" => Alphanumeric
			"byte" => Byte
			_ => return Err(BadLine(line))
		}
		pattern = U8.from_str(word(5)) ? |_| BadLine(line)
		Ok({ level, mode, ver: number(2)?, length: number(3)?, seed: number(4)?, pattern, checksum: word(6) })
	}

	## The CRC-32 of the rows joined by newlines, as 8 hexadecimal digits.
	checksum_of : List(Str) -> Str
	checksum_of = |lines| {
		sum = Crc32.checksum(Str.join_with(lines, "\n").to_utf8())
		digits = [28, 24, 20, 16, 12, 8, 4, 0].map(|shift| hex_digit(sum.shr_zf_wrap(shift).bitwise_and(0xF).to_u8_wrap()))
		Str.from_utf8_lossy(digits)
	}

	## The text a case is made of.
	text_of : Case -> Str
	text_of = |c| Inputs.text(c.mode, c.length, c.seed)
}

level_name : [Low, Medium, Quartile, High] -> Str
level_name = |level|
	match level {
		Low => "L"
		Medium => "M"
		Quartile => "Q"
		High => "H"
	}

mode_name : Inputs.Mode -> Str
mode_name = |mode|
	match mode {
		Numeric => "numeric"
		Alphanumeric => "alphanumeric"
		Byte => "byte"
	}

hex_digit : U8 -> U8
hex_digit = |n| if n < 10 '0' + n else 'a' + n - 10
