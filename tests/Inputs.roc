## The texts the fixtures are made of. tests/qrencode.txt names each by its
## mode, its length and a seed, and tests/update_fixtures.roc and
## tests/harness.roc both make it from them here.
Inputs :: [].{

	Mode : [Numeric, Alphanumeric, Byte]

	## A text of `length` characters that every encoder writes in `mode` as
	## one segment, pseudo-random from `seed`. Alphanumeric texts never have
	## two digits in a row, so an encoder that mixes modes, as qrencode does,
	## has no run of digits worth a numeric segment. Byte texts are printable
	## ASCII.
	text : Mode, U64, U64 -> Str
	text = |mode, length, seed| {
		var $state = seed.bitwise_xor(0x9E3779B97F4A7C15)
		var $bytes = List.with_capacity(length)
		var $digit_before = Bool.False
		while $bytes.len() < length {
			$state = $state.times_wrap(6364136223846793005).plus_wrap(1442695040888963407)
			random = $state.shr_zf_wrap(33)
			byte =
				match mode {
					Numeric => '0' + (random % 10).to_u8_wrap()
					Alphanumeric =>
						if !$digit_before and random % 7 == 0 {
							'0' + (random.shr_zf_wrap(8) % 10).to_u8_wrap()
						} else {
							letters.get(random % letters.len()) ?? 'A'
						}

					Byte => 32 + (random % 95).to_u8_wrap()
				}
			$digit_before = byte >= '0' and byte <= '9'
			$bytes = $bytes.append(byte)
		}
		Str.from_utf8_lossy($bytes)
	}
}

## The alphanumeric characters without the digits.
letters : List(U8)
letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:".to_utf8()
