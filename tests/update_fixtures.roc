#!/usr/bin/env roc
## Writes tests/qrencode.txt: what qrencode makes of a text in each mode, at
## each level, in each version, once for the shortest and once for the
## longest text the version holds. The capacities come from qrencode too, by
## asking it for codes of growing texts, so nothing in the file comes from
## this package. tests/harness.roc compares our codes with it.
##
## Run it from the repository root in the dev shell, which has qrencode:
##
##     nix develop -c roc tests/update_fixtures.roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	crc32: "https://github.com/niclas-ahden/roc-crc32/releases/download/2.0.0/Dpu6zTP4s6bYRC2U9jWK7CcNBNGEfViKQbp35xSRC2Cr.tar.zst",
}

import pf.Cmd
import pf.OsStr
import pf.Path
import pf.Stdout
import Fixture
import Inputs

main! = |_args| {
	var $lines = ["# level mode version length seed mask crc32, made by tests/update_fixtures.roc with ${version!()?}"]
	for level in [Low, Medium, Quartile, High] {
		for mode in [Numeric, Alphanumeric, Byte] {
			var $longest_before = 0.U64
			var $ver = 1
			while $ver <= 40 {
				seed = $ver * 100 + level_index(level) * 10 + mode_index(mode)
				longest = longest_in!(level, mode, $ver, seed, $longest_before)?
				for length in [$longest_before + 1, longest] {
					code = qrencode!(level, mode, Inputs.text(mode, length, seed))?
					if code.ver != $ver {
						return Err(VersionMoved({ level, mode, length, expected: $ver, got: code.ver }))
					}
					$lines = $lines.append(Fixture.to_line({ level, mode, ver: $ver, length, seed, pattern: code.pattern, checksum: Fixture.checksum_of(code.lines) }))
				}
				$longest_before = longest
				$ver = $ver + 1
			}
			Stdout.line!("${Str.inspect(level)} ${Str.inspect(mode)}: version 40 holds ${$longest_before.to_str()}")?
		}
	}
	Path.write_bytes!(Path.from_os_str(OsStr.from_str("tests/qrencode.txt")), Str.join_with($lines, "\n").concat("\n").to_utf8())?
	Stdout.line!("Wrote ${($lines.len() - 1).to_str()} cases to tests/qrencode.txt")
}

## The longest text `ver` holds, more than `known`, which it holds.
longest_in! : [Low, Medium, Quartile, High], Inputs.Mode, U64, U64, U64 => Try(U64, _)
longest_in! = |level, mode, ver, seed, known| {
	fits! = |length|
		match qrencode!(level, mode, Inputs.text(mode, length, seed)) {
			Ok(code) => Ok(code.ver <= ver)
			Err(TooLong) => Ok(Bool.False)
			Err(other) => Err(other)
		}
	var $low = known
	var $high = known + 1
	while fits!($high)? {
		$low = $high
		$high = $high * 2
	}
	while $high - $low > 1 {
		middle = ($low + $high) // 2
		if fits!(middle)? {
			$low = middle
		} else {
			$high = middle
		}
	}
	Ok($low)
}

## qrencode's code for `text`: one segment in byte mode, and its own choice
## of segments otherwise, which is one segment for the texts of Inputs.roc.
qrencode! : [Low, Medium, Quartile, High], Inputs.Mode, Str => Try({ ver : U64, pattern : U8, lines : List(Str) }, _)
qrencode! = |level, mode, text| {
	level_flag = match level {
		Low => "L"
		Medium => "M"
		Quartile => "Q"
		High => "H"
	}
	byte_flag = match mode {
		Byte => ["-8"]
		_ => []
	}
	arguments = ["-l", level_flag, "-m", "0", "-t", "ASCII", "-o", "-"].concat(byte_flag).concat(["--", text])
	output = Cmd.new(OsStr.from_str("qrencode")).args(arguments.map(OsStr.from_str)).run!()?
	match output.status {
		Exited(0) => {}
		_ =>
			if output.stderr_utf8_lossy().contains("too large") {
				return Err(TooLong)
			} else {
				return Err(QrencodeFailed(output.stderr_utf8_lossy()))
			}
	}
	# Two characters per module, "##" for a dark one.
	lines = Str.from_utf8_lossy(output.stdout_bytes)
		.split_on("\n")
		.drop_if(|line| line.is_empty())
		.map(|line| Str.from_utf8_lossy(line.to_utf8().chunks_of(2).map(|pair| if pair.first() == Ok('#') '#' else '.')))
	side = lines.len()
	dark = |x, y| (lines.get(y) ?? "").to_utf8().get(x) == Ok('#')
	# The first copy of the format information, as Nayuki's library draws it.
	positions = [(8, 0), (8, 1), (8, 2), (8, 3), (8, 4), (8, 5), (8, 7), (8, 8), (7, 8), (5, 8), (4, 8), (3, 8), (2, 8), (1, 8), (0, 8)]
	bits = positions.fold_with_index(0.U64, |sum, (x, y), i| if dark(x, y) sum.bitwise_or(1.U64.shl_wrap(i.to_u8_wrap())) else sum)
	data = bits.bitwise_xor(0x5412).shr_zf_wrap(10)
	Ok({ ver: (side - 17) // 4, pattern: data.bitwise_and(7).to_u8_wrap(), lines })
}

version! : () => Try(Str, _)
version! = || {
	output = Cmd.new(OsStr.from_str("qrencode")).args([OsStr.from_str("--version")]).run!()?
	Ok(Str.from_utf8_lossy(output.stdout_bytes).concat(output.stderr_utf8_lossy()).split_on("\n").first() ?? "qrencode")
}

level_index : [Low, Medium, Quartile, High] -> U64
level_index = |level|
	match level {
		Low => 0
		Medium => 1
		Quartile => 2
		High => 3
	}

mode_index : Inputs.Mode -> U64
mode_index = |mode|
	match mode {
		Numeric => 0
		Alphanumeric => 1
		Byte => 2
	}
