## Our codes against other programs.
##
## tests/qrencode.txt holds what qrencode made of a text in each mode, at each
## level, in each version, once for the shortest and once for the longest
## text that version holds (see update_fixtures.roc). Each of our codes must
## have qrencode's version and, with qrencode's mask, the same modules.
## Encoders pick masks differently, so the masks we pick ourselves are proven
## by reading them back: when zbarimg is on the PATH, it has to read every
## text back from our own code. The dev shell has it:
##
##     nix develop -c ./tests.roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
	qr: "../package/main.roc",
	crc32: "https://github.com/niclas-ahden/roc-crc32/releases/download/2.0.0/Dpu6zTP4s6bYRC2U9jWK7CcNBNGEfViKQbp35xSRC2Cr.tar.zst",
}

import pf.Cmd
import pf.Env
import pf.OsStr
import pf.Path
import pf.Stdout
import qr.QrCode
import Fixture

main! = |_args| {
	listing = Path.read_bytes!(Path.from_os_str(OsStr.from_str("tests/qrencode.txt")))?
	cases = Str.from_utf8_lossy(listing)
		.split_on("\n")
		.drop_if(|line| line.is_empty() or line.starts_with("#"))
		.map_try(Fixture.from_line)?

	Stdout.line!("== ${cases.len().to_str()} codes against qrencode's")?
	var $failed = 0.U64
	for c in cases {
		match ours(c) {
			Ok(code) => {
				if code.version() != c.ver {
					Stdout.line!("  ${Fixture.to_line(c)}: our version is ${code.version().to_str()}")?
					$failed = $failed + 1
				} else {
					remasked = code.with_mask(c.pattern) ?? code
					checksum = Fixture.checksum_of(remasked.rows())
					if checksum != c.checksum {
						Stdout.line!("  ${Fixture.to_line(c)}: our modules have CRC-32 ${checksum}")?
						$failed = $failed + 1
					}
				}
			}

			Err(TooLong(sizes)) => {
				Stdout.line!("  ${Fixture.to_line(c)}: too long for us, ${Str.inspect(sizes)}")?
				$failed = $failed + 1
			}
		}
	}

	if Cmd.check_available!("zbarimg") {
		Stdout.line!("== zbarimg reads the same texts from our own codes")?
		directory = Env.create_temp_dir_with_prefix!("roc-qr-code-")?
		for c in cases {
			match ours(c) {
				Ok(code) => {
					image = directory.join("code.pbm")
					Path.write_bytes!(image, pbm(code))?
					output = Cmd.new(OsStr.from_str("zbarimg")).args(["--raw", "-q", "-Sdisable", "-Sqrcode.enable"].map(OsStr.from_str).append(Path.to_os_str(image))).run!()?
					read = Str.from_utf8_lossy(output.stdout_bytes)
					if read != Fixture.text_of(c).concat("\n") {
						Stdout.line!("  ${Fixture.to_line(c)}: zbarimg read ${Str.inspect(read)} from our code with mask ${code.mask().to_str()}")?
						$failed = $failed + 1
					}
				}

				Err(_) => {}
			}
		}
		Path.delete_all!(directory)?
	} else {
		Stdout.line!("== zbarimg is not on the PATH, so nothing reads our own codes back")?
	}

	if $failed == 0 {
		Stdout.line!("All passed")
	} else {
		Stdout.line!("${$failed.to_str()} failed")?
		Err(Exit(1))
	}
}

## Our code for a case: in byte mode for qrencode's `-8`, and in the mode
## `encode` picks otherwise.
ours : Fixture.Case -> Try(QrCode, [TooLong({ bytes : U64, most : U64 })])
ours = |c|
	match c.mode {
		Byte => QrCode.encode_bytes(Fixture.text_of(c).to_utf8(), c.level)
		_ => QrCode.encode(Fixture.text_of(c), c.level)
	}

## The code as a plain PBM image, 4 pixels to a module, with the 4 modules
## of light border the standard asks for.
pbm : QrCode -> List(U8)
pbm = |code| {
	scale = 4
	border = 4
	side = (code.size() + 2 * border) * scale
	var $out = "P1\n${side.to_str()} ${side.to_str()}\n".to_utf8()
	var $py = 0
	while $py < side {
		var $px = 0
		while $px < side {
			x = $px // scale
			y = $py // scale
			on = x >= border and y >= border and code.dark(x - border, y - border)
			$out = $out.append(if on '1' else '0')
			$px = $px + 1
		}
		$out = $out.append('\n')
		$py = $py + 1
	}
	$out
}
