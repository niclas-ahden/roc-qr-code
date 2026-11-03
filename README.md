# roc-qr-code

QR codes (ISO/IEC 18004) in pure Roc.

- Every version, 1 to 40, and every error correction level, `Low`, `Medium`, `Quartile` and `High`.
- `QrCode.encode` writes a text in the most compact mode the whole text fits: numeric, alphanumeric or bytes. `QrCode.encode_bytes` writes any bytes.
- The smallest version that holds the data, and the mask with the lowest penalty score of the standard.
- A `QrCode` is its grid of modules. Draw it however you like: in SVG, in a PDF, in a terminal.

See the documentation at [https://niclas-ahden.github.io/roc-qr-code/](https://niclas-ahden.github.io/roc-qr-code/).

## Usage

```roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
	qr: "https://github.com/niclas-ahden/roc-qr-code/releases/download/0.1.0/2LMK7BBGrMD3rwdEV9mPK41ZPp3gNXS1Z7bgS7p9gZ4N.tar.zst",
}

import pf.Stdout
import qr.QrCode

main! = |_args| {
	code = QrCode.encode("https://www.roc-lang.org", Medium)?

	# Two characters to a module, with the light border of four modules a
	# reader needs.
	border = 4
	side = code.size() + 2 * border
	var $y = 0
	while $y < side {
		var $line = ""
		var $x = 0
		while $x < side {
			dark = $x >= border and $y >= border and code.dark($x - border, $y - border)
			$line = $line.concat(if dark "██" else "  ")
			$x = $x + 1
		}
		Stdout.line!($line)?
		$y = $y + 1
	}

	Ok({})
}
```

A text too long for version 40 at the chosen level is an error that says how long it is and how much fits:

```roc
match QrCode.encode(text, High) {
	Ok(code) => draw(code)
	Err(TooLong({ bytes, most })) => "${bytes.to_str()} bytes, only ${most.to_str()} fit"
}
```

See [examples](examples/) for a program that also writes the code as SVG.

## Limitations

- One mode for the whole text. An encoder that splits a text into segments, such as qrencode, can make a smaller code for a text that mixes long runs of digits with other characters.
- No kanji mode, no ECI and no structured append. Byte mode carries UTF-8 without an ECI designator, which is what phones read.
- No micro QR codes.
- It makes codes and does not read them.
