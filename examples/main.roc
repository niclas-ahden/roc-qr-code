## Make a QR code, print it in the terminal and write it as SVG.
## Run it with: roc examples/main.roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	qr: "../package/main.roc",
}

import pf.Stdout
import qr.QrCode

main! = |_args| {
	code = QrCode.encode("https://www.roc-lang.org", Medium)?
	Stdout.line!("Version ${code.version().to_str()}, ${code.size().to_str()} modules on each side, mask ${code.mask().to_str()}")?

	# Two characters to a module look about square in a terminal. A reader
	# needs the light border, so the terminal must have a light background.
	for line in terminal(code) {
		Stdout.line!(line)?
	}

	Stdout.line!(svg(code))?

	Ok({})
}

## The code in a terminal, with a light border of four modules.
terminal : QrCode -> List(Str)
terminal = |code| {
	border = 4
	side = code.size() + 2 * border
	var $lines = []
	var $y = 0
	while $y < side {
		var $line = ""
		var $x = 0
		while $x < side {
			dark = $x >= border and $y >= border and code.dark($x - border, $y - border)
			$line = $line.concat(if dark "██" else "  ")
			$x = $x + 1
		}
		$lines = $lines.append($line)
		$y = $y + 1
	}
	$lines
}

## The code as an SVG image, one unit to a module, with a light border of
## four modules. Each dark module is a square of one path.
svg : QrCode -> Str
svg = |code| {
	border = 4
	side = code.size() + 2 * border
	var $squares = ""
	var $y = 0
	while $y < code.size() {
		var $x = 0
		while $x < code.size() {
			if code.dark($x, $y) {
				$squares = $squares.concat("M${($x + border).to_str()} ${($y + border).to_str()}h1v1h-1z")
			}
			$x = $x + 1
		}
		$y = $y + 1
	}
	viewbox = "0 0 ${side.to_str()} ${side.to_str()}"
	"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"${viewbox}\" shape-rendering=\"crispEdges\"><rect width=\"100%\" height=\"100%\" fill=\"#fff\"/><path d=\"${$squares}\" fill=\"#000\"/></svg>"
}
