#!/usr/bin/env roc
## All tests: the package's `expect` blocks, then our codes against
## qrencode's and, where zbarimg is installed, read back by it.
##
## The `expect` blocks in QrCode.roc pin a few codes module by module, the
## capacities of the standard's table 7 at both ends, and the masks.
## tests/harness.roc compares 960 codes, every version at every level in each
## mode, with the ones qrencode made (tests/qrencode.txt), and has zbarimg read
## the text back from each of our own codes. It is built with --opt=speed,
## since a version 40 code is slow to make under the dev backend. The dev
## shell has zbarimg:
##
##     nix develop -c ./tests.roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
}

import pf.Stdout
import pf.Cmd
import pf.OsStr

main! = |_| {
	Stdout.line!("== unit tests")?
	run!("roc", ["test", "package/main.roc"])?

	Stdout.line!("== against qrencode and zbarimg (compiled --opt=speed)")?
	build!("tests/harness.roc", "tests/harness")?
	run!("./tests/harness", [])?
	Ok({})
}

# Build a .roc file to a named binary with the LLVM speed backend.
build! : Str, Str => Try({}, [Exit(I32)])
build! = |src, out| run!("roc", ["build", "--opt=speed", src, "--output=${out}"])

# Run a command with inherited stdio, failing the script on a nonzero exit.
run! : Str, List(Str) => Try({}, [Exit(I32)])
run! = |program, arguments| {
	Cmd.call!(OsStr.from_str(program), arguments.map(OsStr.from_str)) ? |_| Exit(1)
	Ok({})
}
