import bare
import click

from owl.parser import VERBOSE_DEBUG, OwlbearParser
from owl.scanner import OwlbearScanner


def init():
    main()


@click.group()
@click.version_option()
def main():
    """ owlcli tool for playing with owlbear frontend """


@main.command("scan")
@click.argument("owlfile", type=click.File("r"))
def main_scan(owlfile):
    """ Print tokens from file """

    scan = OwlbearScanner()
    scan.continue_after_error = True

    owlcode = owlfile.read()
    tokens = scan.tokenize(owlcode)

    print("-" * 40)
    print(scan.as_string())

    # print("-" * 40)
    # for t in tokens:
    #     print(t)
    #     print(t.highlight())
        # print("\n")


@main.command("parse")
@click.argument("owlfile", type=click.File("r"))
@click.option("--verbose", "-v", is_flag=True)
def main_parse(owlfile, verbose):
    """ Print AST from file """

    scan = OwlbearScanner()
    owlcode = owlfile.read()

    tokens = scan.tokenize(owlcode)

    parser = OwlbearParser(debug=VERBOSE_DEBUG)

    if verbose:
        parser.check_grammar()
        parser.check_sets()
        # parser.dump_grammar()

    ast = parser.parse(tokens)

    print("-" * 40)
    print(ast)


@main.command("build")
def main_build():
    target = bare.Target()

    pack = target.package("std.owlbear")
    mod1 = pack.module("list")
    mod2 = target.module("std.owlbear.map")

    fun1 = bare.Function("fpadd", context=mod1)
    mod1.add(fun1)
    mod1.add(bare.Function("fpadd2", context=fun1, private=True))

    mod2.add(bare.Function("fpadd1", context=mod2))

    print(bare.target)
    target.print()
