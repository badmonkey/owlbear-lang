import click

from owlbear.scanner import OwlbearScanner
from owlbear.parser import OwlbearParser, VERBOSE_DEBUG


def init():
    main()


@click.group()
@click.version_option()
def main():
    """ owlcli tool for playing with owlbear frontend """


@main.command("scan")
@click.argument("owlfile", type=click.File("r"))
def main_tree(owlfile):
    """ Print tokens from file """

    scan = OwlbearScanner()
    scan.continue_after_error = True

    owlcode = owlfile.read()
    tokens = scan.tokenize(owlcode)

    print("-" * 40)
    for t in tokens:
        print(t)


@main.command("parse")
@click.argument("owlfile", type=click.File("r"))
@click.option("--verbose", "-v", is_flag=True)
def main_tree(owlfile, verbose):
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
