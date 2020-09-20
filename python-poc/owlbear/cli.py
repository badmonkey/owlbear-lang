import click

from owlbear.scanner import OwlbearScanner


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
    owlcode = owlfile.read()
    tokens = scan.tokenize(owlcode)

    for t in tokens:
        print(t)
