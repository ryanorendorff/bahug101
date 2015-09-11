import re
from sys import stdout, stdin

def addFragile(match):
    #print("Calling addFragile")

    options = match.group("options")
    title = match.group("title")

    if options is None and title is None:
        return r"\begin{frame}[fragile]"

    if options is None:
        return r"\begin{{frame}}[fragile]{{{title}}}".format(**locals())

    return r"\begin{{frame}}[fragile, {options}]{{{title}}}".format(**locals())


def matchFragile(s):

    matches = re.subn("^\\\\begin{frame}(?:\[(?P<options>.*)\])*(?:{(?P<title>.*)})*", addFragile, s)[0]

    #print("matches are: {}".format(matches))
    return(matches)

if __name__ == '__main__':

    for line in stdin:
        stdout.write(matchFragile(line))
