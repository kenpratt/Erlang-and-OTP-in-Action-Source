#!/usr/bin/env python

import sys



def main():
    """ Lets just echo everything we get on stdin to stdout,
    that should be enough to give us a good example """

    while True:
        line = sys.stdin.readline().strip()

        ## We will check and see if its time to exit.
        if line == "stop-good":
            return 0
        elif line == "stop-bad":
            return 1

        sys.stdout.write(line)
        sys.stdout.flush()

if __name__ == "__main__":
    sys.exit(main())
