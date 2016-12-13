#!/usr/bin/python3

import argparse
import os
import sys
import re
from subprocess import PIPE, Popen, call
from collections import defaultdict

numArguments = 50

parameters = {
    "ER_dg2": ["-type","ErdosRenyi", "-ER_probAttacks", str(2.0/(numArguments+1))],
    "ER_dg3": ["-type","ErdosRenyi", "-ER_probAttacks", str(3.0/(numArguments+1))],
    "ER_dg4": ["-type","ErdosRenyi", "-ER_probAttacks", str(4.0/(numArguments+1))],
    "ER_dg5": ["-type","ErdosRenyi", "-ER_probAttacks", str(5.0/(numArguments+1))],
}

numFramesPerParameter = 40
outDir = "out"
genJar = "jAFBenchGen.jar"

if __name__ == "__main__":
    for p in parameters:
        v = parameters[p]
        print("Parameters {}: {}".format(p,v))
        d = os.path.join(outDir, p)
        if not os.path.exists(d):
            os.makedirs(d)
        for n in range(1,numFramesPerParameter+1):
            print("{}/{}".format(n, numFramesPerParameter), end="\r")
            with open(os.path.join(d, "{}_{}.apx".format(p,n)), 'w') as outfile:
                cmd = ["java", "-jar", genJar, "-numargs", str(numArguments)] + v
                call(cmd, stdout=outfile)
