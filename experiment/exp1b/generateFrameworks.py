#!/usr/bin/python3

import argparse
import os
import sys
import re
from subprocess import PIPE, Popen, call
from collections import defaultdict

parameters = {
    "ER_0.033": ["-type","ErdosRenyi", "-ER_probAttacks", "0.033"],
    "ER_0.05": ["-type","ErdosRenyi", "-ER_probAttacks", "0.05"],
    "ER_0.066": ["-type","ErdosRenyi", "-ER_probAttacks", "0.066"],
}

numArguments = 50
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
