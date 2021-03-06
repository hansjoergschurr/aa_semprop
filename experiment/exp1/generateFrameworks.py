#!/usr/bin/python3

import argparse
import os
import sys
import re
from subprocess import PIPE, Popen, call
from collections import defaultdict

parameters = {
    "ER_0.33": ["-type","ErdosRenyi", "-ER_probAttacks", "0.33"],
    "ER_0.5": ["-type","ErdosRenyi", "-ER_probAttacks", "0.5"],
    "ER_0.66": ["-type","ErdosRenyi", "-ER_probAttacks", "0.66"],
    "WS_4_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "4", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"],
    "WS_10_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "10", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"],
    "WS_30_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "30", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"],
    "BA_0.33": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.33"],
    "BA_0.5": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.5"],
    "BA_0.66": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.66"]
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
