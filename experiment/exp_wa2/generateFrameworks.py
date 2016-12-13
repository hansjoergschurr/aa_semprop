#!/usr/bin/python3

import argparse
import os
import sys
import re
from subprocess import PIPE, Popen, call
from collections import defaultdict

parameters = {
    "WS_2_0.01": ["-type", "WattsStrogatz", "-WS_baseDegree", "4", "-WS_beta", "0.01", "-BA_WS_probCycles", "0.66"],
    "WS_2_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "4", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.66"],
    "WS_2_0.66": ["-type", "WattsStrogatz", "-WS_baseDegree", "4", "-WS_beta", "0.66", "-BA_WS_probCycles", "0.66"],
    "WS_4_0.01": ["-type", "WattsStrogatz", "-WS_baseDegree", "2", "-WS_beta", "0.01", "-BA_WS_probCycles", "0.66"],
    "WS_4_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "2", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.66"],
    "WS_4_0.66": ["-type", "WattsStrogatz", "-WS_baseDegree", "2", "-WS_beta", "0.66", "-BA_WS_probCycles", "0.66"],
    "WS_6_0.01": ["-type", "WattsStrogatz", "-WS_baseDegree", "6", "-WS_beta", "0.01", "-BA_WS_probCycles", "0.66"],
    "WS_6_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "6", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.66"],
    "WS_6_0.66": ["-type", "WattsStrogatz", "-WS_baseDegree", "6", "-WS_beta", "0.66", "-BA_WS_probCycles", "0.66"],
}

numArguments = 50
numFramesPerParameter = 190
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
