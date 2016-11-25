#!/usr/bin/python3

import argparse
import os
import sys
import re
from subprocess import PIPE, Popen, TimeoutExpired
from collections import defaultdict

parameters {
    "ER_0.33": ["-type","ErdosRenyi", "-ER_probAttacks", "0.33"],
    "ER_0.5": ["-type","ErdosRenyi", "-ER_probAttacks", "0.5"],
    "ER_0.66": ["-type","ErdosRenyi", "-ER_probAttacks", "0.66"],
    "WS_3_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "3", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"]
    "WS_10_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "10", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"]
    "WS_30_0.33": ["-type", "WattsStrogatz", "-WS_baseDegree", "30", "-WS_beta", "0.33", "-BA_WS_probCycles", "0.33"]
    "BA_0.33": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.33"]
    "BA_0.5": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.5"]
    "BA_0.66": ["-type", "BarabasiAlbert", "-BA_WS_probCycles", "0.66"]
}

if __name__ == "__main__":
    pass
