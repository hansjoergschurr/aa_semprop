#!/usr/bin/python3

import csv
import os.path

def genBool(s):
    if s == "False":
        return 1
    else:
        return 0

class Config:
    def __init__(self, name):
        self.name = name
        self.args = 0
        self.prfNum = 0
        self.prfExts = 0
        self.prfNumTight = 0
        self.prfRJ = 0
        self.prfIC = 0
        self.prfICNR = 0
        self.semNum = 0
        self.semExts = 0
        self.semNumTight = 0
        self.semRJ = 0
        self.semIC = 0
        self.semICNR = 0

    def addLine(self, line):
        if line[1] == "NaN":
            return
        self.args = int(line[1])
        if line[2] != "NaN":
            self.prfNum = self.prfNum + 1
            self.prfExts = self.prfExts + int(line[2])
            self.prfNumTight = self.prfNumTight + genBool(line[3])
            self.prfRJ = self.prfRJ + int(line[4])
            self.prfIC = self.prfIC + int(line[5])
            self.prfICNR = self.prfICNR + int(line[6])
        if line[7] != "NaN":
            self.semNum = self.semNum + 1
            self.semExts = self.semExts + int(line[7])
            self.semNumTight = self.semNumTight + genBool(line[8])
            self.semRJ = self.semRJ + int(line[9])
            self.semIC = self.semIC + int(line[10])
            self.semICNR = self.semICNR + int(line[11])

    def printStats(self):
        if max(self.prfNum,self.semNum) == 0:
            print("{}\tNo entries.".format(self.name))
        else:
            pn = self.prfNum
            sn = self.semNum
            print("{} & {} & {}\\% & {:.2f} & {:.2f} & {:.2f} & {:.2f} & {}\\%  & {:.2f} & {:.2f} & {:.2f} & {:.2f}\\\\".format(
            self.name, self.args, self.prfNumTight*100.0/pn, self.prfExts/pn, self.prfRJ/pn, self.prfIC/pn, self.prfICNR/pn,
            self.semNumTight*100.0/sn, self.semExts/sn, self.semRJ/sn, self.semIC/sn, self.semICNR/sn))

if __name__=="__main__":
    out = dict()
    with open('output.csv', newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=',', quotechar='|')
        next(csvreader)
        for row in csvreader:
            (name, f) = os.path.split(row[0])
            try:
                out[name].addLine(row)
            except KeyError:
                out[name] = Config(name)
                out[name].addLine(row)

    for k in out:
        out[k].printStats()
