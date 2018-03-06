#!/usr/bin/env python

from sys import argv
import os
import xlrd


FacilityFile = argv[1]                           # get in-file name from cmd line
RubricFile = argv[2]                             # get out-file name from cmd line


FacilityWorkbook = xlrd.open_workbook(FacilityFile)
FacilityWorksheet = FacilityWorkbook.sheet_by_index(0)

FacilityName = str(FacilityWorksheet.cell(3, 2).value)
print("\n" + "\n" + "\n" + "Processing " + FacilityName + "..." + "\n" + "\n" + "\n")

if 'SC' in FacilityName:
    FacilityLevel = str("SC")
if 'PHC' in FacilityName:
    FacilityLevel = str("PHC")
if 'RH' in FacilityName:
    FacilityLevel = str("RH")


RubricFileOpen = open(RubricFile, "r")

OutFile = "tidy-" + FacilityName + ".csv"
OpenOut = open(OutFile, 'w')          # open out-file in write mode


for RowNumber in range(8,533):


        RubricLine = RubricFileOpen.readline().strip()
        DataValues = FacilityWorksheet.row_values(RowNumber)[4:16]

        OpenOut.write(FacilityName + "," + RubricLine + "," + FacilityLevel + "," + ','.join(str(v) for v in DataValues)  + '\n')


OpenOut.close()
RubricFileOpen.close


