#!/bin/bash

TEST_FILE=$1

R_CODE="library('RUnit')"
R_CODE="${R_CODE};library('Biobase')"
R_CODE="${R_CODE};res <- runTestFile('${TEST_FILE}', rngKind='default', rngNormalKind='default')"
R_CODE="${R_CODE};printTextProtocol(res, showDetails=FALSE)"

echo $R_CODE | R --slave
