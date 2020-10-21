#!/bin/bash

echo -n "store cnt: "
grep "accept req" $1 | wc -l

echo -n "dcache req cnt: "
grep "send buf" $1 | wc -l

echo -n "req[0] blocked: "
grep "\[0\] blocked by sbuffer" $1 | wc -l

echo -n "req[1] blocked: "
grep "\[1\] blocked by sbuffer" $1 | wc -l

echo -n "sbuffer cnt = 15: "
grep "sbuffer entry cnt: 15" $1 | wc -l

echo -n "sbuffer cnt = 16: "
grep "sbuffer entry cnt: 16" $1 | wc -l
