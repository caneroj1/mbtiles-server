#!/bin/bash

curl -s http://localhost:3000/tiles/[1-5]/[1-500]/[1-25] > /dev/null && echo "done1" &
curl -s http://localhost:3000/tiles/[5-10]/[500-1000]/[25-50] > /dev/null && echo "done2" & 
curl -s http://localhost:3000/tiles/[10-14]/[1000-1200]/[50-100] > /dev/null && echo "done3" &

wait