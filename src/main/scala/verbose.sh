#!/bin/bash
comment() {
    uncomment;
    for f in *.scala; do
        sed 's/println/\/\/println/g' -i "$f";
    done;
}

uncomment() {
    for f in *.scala; do
        sed 's/\/\/println/println/g' -i "$f";
    done;
};

uncomment

