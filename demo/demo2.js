#lang demo/js

function oo (n) {
    n = n || 0;
    var s = "";
    while (n>0) {
        s = s + "oo";
        n--;
    }
    return s;
}

exports.oo = oo;
