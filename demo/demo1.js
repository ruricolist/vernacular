#lang demo/js

var demo2 = require('demo2.js');

function moo (n) {
    var m = 'moo';
    return m + demo2.oo(n);
}

exports.moo = moo;
