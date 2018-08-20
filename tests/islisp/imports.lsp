#lang core-lisp

(core-lisp:import m :from "exports.lsp"
  :binding (x (cl:function y) (cl:macro-function z)))

(:export-default (list x (y) (z)))
