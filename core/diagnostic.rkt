#lang racket/base

(struct diagnostic (path line col severity rule-id message)
  #:transparent)

(provide (struct-out diagnostic))
