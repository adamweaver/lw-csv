# Cromulent CSV parser
Reads comma separated values from a vector, pathname, or string

``` common-lisp
(CSV:SLURP-CSV file-or-string &key has-header)
(CSV:MAP-CSV-STREAM function stream)
(CSV:MAKE-CSV-ALIST &key trim) ; Runs 'CSV:NORMALISE-FIELD over each entry
(CSV:NORMALISE-FIELD string) ; " apples     are \n red " => "apples are red"
```


