(in-package :csv)

(defconstant +lwsp+ #.(format nil " ~C~C~C" #\Tab #\Return #\Linefeed))

(defun parse-csv-stream (stream)
  "Convert our next line in STREAM into a list of field strings"
  (loop with quotedp = nil
        with fields = ()
        with field = ()
        for char = (read-char stream nil nil)
        while char do
          (case* #'char= char
            (#\,
             (if quotedp
                 (setf field (cons #\, field))
                 (setf fields (cons field fields) field nil)))
            (#\"
             (if (and quotedp (eql (peek-char nil stream nil nil) #\"))
                 (progn (setf field (cons #\" field)) (read-char stream nil nil))
                 (setf quotedp (not quotedp))))
            ((#\Return #\Newline)
             (if quotedp
                 (setf field (cons char field))
                 (progn
                   (loop for c = (peek-char nil stream nil nil)
                         while (and c (find c '(#\Return #\Newline) :test #'char=)) do (read-char stream nil nil))
                   (loop-finish))))
            (t
             (setf field (cons char field))))
        finally (when (or field fields) (return (nreverse (loop for f in (cons field fields) collect (concatenate 'string (nreverse f))))))))

(defun csv-crlf-p (c)
  (or (char= c #\Return) (char= c #\Linefeed)))

(defun csv-quoted-field (string idx length)
  (let ((end (position #\" string :test #'char= :start idx)))
    (if (and end (< (1+ end) length) (char= (char string (1+ end)) #\"))
        (reduce (lambda (a b) (concatenate 'string a b)) (list (subseq string idx end) "\"" (csv-quoted-field string (+ end 2) length)))
        (values (nsubseq string idx end) (when (and end (< end (1- length))) (if (char= (char string (1+ end)) #\,) (+ end 2) (1+ end)))))))

(defun csv-unquoted-field (string idx)
  (let ((end (position-if (lambda (c) (or (char= c #\,) (csv-crlf-p c))) string :start idx)))
    (values (nsubseq string idx end) (when (and end (char= (char string end) #\,)) (1+ end)))))

(defgeneric slurp-csv (file-or-string &key has-header)
  (:documentation "Read FILE-OR-STRING into a list of list of values. Returns (VALUES list-of-lists header-row)")
  (:method ((string vector) &key has-header)
    (slurp-csv (ef:decode-external-string string :utf-8) :has-header has-header))
  (:method ((string string) &key has-header)
    (with-input-from-string (stream string)
      (slurp-csv stream :has-header has-header)))
  (:method ((pathname pathname) &key has-header)
    (with-open-file (stream pathname :direction :input :external-format '(:utf-8)) (slurp-csv stream :has-header has-header)))
  (:method ((stream stream) &key has-header)
    (let ((lines (loop for l = (parse-csv-stream stream) while l collect l)))
      (if has-header
          (values (cdr lines) (car lines))
          lines))))

(defun map-csv-stream (function stream)
  "Run FUNCTION over each list of fields in STREAM"
  (loop for line = (parse-csv-stream stream) while line collect (funcall function line)))

(defun normalise-field (field)
  "Remove leading and trailing whitespace, and coalesce multiple whitespace internally into one string"
  (join " " (lw:split-sequence +lwsp+ (string-trim +lwsp+ field) :coalesce-separators t :test #'char=)))

(defun make-csv-alist (source &key trim)
  (multiple-value-bind (rows header) (slurp-csv source :has-header t)
    (when trim (setf header (mapcar #'normalise-field header)))
    (loop for row in rows
          collect (loop for e in row for h in header collect (cons h (if trim (normalise-field e) e))))))

(defun csv-escape (o)
  (cond ((stringp o) (csv-escape-string o))
        ((null o) "")
        ((eq o t) "true")
        (t o)))

(defun csv-escape-string (s)
  (format nil "\"~{~A~^\"\"~}\"" (lw:split-sequence '(#\") s :test #'char=)))

(defun print-csv-fields (fields stream)
  "Print delimited FIELDS to STREAM"
  (format stream "~{~A~^,~}~%" (map 'list #'csv-escape fields)))
