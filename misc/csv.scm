(define-library (picrin csv)
  (import (scheme base)
          (scheme file)
          (srfi 1)
          (picrin regexp))

  

  (export write-csv
          ;make-csv-writer
          read-csv
          ;make-csv-reader
          ))
