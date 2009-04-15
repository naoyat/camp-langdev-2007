(use gauche.test)
(test-start "JSON")

(debug-print-width 1024)
(set! *test-report-error* #t)

(use json)
(test-module 'json)

(use peg)

;;;============================================================
(test-section "parser")

(test* "an object"
       '(("Image"
          ("Width"  . 800)
          ("Height" . 600)
          ("Title"  . "View from 15th Floor")
          ("Thumbnail"
           ("Url"    . "http://www.example.com/image/481989943")
           ("Height" . 125)
           ("Width"  . "100"))
          ("IDs" . #(116 943 234 38793))))
       (parse-json "{
   \"Image\": {
       \"Width\":  800,
       \"Height\": 600,
       \"Title\":  \"View from 15th Floor\",
       \"Thumbnail\": {
           \"Url\":    \"http://www.example.com/image/481989943\",
           \"Height\": 125,
           \"Width\":  \"100\"
       },
       \"IDs\": [116, 943, 234, 38793]
     }
}"))

(test* "an array containing two objects"
       '#((("precision" . "zip")
           ("Latitude"  . 37.7668)
           ("Longitude" . -122.3959)
           ("Address"   . "")
           ("City"      . "SAN FRANCISCO")
           ("State"     . "CA")
           ("Zip"       . "94107")
           ("Country"   . "US"))
          (("precision" . "zip")
           ("Latitude"  . 37.371991)
           ("Longitude" . -122.026020)
           ("Address"   . "")
           ("City"      . "SUNNYVALE")
           ("State"     . "CA")
           ("Zip"       . "94085")
           ("Country"   . "US")))
       (parse-json "[
   {
      \"precision\": \"zip\",
      \"Latitude\":  37.7668,
      \"Longitude\": -122.3959,
      \"Address\":   \"\",
      \"City\":      \"SAN FRANCISCO\",
      \"State\":     \"CA\",
      \"Zip\":       \"94107\",
      \"Country\":   \"US\"
   },
   {
      \"precision\": \"zip\",
      \"Latitude\":  37.371991,
      \"Longitude\": -122.026020,
      \"Address\":   \"\",
      \"City\":      \"SUNNYVALE\",
      \"State\":     \"CA\",
      \"Zip\":       \"94085\",
      \"Country\":   \"US\"
   }
]"))

;;;============================================================
(test-section "writer")

(define (test-writer name obj)
  (test* name obj
         (parse-json (->json obj))))

(test-writer "an object"
             '(("Image"
                ("Width"  . 800)
                ("Height" . 600)
                ("Title"  . "View from 15th Floor")
                ("Thumbnail"
                 ("Url"    . "http://www.example.com/image/481989943")
                 ("Height" . 125)
                 ("Width"  . "100"))
                ("IDs" . #(116 943 234 38793)))))

(test-writer "an array containing two objects"
             '#((("precision" . "zip")
                 ("Latitude"  . 37.7668)
                 ("Longitude" . -122.3959)
                 ("Address"   . "")
                 ("City"      . "SAN FRANCISCO")
                 ("State"     . "CA")
                 ("Zip"       . "94107")
                 ("Country"   . "US"))
                (("precision" . "zip")
                 ("Latitude"  . 37.371991)
                 ("Longitude" . -122.026020)
                 ("Address"   . "")
                 ("City"      . "SUNNYVALE")
                 ("State"     . "CA")
                 ("Zip"       . "94085")
                 ("Country"   . "US"))))

(test-end)
