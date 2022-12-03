-- Run with: dhall text --file 03/main.dhall
let
  -- How do I split this into lines? It seems impossible:
  -- https://stackoverflow.com/questions/54096090/splitting-a-string-in-dhall
  input = ./example.txt as Text
in
  Natural/show 10
