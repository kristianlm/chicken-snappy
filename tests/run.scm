(import snappy test)

(test-group
 "decompress"

 (test "uncompress empty" "" (snappy-uncompress "\x00"))

 (test "uncompressed-length" 4 (snappy-uncompressed-length "\x04\x0cAAAA"))
 (test "uncompress" "AAAA" (snappy-uncompress "\x04\x0cAAAA"))
 
 (define uncompressed-length% snappy-uncompressed-length)
 (set! snappy-uncompressed-length (lambda args (+ 10 (apply uncompressed-length% args))))

 (test "uncompressed-length was hacked" 14 (snappy-uncompressed-length "\x04\x0cAAAA"))
 (test "uncompress still works" "AAAA" (snappy-uncompress "\x04\x0cAAAA")))

(test-group
 "compress"

 (test "compress" "\x00"         (snappy-compress ""))
 (test "compress" "\x04\x0cAAAA" (snappy-compress "AAAA")))

(test-exit)
