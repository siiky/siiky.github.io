(import
  chicken.pathname
  chicken.process-context
  openssl
  geminid)

(define-constant ciphers "ECDH+CHACHA20:ECDH+AESGCM:ECDH+AES256:ECDH+AES128:!aNULL:!SHA1")

(define listener
  (ssl-listen*
    hostname: "localhost"
    port: 1965
    protocol: 'tlsv12
    cipher-list: ciphers
    certificate: "cert.pem"
    private-key: "key.pem"
    private-key-type: 'prime256v1
    verify?: #f
    ))

(parameterize ((root-path (make-absolute-pathname (current-directory) "root")))
  (start-server listener))
