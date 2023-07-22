(defpackage yxorp
  (:use #:cl)
  (:import-from #:usocket
                #:address-in-use-error
                #:socket-stream
                #:socket-server
                #:socket-connect)
  (:import-from #:binding-arrows
                #:some->>
                #:some-<>>
                #:some->
                #:-<>
                #:->
                #:as->
                #:->>)
  (:import-from #:flexi-streams
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:rutils
                #:alist->ht
                #:ht->alist)
  (:import-from #:smart-buffer
                #:make-smart-buffer)
  (:import-from #:alexandria
                #:compose)
  (:export
   #:*suppress-errors*
   #:header
   #:*headers*
   #:*request-headers*
   #:*response-headers*
   #:config
   #:ssl-config
   #:*integer-headers*
   #:stop
   #:start
   #:read-config
   #:valid-destination-p
   #:destination-parts
   #:port
   #:destination
   #:websocket-p
   #:read-headers
   #:write-headers
   #:write-body-and-headers
   #:track-thread
   #:map-threads
   #:extract-charset
   #:extract-encodings))

(defpackage yxorp-config
  (:use #:cl
        #:yxorp
        #:binding-arrows))
