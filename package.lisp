(defpackage yxorp
  (:use #:cl)
  (:import-from #:usocket
                #:address-in-use-error
                #:socket-stream
                #:socket-server
                #:socket-connect)
  (:import-from #:binding-arrows
                #:some->
                #:-<>
                #:->
                #:as->
                #:->>)
  (:import-from #:flexi-streams
                #:string-to-octets
                #:octets-to-string)
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
   #:websocket-p
   #:read-headers
   #:read-body
   #:write-headers
   #:write-body-and-headers
   #:track-thread
   #:map-threads))

(defpackage #:yxorp-config
  (:use #:cl
        #:yxorp
        #:binding-arrows))
