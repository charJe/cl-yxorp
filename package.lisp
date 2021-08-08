(defpackage yxorp
  (:use #:cl)
  (:import-from #:usocket
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
   #:header
   #:config
   #:ssl-config
   #:*integer-headers*
   #:stop
   #:start
   #:read-config
   #:websocket-p))

(defpackage #:yxorp-config
  (:use #:cl
        #:yxorp
        #:binding-arrows))
