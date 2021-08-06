(defpackage yxorp
  (:use #:cl)
  (:import-from #:usocket
                #:socket-stream
                #:socket-server
                #:socket-connect)
  (:import-from #:binding-arrows
                #:some-<>
                #:some->>
                #:->
                #:as->
                #:->>)
  (:export
   #:header
   #:config
   #:ssl-config
   #:*integer-headers*
   #:stop
   #:start
   #:read-config))

(defpackage #:yxorp-config
  (:use #:cl
        #:yxorp
        #:binding-arrows))
