## Event/fiber utils

(defn make-broadcast-chan [in-chan]
  (def conn-map @{})

  (ev/spawn
    (forever
      (def value (ev/take in-chan))
      (eachp [k ch] conn-map
        (ev/give ch value)
        )))

  (defn m-connect [self]
    (def code (self :cur-code))
    (def ch (ev/chan 128)) # FIXME: filling up the capacity here might cause chaos
    (set (conn-map code) ch)
    (++ (self :cur-code))
    [code ch])

  (defn m-disconnect [self code]
    (set (conn-map code) nil))

  @{:cur-code 0
    :connect m-connect
    :disconnect m-disconnect})
