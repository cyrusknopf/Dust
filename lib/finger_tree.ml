module FingerTree =
    struct
        type 'a ftree =
            | Empty
            | Single
            | Deep (Digit 'a) * (ftree (Node 'a)) * (Digit 'a)
    end
