(ns cube-bot.macros)

(defmacro else-let
  "bindings => binding-form test
     If test is true, evaluates else with binding-form bound to the value of
     test, if not, yields then"
  ([bindings then]
   `(if-let2 ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~else)
          ~then)))))

(defmacro error-let
  [bindings then]
  (reduce (fn [inner cur]
            (let [form (first cur)
                  tst (second cur)
                  err (nth cur 2)]
              `(let [temp# ~tst]
                 (if temp#
                   (let [~form temp#]
                     ~inner)
                   ~err))))
          then
          (reverse (partition 3 bindings))))
