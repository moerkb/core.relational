(ns core.relational.examples.sap
  (require [core.relational :refer :all]))

stop

(def foo #rel #{{:pcity "Paris",
               :Parts-in-City #rel #{{:pno "P2", :color "Green", :weight 17, :pname "Bolt"} 
                                     {:pno "P5", :color "Blue",  :weight 12, :pname "Cam"}}} 
          {:pcity "Oslo",
           :Parts-in-City #rel #{{:pno "P3", :color "Blue",  :weight 17, :pname "Screw"}}} 
          {:pcity "London",
           :Parts-in-City #rel #{{:pno "P1", :color "Red",   :weight 12, :pname "Nut"} 
                                 {:pno "P4", :color "Red",   :weight 14, :pname "Screw"} 
                                 {:pno "P6", :color "Red",   :weight 19, :pname "Cog"}}}})

(map (fn [t]
       (map (fn [inner-t] 
              (merge t inner-t))
         (:Parts-in-City t))) 
  foo)