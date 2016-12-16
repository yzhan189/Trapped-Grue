(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map  ;  10 static rooms + 5 dynamic rooms   13 unconditional objs + 4 conditional objs
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.
You get the feeling it was just created for a game or something."
           :title "in the foyer"
           :dir {:south :grue-pen :west :DNE :east :kitchen :upstairs :mirror-room}
           :hint "Seems like there is something on the ground."
           :contents #{:raw-egg }}
   :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue. Are you going to fight? "
              :title "in the grue pen"
              :hint "Don't you know that grue loves raw eggs?"
              :dir {:north :foyer :downstairs :basement :south :truth-room}
              };alter it so that grue doesn't eat you
   :basement { :title "in the basement"
               :desc "Something is glowing in the dark."
               :hint "Seems like there is something on the ground."
               :dir {:upstairs :grue-pen}
               :contents #{:spirit3 }}
   :DNE {:desc "Full of NULL pointers and Segment Faults!!! "
         :title "in the room DoesNotExist"
         :hint "STAY IN MY NIGHTMARE."
         :dir {:south :gallery :east :foyer}}
   :kitchen {:title "in kitchen"
             :desc "It is a clean and tidy full kitchen, you may have a decent meal here!"
             :hint "You are hungry. And this is kitchen."
             :dir {:south :lab :west :foyer}
             :contents #{:cooked-egg }}
   :gallery {:title "in gallery"
             :hint "You can pick something up."
             :desc "A piece of grue lives here."
             :dir {:north :DNE}
             :contents #{:spirit1 }}
   :lab {:title "in lab"
         :hint "Seems like there is something on the ground."
         :dir {:north :kitchen}
         :desc "You can create somthing here."
         :contents #{:beaker}}
   :truth-room {:title "in truth-room"
                :hint "Seems like there is something on the ground."
                :dir {:north :grue-pen}
                :desc "You can ask for truth."
                }

   :bedroom {:title "in bedroom"
            :hint "There is a piece of grue under the bed."
            :dir {:west :study-room :east :bar :south :balcony}
            :desc "Do not sleep on the bed."
            :contents #{:spirit2 }}
   :ball-room {:title "in ball-room"
               :hint "Seems like there is something on the ground."
               :desc "Shut up and listen to the whisper."
               :dir {:south :study-room :east :mirror-room}
               :contents #{:spell1 }}
   :bath-room {:title "in bath-room"
               :hint "Seems like there is something on the ground."
               :dir {:west :mirror-room :south :bar}
               :desc "Shut up and listen to the whisper."
               :contents #{:spell2 }}
   :study-room {:title "in study-room"
                :hint "Seems like there is something on the ground."
                :dir {:north :ball-room :east :bedroom}
                :desc "Shut up and listen to the whisper."
                :contents #{:spell3 }}
   :bar {:title "in bar"
         :hint "Seems like there is something on the ground."
         :dir {:west :bedroom :north :bath-room}
         :desc "Shut up and listen to the whisper."
         :contents #{:spell4 }}
   :mirror-room {:desc "It is a room full of mirrors.
Be careful with the direction you choose! "
                 :title "in mirror-room"
                 :hint "Opposite directions and useless thing on the ground."
                 :dir {:west :bath-room :east :ball-room :downstairs :foyer}
                 :contents #{:broken-mirrors}} ;dir flipped
   :balcony {:title "in the balcony"
             :desc "It is very large."
             :hint "As the game developper, I'm telling you that this is the exit! "
             :dir {:north :bedroom}
             :contents #{:Patronus }}
   })

(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :energy 0
   :grue-love 0
   :seen #{}})

(defn check [player]
  ( let [inventory (player :inventory)]
    (print (str "You have "))
    (if (empty? inventory)
      (print (str "nothing"))
      (doseq [ v inventory] (print( str v " "))) )
    (do (println ". ") )
    player))

(defn soul [themap player]
  (do (if (contains? (player :inventory) :grue-soul)
        (update-in themap [:grue-pen] assoc :desc (str "The spirit of grue glows in dark. "))
        (do themap))))

(defn beaker [themap player]
  (do (if (contains? (player :inventory) :beaker)
        (update-in themap [:lab] assoc :contents #{}  )
        (do themap))))
(defn broken-mirrors [themap player]
  (do (if (contains? (player :inventory) :broken-mirrors)
        (update-in themap [:mirror-room] assoc :contents #{}  )
        (do themap))))
(defn raw-egg [themap player]
  (do (if (contains? (player :inventory) :raw-egg)
        (update-in themap [:foyer] assoc :contents #{}  )
        (do themap))))
(defn cooked-egg [themap player]
  (do (if (> (player :energy) 0 ) ;cooked
        (update-in themap [:kitchen] assoc :contents #{:cooked-egg}  )
        (do themap))))

(defn status [player themap]  ;status associate
  (let [location (player :location)]
    (print (str "You are " (-> themap location :title) ". \n"))
    (when-not ((player :seen) location) ;when enter from other room
      (print (-> themap location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))



(defn pickup [player themap]
  (let [location (player :location)
        items (->> themap location :contents )
        item (first items)]
    (if (nil? item)
      (do (println "There is nothing to pick up.") player)
      (do (print (str "You recieve " item))
          (println "")
          (if (= item :spell1)
            (do (println "Spell 1: Benedict Cumberbatch"))(print (str "")))
          (if (= item :spell2)
            (do (println "Spell 2: Shibutani Subaru"))(print (str "")))
          (if (= item :spell3)
            (do (println "Spell 3: Matsuko Deluxe"))(print (str "")))
          (if (= item :spell4)
            (do (println "Spell 4: Washington DC is not in Washington"))(print (str "")))
          (update-in player [:inventory] #(conj % item)) )) ))

(defn tock [player]  ;timer
  (update-in player [:tick] inc))


(defn help [player]
  (do (println "Try: look -- see where you are")
      (println "     check -- see what you have")
      (println "     north, south, east, west, upstairs, downstairs -- move around")
      (println "     pickup -- receive objects")
      (println "     hint, quit, help -- game options")
      (if (< (player :grue-love) 1)
        (do (println "(Hidden actions are not shown. )"))
        (do (println "The more grue loves you, the more tips you will get.")
            (if (> (player :grue-love) 1)
              (do
                (println "     fight -- your energy decremetns")
                (println "     feed -- grue-love increments")
                (println "     cook -- generate cooked food and your energy increments")
                (println "     mix -- use beaker to produce pieces of spirits")
                (if (> (player :grue-love) 2)
                  (do  (println "     ask -- ask grue in truth room to know the correct spell")
                       (println "     shout -- shout correct spell on balcony and get Patronus")
                       (println "     escape -- escape from balcony with sufficient energy")
                       (println "" )
                       (println "Tips: ")
                       (println "Do NOT stay in the room does not exist. Nothing's happening.")
                       (println "Eat more to get enough energy."))
                  (do (println "") )))
              (do (println ""))) ))
  player))

(defn hint [player]
  ( let [location (player :location)]
    (print (str  (-> the-map location :hint) ))
    (do (println ""))
  player) )

(defn escape [player]
  (let [location (player :location)]
    (if (= location :balcony )
      (do (if (contains? (player :inventory) :Patronus )
            (do (if (> (player :energy) 1)
                (do (println "You have spell to break the curse.")
                  (println "You free grue's soul and yourself.")
                  (println "Oh! you take the secret and leave the house.")
                  (println "Farewell~ Never let me see you again :) " )
                  (System/exit 0))
                (do (println "You are too tired to do so."))))
            (do (println "If you jump, you die :( You need Patronus."))))

      (do (println "All windows are locked.")))

    player))

(defn cook [player]
  (let [location (player :location)]
    (if  (= location :kitchen )
      (do (if (contains? (player :inventory) :raw-egg)
            (do (println "No other food in kitchen, so you boil some eggs. Go pick up cooked eggs. ")
                (update-in player [:energy] inc ) )
            (do (println "Unfortunately, there is no food left.") player) ) )

      (do (println "Come on, yall cannot cook outside kitchen.") player) )
    )) ;set energy to 1



(defn feed [player]
  (let [location (player :location)]
    (if  (= location :grue-pen )
      (do (if (contains? (player :inventory) :grue-soul)
            (do (println "Grue has soul now.")
               player)
            (do (if (contains? (player :inventory) :raw-egg)
                  (do (if (> (player :grue-love) 0)
                        (do (println "Grue loves you, go for its spirit. ")player)
                        (do (print (str "Grue starts to love you. " ))player))
                      (println "Grue loves you more.")
                      (update-in player [:grue-love] inc ))
                  (do (println "Seriously? Are you going to feed grue with yourself?") player)))))
      (do (println "No one is there to be fed.")player))
    )) ;grue disapper , soul appears

(defn fight [player]
  (let [location (player :location)]
    (if  (= location :grue-pen )
      (do (println "You lose the fight. (for sure...) ")
          (println "And grue hates you more.")
          (update-in player [:grue-love] dec ))
      (do (println "No one is there to fight.") player))
    ))

(defn mix [player]
  (let [location (player :location)]
    (if  (= location :lab )
      (do (if (and (and (contains? (player :inventory) :spirit3) (contains? (player :inventory) :spirit1) )
                   (and (contains? (player :inventory) :spirit2) (contains? (player :inventory) :beaker) ))
            (do (println "You own grue's soul now.")
                (println "Go to truth room and ask for truth.")
                (update-in player [:inventory] #(conj % :grue-soul )) )
            (do (println "Not enough materials.") player) ))
      (do (println "It is very dangerous to do so outside of lab.") player))
    ))

(defn ask [player]
  (let [location (player :location)]
    (if  (= location :truth-room )
      (do (if (contains? (player :inventory) :grue-soul)
            (do (println "Grue: \"Shout out spell 2 on balcony and I will be released.\" "))
            (do (println "Only grue knows the truth."))))
      (do (println "You have to go to truth room for truth.") ))
  player) )

(defn shout [player]
  (let [location (player :location)
        content (read-line)]

    (if  (= location :balcony )
      (do (if (= content "Shibutani Subaru")
            (do (println "The spell is correct, grue is relieved. ")
		            (println "The truth of grue is your Patronus. You get Patronus.")
            		(update-in player [:inventory] #(conj % :Patronus ) ))
            (do (println "Wrong Spell! You are losing energy")
            		(update-in player [:energy] dec ) )))
      (do (print (str content "... (echo) "  ) ) player ))
  ) )

(defn respond [player command themap] ; 19 commands
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [:check] (check player)

         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:east] (go :east player)
         [:west] (go :west player)
         [:upstairs] (go :upstairs player)
         [:downstairs] (go :downstairs player)

         [:pickup] (pickup player themap) ;recieve item
         [:mix] (mix player)
         [:ask] (ask player)
         [:cook] (cook player)
         [:feed] (feed player)
         [:escape] (escape player)
         [:fight] (fight player)
         [:help] (help player) ;print out all command
         [:quit] ((System/exit 0)) ;quit game
         [:hint] (hint player) ;give hints
         [:shout] (shout player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [m4 (cooked-egg local-map local-player)
          m3 (broken-mirrors m4 local-player)
          m2 (soul m3 local-player)
          m1 (raw-egg m2 local-player)
          m  (beaker m1 local-player)
          pl (status local-player m)
          _  (println "What do you want to do?")
          command (read-line)]
     ; (print (-> m :lab :contents))
     ; (print (-> local-player :inventory))
      (recur m (respond pl (to-keywords command) m )))))
