(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:Great_Hall {:desc "The house is completely dark. You try the front door but it's locked."
           :title "in the Great Hall"
           :commands "Commands: left, right, up."
           :dir {:left :Family_Room
                 :right :Kitchen
                 :up :Main_Second_Floor}
           :contents #{}}


   :Family_Room {:desc "There is a door across the room, or you can go right."
              :title "in the Family Room"
              :commands "Commands: right, door, explore."
              :dir {:right :Great_Hall
                    :down :Basement}
              :contents :book}

   :Kitchen {:desc "You notice some food on the table."
             :commands "Commands: right, left, explore."
              :title "in the Kitchen"
              :dir {:right :Dining_Hall
                    :left :Great_Hall}
              :contents :dog_treat}

   :Dining_Hall {:desc "The dining table is huge. There seems to be some batteries on the table. Pick up batteries?"
              :commands "Commands: up, left, pickup."
              :title "in the Dining Hall"
              :dir {:up :Main_Second_Floor
                    :left :Kitchen}
              :contents :batteries}

   :Main_Second_Floor {:desc "There is a room to your left and right. Or you can take the stairs up or down. "
              :title "on the main second floor"
              :commands "Commands: left, right, up, down."
              :dir {:left :Master_Bedroom
                    :down :Great_Hall
                    :up :Main_Third_Floor
                    :right :Dog_room}
              :contents #{}}

   :Master_Bedroom {:desc "You walk into the bedroom. You can go to the closet, bathroom, or go back out(left)."
              :title "in the Master Bedroom"
              :commands "Commands: left, closet, bathroom, explore."
              :dir {:closet :Closet
                    :bathroom :Bathroom
                    :left :Main_Second_Floor}
              :contents :Flashlight}

   :Bathroom {:desc "You notice something sharp on the floor."
              :title "in Bathroom"
              :commands "Commands: leave, explore."
              :dir {:leave :Master_Bedroom}
              :contents :hook}

   :Closet {:desc "There is a a bunch of clothes hanging. On the drawer you notice some gloves."
              :title "in the Closet"
              :commands "Commands: leave, grab."
              :dir {:leave :Master_Bedroom}
              :contents :Gloves}

   :Basement {:desc "You open the door and walk downstairs. It's completely dark. Use flashlight?"
              :title "in the Basement"
              :commands "Commands: up."
              :dir {:up :Family_Room
                    :fireplace :Fireplace}
              :contents #{}}

   :Fireplace {:desc "You walk up to the Fireplace and notice that there is a three number combo."
              :title "at the Fireplace"
              :commands "Commands: back"
              :dir {:117 :Inside_fireplace
                    :back :Basement}
              :contents #{}}


   :Main_Third_Floor {:desc "You walk up to the third floor. There are four ways you can go."
              :title "on the third floor"
              :commands "Commands: down, nw, ne, se."
              :dir {:down :Main_Second_Floor
                    :nw :Library
                    :ne :Observatory
                    :se :Gallery}
              :contents #{}}

   :Library {:desc "The shelves tower above you. There seems to be space to insert one more book in one of the shelves. Use book?"
              :title "in the Library"
              :commands "Commands: back."
              :dir {:back :Main_Third_Floor}
              :contents :numbers}

   :Observatory {:desc "The ceiling is painted with constellations that make the room look like the night sky."
              :title "in the Observatory"
              :commands "Commands: leave, explore."
              :dir {:leave :Main_Third_Floor}
              :contents :bow}

   :Dog_room {:desc "You see a big dog at the end of the room. Use treat?"
              :title "in the Dog Room"
              :commands "Commands: back."
              :dir {:back :Main_Second_Floor}
              :contents :hook}

   :Gallery {:desc "The picture in the room seem to be staring at you."
              :title "in the Gallery"
              :commands "Commands: back, explore"
              :dir {:back :Main_Third_Floor}
              :contents :key}

   :Inside_fireplace {:desc "You see a light at the top. If only you could find a way to get up there."
              :title "inside the fireplace"
              :commands "Commands: back, make"
              :dir {:back :fireplace
                    :climb :roof}
              :contents #{}}

    :roof {:desc "You climb up and reach the roof. You have made it out of the house!"
              :title "on the roof"
              :commands " "
              :dir {}
              :contents #{}}

   })
(keyword :grappling_hook)

(def adventurer
  {:location :Great_Hall
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (println (str "You are " (-> the-map location :title) ". "))
    ;(when-not ((player :seen) location)
    (println (-> the-map location :desc))
    (println (-> the-map location :commands))
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

(defn tock [player]
  (update-in player [:tick] inc))


(defn invent [player]
  (do(println (player :inventory))
      player))


(defn explore [player]
  (let [room (get-in player [:location])]
    (let [item (get-in the-map [room :contents])]
      (update-in player [:inventory] #(conj % item))
      (do (println "You found" (-> item) "."))))player)

(defn pickup [player]
  (let [room (get-in player [:location])]
    (let [item (get-in the-map [room :contents])]
      (if (or (= room :Dining_Hall) (= room :Library) (= room :Dog_room))
        (do(println "Picked up" (-> item) ".")
        (update-in player [:inventory] #(conj % item)))
        player))))

(defn grab [player]
  (let [room (get-in player [:location])]
    (let [item (get-in the-map [room :contents])]
      (if (= room :Closet)
        (do(println "Picked up" (-> item) ".")
        (update-in player [:inventory] #(conj % item)))
        player))))

(defn flashlight [player]
    (let [room (get-in player [:location])
          invent (player :inventory)]
    (let [item (get-in the-map [room :contents])]
      (if (and (= room :Basement) (contains? invent :Flashlight))
        (do(println "You see a fireplace across the room. Go to fireplace? (fireplace)"))
        player)))player)

(defn treat [player]
    (let [location (player :location)
          invent (player :inventory)
          item (the-map [location :contents])]
      (if (and (= location :Dog_room) (contains? invent :dog_treat))
        (do(println "The dog moves out of the way and you find a metal hook. Pickup hook?")
        (update-in player [:inventory] #(conj % item)) player)
        (do (println "You need to be in the dog room and have the treat!") player))))

(defn book [player]
    (let [location (player :location)
          invent (player :inventory)
          item (get-in the-map [location :contents])]
      (if (and (= location :Library) (contains? invent :book))
        (do(println "You place the book on the shelf and it moves slightly to the right. A small paper falls to the ground. It reads 117. Pickup?")
        (update-in player [:inventory] #(conj % item)) player)
        (do (println "You need to have the book and be in the library!")player))))


(defn read_paper [player]
  (let [invent (player :inventory)]
  (if (contains? invent :numbers)
   (do (println "The numbers on the paper read 117")
    player))))



(defn shoot [player]
  (let [location (player :location)
        invent (player :inventory)]
    (if (and (contains? invent :rope) (contains? invent :hook) (contains? invent :bow) (= location :Inside_fireplace))
      (do (println "You shoot the grappling hook up. It gets stuck and the rope becomes sturdy enough to climb up"
        (update-in player [:inventory] #(conj % :gh))) player)
      (do (println "You need to be inside the fireplace and have the grappling hook made!") player))))

(defn door [player]
  (let [location (player :location)
        invent (player :inventory)]
    (if (contains? invent :key)
      (do (println "You unlock the door. Go down?")player)
      (do (println "You dont have a key!")player))))

(defn climb [player]
  (let [location (player :location)
        invent (player :inventory)]
    (if (and (= location :Inside_fireplace) (contains? invent :hook) (contains? invent :rope) (contains? invent :bow) (contains? invent :bow))
      ((do (println "You create a grapple and shoot it to the top. You put on your gloves and climb it up. You have made it out of the house!")player)
        (System/exit 0))
      (do (println "You dont have everything to climb up!") player))))



(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [:bag] (invent player)
         [:explore] (explore player)
         [:pickup] (pickup player)
         [:grab] (grab player)
         (:or [:flashlight] [:fl]) (flashlight player)
         [:treat] (treat player)
         [:read] (read_paper player)
         [:book] (book player)
         [:shoot] (shoot player)
         [:door] (door player)
         [:climb] (climb player)

         (:or [:l] [:left] ) (go :left player)
         [:left] (go :left player)
         [:right] (go :right player)
         [:up] (go :up player)
         [:down] (go :down player)

         [:closet] (go :closet player)
         [:bathroom] (go :bathroom player)
         [:leave] (go :leave player)
         [:nw] (go :nw player)
         [:ne] (go :ne player)
         [:sw] (go :sw player)
         [:se] (go :se player)
         [:fireplace] (go :fireplace player)
         [:back] (go :back player)
         [:117] (go :117 player)



         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))



