(import jaylib :as j)
(import spork/netrepl)

(def bg-color [0.2 0.2 0.2])

(def screen-width 640)
(def screen-height 480)

(def square-size 20)
(def squares-h (/ screen-width square-size))
(def squares-v (/ screen-height square-size))

(var game-state @{:paused? true
                  :score 0
                  :state :playing})

(var accumulated-time 0)

(var snake @{:head [10 20]
             :tail @[[10 21] [10 22] [11 22]]
             :speed 2
             :color :green
             :dir :right})

(var apple [10 10])



(defn add-points [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(def dir->vec
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]})

(defn out-of-bounds-wrap [[x y] max-x max-y]
  [(cond
     (> x (dec max-x)) 0
     (< x 0) (dec max-x)
     x)
   (cond
     (> y (dec max-y)) 0
     (< y 0) (dec max-y)
     y)])

(defn randomize-apple []
  (set apple [(math/floor (* (math/random) squares-h))
              (math/floor (* (math/random) squares-v))]))

(defn update-snake-head [dir head]
  (-> (dir->vec dir)
      (add-points head)
      (out-of-bounds-wrap squares-h squares-v)))

(defn tick-snake [snake]
  (array/insert (get snake :tail) 0 (get snake :head))
  (update snake :head (partial update-snake-head (get snake :dir)))
  (def eat?
    (if (= (get snake :head) apple)
      (do
        (randomize-apple)
        (update game-state :score inc)
        true)
      false))

  (when
    (some (fn [tails] (= tails (get snake :head))) (get snake :tail))
    (put game-state :state :game-over))

  (when (not eat?)
    (array/pop (get snake :tail))))

(defn draw-snake [snake]
  (each [x y] (get snake :tail)
    (j/draw-rectangle (inc (* square-size x))
                      (inc (* square-size y))
                      (dec square-size)
                      (dec square-size)
                      [0.8 0 0.8]))
  (let [[x y] (get snake :head)]
    (j/draw-rectangle (inc (* square-size x))
                      (inc (* square-size y))
                      (dec square-size)
                      (dec square-size)
                      [0.2 0.7 0.2]))) 

(defn init-game []
  (set snake @{:head [10 20]
               :tail @[[10 21] [10 22] [11 22]]
               :speed 2
               :color :green
               :dir :right})
  (set game-state @{:paused? false
                    :score 0
                    :state :playing})
  (randomize-apple))

(defn handle-events []
  (when (and (j/key-pressed? :w)
             (not= :down (snake :dir)))
    (put snake :dir :up))
  (when (and (j/key-pressed? :d)
             (not= :left (snake :dir)))
    (put snake :dir :right))
  (when (and (j/key-pressed? :a)
             (not= :right (snake :dir)))
    (put snake :dir :left))
  (when (and (j/key-pressed? :s)
             (not= :up (snake :dir)))
    (put snake :dir :down))

  (when (j/key-pressed? :p)
    (update game-state :paused? not))

  (when (j/key-pressed? :space)
    (init-game)))

(defn update-game []
  (set accumulated-time (+ accumulated-time (j/get-frame-time)))

  (let [snake-speed (/ 1 (+ (get snake :speed) (/ (get game-state :score) 2)))]
    (when (> accumulated-time snake-speed)
      (set accumulated-time (- accumulated-time snake-speed))
      (tick-snake snake))))

(defn draw-apple [apple]
  (let [[x y] apple
        start-x (* square-size x)
        start-y (* square-size y)]
    (j/draw-rectangle (+ 4 start-x)
                      (+ 4 start-y)
                      (- square-size 6)
                      (- square-size 6)
                      :red)))

(defn draw []
  (j/begin-drawing)
  (j/clear-background bg-color)

  (loop [x :range [0 squares-h]
         y :range [0 squares-v]]
    (let [start-x (* square-size x)
          start-y (* square-size y)]
      (j/draw-rectangle start-x
                        start-y
                        square-size
                        square-size
                        [0.2 0.2 0.2])
      (j/draw-rectangle (inc start-x)
                        (inc start-y)
                        (dec square-size)
                        (dec square-size)
                        :black)))
  (draw-snake snake)
  (draw-apple apple)

  (when (game-state :paused?)
    (j/draw-text "Game Paused" (/ screen-width 2) (/ screen-height 2) 32 :green))

  (when (= :game-over (game-state :state))
    (j/draw-text "Game Over" (/ screen-width 2) (/ screen-height 2) 32 :green))

  (j/draw-text (string "Score: " (get game-state :score))
               (- screen-width 120)
               10 24 :green)
  (j/draw-fps 10 10)
  (j/end-drawing))

(def event-chan (ev/chan 100)) 

(defn command-handler [command]
  (xprintf stdout (string "Hello " command))) 

(defn command-listener []
  (forever
    (ev/sleep 0)
    (let [command (ev/take event-chan)]
      (command-handler command)))) 

(defn game-loop []
  (def commands @[])
  (handle-events)
  (when (and (not (game-state :paused?))
             (= :playing (get game-state :state)))
    (update-game))
  (draw))

(comment 
 (def handle-commands (ev/call command-listener)) 
 (ev/cancel handle-commands "Canceled") 
 (ev/give event-chan "Test3"))  

(defn main
  [& args]
  (def repl-server
    (netrepl/server "127.0.0.1" "9365" (fiber/getenv (fiber/current))))
  (j/init-window screen-width screen-height "Snake")
  (j/set-target-fps 60)
  (init-game)
  (while (not (j/window-should-close))
    (ev/sleep 0)
    (game-loop))
  (:close repl-server))


 
