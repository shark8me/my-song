(ns my-song.song1
  (:require [overtone.live :refer :all]
            [overtone.inst.piano :refer [ piano]]
            [overtone.inst.synth :refer [bass vintage-bass tb303 ]]
            [overtone.synth.stringed :refer [pick-string strum-strings guitar-chord-frets]]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :refer [triad inversion root]]
            [leipzig.temperament :as temperament]))

(def saptak ["s" "r" "g" "m" "p" "d" "n"])
(def tar-saptak ["s." "r." "g." "m." "p." "d." "n."])
(def mandra-saptak [".s" ".r" ".g" ".m" ".p" ".d" ".n"])
(def teen-saptak (conj (reduce into [mandra-saptak saptak tar-saptak]) "s.."))
(def smap (zipmap teen-saptak (iterate inc 0)))
(def revsmap 
  (let [k (keys smap) 
        v (mapv smap k)]
  (zipmap v k)))

(defn sinc 
  [note]
  (revsmap (inc (smap note))))
(defn sdec 
  [note]
  (revsmap (dec (smap note))))
(defn sfn 
  [inp]
  (vec (.split inp " ")))

(def sthayi (let [m1 (sfn ".p .n s .n s .n r s")
      m2 (into (-> m1 first sinc vector) (subvec m1 1))
      m3 (into m1 m2)
      m4 (mapv sinc m1)
      m5 (sfn "g r r s s .n .n .d .p .d .n .d .n s r g")
      m6 (into (mapv sdec (subvec m5 0 8)) (subvec m5 8))] 
  (->> (concat (repeat 2 m3) (repeat 2 m4) m6) 
       flatten vec
  (mapv smap))))

(def melody
         ; Row,  row,  row   your  boat
  (phrase [3/3   3/3   2/3   1/3   3/3 3/3 3/3]
          [  0     0     0     1     2 -1 -2]))
(piano 50)
(vintage-bass 70)
(stop)
;(strum-strings :A) 
(defmethod live/play-note :default [{hertz :pitch seconds :duration}] (piano hertz seconds))
(defmethod live/play-note :bass [{hertz :pitch seconds :duration}] (piano hertz))
(defmethod live/play-note :left-hand1 [{hertz :pitch seconds :duration}] (piano (/ hertz 2)))

(def vocal (->>  (phrase (vec (repeat (count sthayi) 1/4)) sthayi)
                (where :part (is :vocal))))

(def left-hand (->> (->> vocal (partition 4 4) (mapv first))
                    (where :part (is :left-hand1))))
(->>
  left-hand
  (tempo (bpm 50))
  (where :pitch (comp scale/C scale/major))
  live/play
  )

(def bass-part "A bass part to accompany the melody."
  (->>  
       ;(where :pitch (comp scale/low))
       (all :part :bass)))

(->>
  (phrase
  (vec (repeat 3 1)) 
  [(-> triad (root 0))
   (-> triad (inversion 1) (root 0))
   (-> triad (inversion 2) (root 0))
   ;(-> triad (inversion 2) (root 4))
   (comment (-> triad (inversion 1) (root 0))
   (-> triad (root 3))
   (-> triad (root 4))
   (-> triad (inversion 1) (root 3))
   (-> triad (inversion 1) (root 4))
   (-> triad (root 0)))
   ]) 
  ;bass-part
  ;(then (with bass melody))
  ;(then (with bass melody reply))
  ;(then (times 2 bass))
  (tempo (bpm 50))
  (where :pitch (comp scale/C scale/major))
  live/play)

(->>
  melody
  (tempo (bpm 150))
  (where :pitch (comp scale/C scale/major))
  live/play)

; Instruments
(definst bass [freq 110 volume 1.0]
  (-> (saw freq)
      (* (env-gen (perc 0.1 0.4) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.01 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))

; Composition
(def progression [0 0 3 0 4 0])

(defn bassline [root]
  (->> (phrase (cycle [1 1/2 1/2 1 1]) [0 -3 -1 0 2 0 2 3 2 0])
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))
       (where :part (is :bass))))

(defn accompaniment [root]
  (->>
    (phrase [8] [(-> chord/seventh (chord/root root))])
    (where :part (is :accompaniment))))

; Track
(def track
  (->>
    (mapthen bassline progression)
    (with (mapthen accompaniment progression))
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (where :time (bpm 90))
    (where :duration (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
)
