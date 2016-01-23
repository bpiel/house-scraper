(ns house-scraper.core
  (:require [clj-http.client :as client])
  (:gen-class))


;; Calculating the distance in kilometers between two points on Earth
(def earth-radius 3959)

(defn degrees->radians [point]
  (mapv #(Math/toRadians %) point))

(defn distance-between
  "Calculate the distance in km between two points on Earth. Each
   point is a pair of degrees latitude and longitude, in that order."
  ([p1 p2] (distance-between p1 p2 earth-radius))
  ([p1 p2 radius]
     (let [[lat1 long1] (degrees->radians p1)
           [lat2 long2] (degrees->radians p2)]
       (* radius
          (Math/acos (+ (* (Math/sin lat1) (Math/sin lat2))
                        (* (Math/cos lat1)
                           (Math/cos lat2)
                           (Math/cos (- long1 long2)))))))))



(def media-rr [39.914320 -75.394905])
(def trader-joes [39.917500 -75.388664])

(defn dist-score
  [d]
  (->> (max d 0.05)
       (/ 0.05)
       float))

(defn dist-between-score
  [lat lon dest]
  (-> [lat lon]
      (distance-between dest)
      dist-score))

(defn score
  [{:keys [lat lon sqft bed bath lot-sqft stories rooms tax built-yr] :as m}]
  (* (Math/pow (dist-between-score lat lon media-rr) 4)
     (Math/pow (dist-between-score lat lon trader-joes) 2)
     (-> sqft (min 2000) (Math/pow 0.5))
     (-> sqft (min 3000) (Math/pow 0.5))
     (/ 1 (or stories 2))
     (min bed 5)
     (min bath 3)
     (Math/pow (min bath 2) 2)
     (Math/pow (->> (or stories
                        2)
                    (/ sqft)
                    (- lot-sqft)
                    (min 20000))
               2)
     (- (or rooms 8)
        bed)
     (Math/pow (/ 1 (or tax 5000)) 0.5)
     (Math/pow (/ 1
                  (- 2035
                     (min 2010
                          (or built-yr
                              1930))))
               2)))

(defn to-int [x]
  (try
    (condp #(% %2) x
      integer? x
      string? (Integer/parseInt x)
      float? (int x)
      nil)
    (catch Exception e nil)))

(defn try-to-int
  [x]
  (or (to-int x)
      (if (nil? x)
        ""
        x)))

(defn try-to-int-harder
  [x d]
  (or (to-int x)
      d))


(defn to-float [x]
  (try
    (condp #(% %2) x
      float? x
      string? (Float/parseFloat x)
      integer? (float x)
      nil)
    (catch Exception e nil)))

(defn try-to-float
  [x]
  (or (to-float x)
      (if (nil? x)
        ""
        x)))

(defn zpid->filename
  [zpid]
  (str "./resources/html/" zpid))

(defn read-file-if-exists
  [filename]
  (when (->  filename
            clojure.java.io/as-file
            .exists)
    (println "SLURPING! " filename)
    (slurp filename)))


(defn get-html-for-zpid
  [zpid]
  (println "DOWNLOADING! " zpid)
  (:body (client/get (format "http://www.zillow.com/homes/%s_zpid/" zpid))))


(defn sleepy-get-html
  [zpid]
  (-> 12000
      rand-int
      (+ 3000)
      Thread/sleep)
  (get-html-for-zpid zpid))

(defn write-html-to-file
  [zpid html]
  (spit (zpid->filename zpid)
        html)
  html)

(defn fetch-html-for-zpid
  [zpid]
  (or (-> zpid
          zpid->filename
          read-file-if-exists)
      (->> zpid
           get-html-for-zpid
           (write-html-to-file zpid))))

(defn sleepy-fetch-html
  [zpid]
  (or  (-> zpid
           zpid->filename
           read-file-if-exists)
       (->> zpid
            sleepy-get-html
            (write-html-to-file zpid))))

(defn hid-field-regex
  [name]
  (->> name
       (format  "name=\"%s\".*?value=\"(.*?)\"")
       re-pattern))

(defn get-value-for-name
  [name html]
  (->> html
       (re-find (hid-field-regex name))
       second))

(defn mo-abbrv->num
  [mo-abbrv]
  (->> mo-abbrv
       (.indexOf ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
       inc))

#_ (def hh (fetch-html-for-zpid 9399481))
#_ (def hh (fetch-html-for-zpid 9398135))
#_ (def hh (fetch-html-for-zpid 9419713))
#_ (def hh (fetch-html-for-zpid 9421398))
#_ (def hh (fetch-html-for-zpid 9421396))
#_ (def hh (fetch-html-for-zpid 9399518))
#_ (def hh (fetch-html-for-zpid 9398387))

#_ (parse-html hh)

(defn parse-html
  [html]
  (try
    (let [[_ mo yr price] (re-find #"Last sold: (.*?) (\d+) for \$(.*?)<"
                                   html)
          mo' (mo-abbrv->num mo)
          sold-date (if yr
                      (format "%s/01/%s" mo' yr)
                      "")
          price' (or (some-> price
                             (clojure.string/replace "," ""))
                     "")]

      {:id (-> html meta :id)
       :address (some->> html
                         (re-find (hid-field-regex "saddr"))
                         second)
       :estimate (some->> html
                          (re-find #"zestimate\":\"(.*?)\"")
                          second
                          try-to-int)
       :bed (->> html
                 (get-value-for-name "bed")
                 try-to-float)
       :bath (->> html
                  (get-value-for-name "bath")
                  try-to-float)
       :lat (some->> html
                     (re-find #"data-latitude=\"(.*?)\"")
                     second
                     try-to-float)
       :lon (some->> html
                     (re-find #"data-longitude=\"(.*?)\"")
                     second
                     try-to-float)
       :sqft (some->> html
                      (re-find #"sqft\":\"(.*?)\"")
                      second
                      try-to-int)
       :built-yr (some->> html
                          (re-find #"Built in (.*?)<")
                          second
                          try-to-int)
       :sold-date sold-date
       :sold-price (try-to-int-harder price' 0)
       :lot-sqft (some->> html
                          (re-find #"a lot of (.*?) sqft")
                          second
                          (#(clojure.string/replace % "," ""))
                          try-to-int)
       :lot-width (some->> html
                           (re-find #"Lot width: (.*?) ")
                           second
                           try-to-int)
       :lot-depth (some->> html
                           (re-find #"Lot depth: (.*?) ")
                           second
                           try-to-int)
       :rooms (some->> html
                       (re-find #"Room count: (.*?)<")
                       second
                       try-to-int)
       :stories (some->> html
                         (re-find #"Stories: (.*?)<")
                         second
                         try-to-int)
       :tax (some->> html
                     (re-find #"<span class=\"vendor-cost\"><strong>\$(.*?)<")
                     second
                     (#(clojure.string/replace % "," ""))
                     try-to-int)
       :ac (not (nil? (or (re-find #"[Cc]entral [Aa]ir" html)
                          (re-find #"Cooling: Central" html))))})

    (catch Exception e
      (clojure.pprint/pprint e)
      (println "vvvvvvvvvvvv")
      (println html)
      (println "^^^^^^^^^^^^"))))

(defn abc
  [p d v]
  (if (p v)
    d
    v))

(defn mk-sort-key
  [m]
  (let [parts (->> m
                   :address
                   (re-find #"(\d*)\s+([NSEW]\s+)?(.*)"))]
    (str (nth parts 3) " "
         (nth parts 2)
         (apply str (repeat (- 4 (-> parts second count)) "0"))
         (second parts))))

(defn process
  [m]
  (let [lat (:lat m)
        lon (:lon m)
        d-b (fn [dest] (distance-between [lat lon] dest))
        dist-rr (d-b media-rr)
        dist-tj (d-b trader-joes)
        ]
    (assoc m
           :url (format "http://www.zillow.com/homedetails/%s_zpid/" (:id m))
           :dist-rr dist-rr
           :dist-tj dist-tj
           :sort-key (mk-sort-key m)
           :yard-est (try (->> (:stories m)
                               (#(or % 2))
                               (abc #{0} 2)
                               (/ (:sqft m))
                               (- (:lot-sqft m))
                               float)
                          (catch Exception ex
                            nil)))))

(defn find-links
  [s]
  (->> s
       (clojure.string/split-lines)
       (filter #(.startsWith % "http://"))))

(defn link->zpid
  [s]
  (->> s
       (re-find #".*?/(\d+)_zpid.*")
       second))

(defn links->zpid
  [s]
  (->> s
       find-links
       (map link->zpid)))

(defn ordered-map-entries
  [m ks]
  (concat (keep #(when (contains? m %)
                   [% (get m %)])
                ks)
          (-> (apply dissoc m ks)
              seq)))

#_ (defn map->csv-headers
  [m & ks]
  (let [[m1 m2] (split-map m ks)]
    (->> [m1 m2]
         (map keys)
         (apply concat)
         (map name)
         (clojure.string/join ","))))

#_ (map->csv-headers {:a 1 :b 2 :c 3 :d 4} :d)

#_ (ordered-map-entries {:a 1 :b 2 :c 3 :d 4} [])

(defn map->csv
  [m & ks]
  (->> ks
       (ordered-map-entries m)
       (map second)
       (clojure.string/join ",")))

#_ (map->csv {:a 1 :b 2 :c 3 :d 4} :d)

(defn map-vec->csv
  [houses & ks]
  (->> houses
       (map #(apply map->csv
                    %
                    ks))
       vec
       (cons (->> houses
                  first
                  (#(ordered-map-entries % ks))
                  (map first)
                  (map name)
                  (clojure.string/join ",")))
       vec
       (clojure.string/join "\n")))

(defn write-csv
  [houses fname & ks]
  (spit fname
        (apply map-vec->csv houses ks)))

(defn write-csv-ordered
  [houses fname]
  (write-csv houses fname :address :url :sold-price :sold-date :sqft :lot-sqft :built-yr :bed :bath :rooms :ac :dist-rr :dist-tj))


     (def exclude-ids #{9398720 9419873 9398179 9398203 9419595 9419713 9399381 9399504})

     (def b1-ids #{9398001 9398004 9397996 9398079 9398076 9398172 9398224 9399564 9398388 9398391 9398370 9419594 9398492 9398444 9398491 9398658 9419872 9419852 9398695 9398698 9398718 9398699 9398719 9398797 9399262 9399256 9399222 9399221 9399517 9399482 9399471 9399503 9399534 9399552 9399562 9399558 9399565})

#_ (do

     (def data
       (->> sss
            links->zpid
            (filter identity)
            (map #(-> %
                      sleepy-fetch-html
                      parse-html
                      (assoc :id (to-int %))
                      process))
            (remove #(-> % :id #{9398720 9419873}))
            distinct
            vec))

     (def next
       (->> data
            (filter #(-> % :id exclude-ids not))
            (filter #(-> % :id b1-ids not))
            (filter (fn [m]
                        (and
                         (>= (:bath m)
                             2)
#_                         (>= (:bed m)
                             3)
#_                         (>= (:sqft m)
                             1800)
#_                         (>= (:lot-sqft m)
                             4500)
#_                         (<= (:dist-rr m)
                             0.7)
#_                         (<= (:dist-tj m)
                             0.75)
#_                         (<= (:sold-price m)
                             650000)
#_                         (<= (or (:tax m) 0)
                             8000))))
            (map #(assoc % :batch "1.0"))))

     #_ (def next
          (->> data
               (filter #(-> % :id exclude-ids not))
               (filter #(-> % :id b1-ids not))
               (filter (fn [m]
                         (and
                          (>= (:bath m)
                              2)
                          (>= (:bed m)
                              3)
                          (>= (:sqft m)
                              1800)
                          (>= (:lot-sqft m)
                              4500)
                          (<= (:dist-rr m)
                              0.7)
                          (<= (:dist-tj m)
                              0.75)
                          (<= (:sold-price m)
                              650000)
                          (<= (or (:tax m) 0)
                              8000))))


               #_ (do
                    (map map->csv)
                    vec
                    (cons (->> data first keys (mapv name) (clojure.string/join ",")))
                    vec
                    (clojure.string/join "\n")
                    (spit "/home/bill/temp.csv"))))


     (def new1
       (->> data
            (filter #(-> % :id #{9398720} not))
            (filter (fn [m]
                      (and
                       (>= (:bath m)
                           2)
                       (>= (:bed m)
                           3)
                       (>= (:sqft m)
                           1900)
                       (>= (:lot-sqft m)
                           5000)
                       (<= (:dist-rr m)
                           0.65)
                       (<= (:dist-tj m)
                           0.7)
                       (<= (:sold-price m)
                           650000)
                       (>= (or (:tax m) 0)
                           8000))))
            #_ (do
                 (map map->csv)
                 vec
                 (cons (->> data first keys (mapv name) (clojure.string/join ",")))
                 vec
                 (clojure.string/join "\n")
                 println
                 #_          (spit "/home/bill/temp1.csv"))))
     (do
       (->> data
            (filter #(-> % :id #{9398720 9419873} not))
            (filter (fn [m]
                      (and
                       (>= (:bath m)
                           2)
                       (>= (:bed m)
                           3)
                       (>= (:sqft m)
                           1900)
                       (>= (:lot-sqft m)
                           5000)
                       (<= (:dist-rr m)
                           0.65)
                       (<= (:dist-tj m)
                           0.7)
                       (<= (:sold-price m)
                           650000)
                       (<= (or (:tax m) 0)
                           8000))))
            count
            println)
       (println))


     (do
       (->> data
            (filter #(-> % :id #{9398720 9419873} not))
            (filter (fn [m]
                      (and

                       (>= (or (:tax m) 0)
                           8000)

                       )))

            clojure.pprint/pprint)
       (println))

     (do
       (->> data

            (filter (fn [m]
                      (or

                       (= (or (:sqft m) 0.0)
                          0.0)
                       (= (or (:bed m) 0.0)
                          0.0)
                       (= (or (:bath m) 0.0)
                          0.0)
                       )))

            clojure.pprint/pprint)
       (println))



     (comment))


#_ (clojure.pprint/pprint (sort-by :score data))

#_ (clojure.pprint/pprint (sort-by :yard-est data))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(def sss "3rd st

http://www.zillow.com/homes/9419713_zpid/3-_beds/249948-650026_price/934-2429_mp/39.923242,-75.395525,39.919848,-75.401211_rect/17_zm/

http://www.zillow.com/homes/9421398_zpid/3-_beds/249948-650026_price/934-2429_mp/39.923242,-75.395525,39.919848,-75.401211_rect/17_zm/

http://www.zillow.com/homes/9421396_zpid/3-_beds/249948-650026_price/934-2429_mp/39.923242,-75.395525,39.919848,-75.401211_rect/17_zm/

http://www.zillow.com/homes/9399504_zpid/3-_beds/249948-650026_price/934-2429_mp/39.923007,-75.392687,39.919613,-75.398373_rect/17_zm/

http://www.zillow.com/homes/for_sale/9399503_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922588,-75.390321,39.919193,-75.396008_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399499_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922585,-75.390322,39.919191,-75.396008_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399498_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922585,-75.390322,39.919191,-75.396008_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398534_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922585,-75.390322,39.919191,-75.396008_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399530_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922585,-75.390322,39.919191,-75.396008_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399528_zpid/3-_beds/249948-650026_price/932-2423_mp/39.922585,-75.390322,39.919191,-75.396008_rect/17_zm/1_fr/

this one was weird  two units?
http://www.zillow.com/homes/for_sale/9399518_zpid/3-_beds/249948-650026_price/932-2423_mp/39.92097,-75.384604,39.919273,-75.387447_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399517_zpid/3-_beds/249948-650026_price/932-2423_mp/39.92097,-75.384604,39.919273,-75.387447_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399516_zpid/3-_beds/249948-650026_price/932-2423_mp/39.92097,-75.384604,39.919273,-75.387447_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399482_zpid/3-_beds/249948-650026_price/932-2423_mp/39.92097,-75.384604,39.919273,-75.387447_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399481_zpid/3-_beds/249948-650026_price/932-2423_mp/39.92097,-75.384604,39.919273,-75.387447_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399480_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920709,-75.38285,39.919011,-75.385693_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399471_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920709,-75.38285,39.919011,-75.385693_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/87893707_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920643,-75.383062,39.918946,-75.385905_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399515_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920641,-75.383062,39.918944,-75.385906_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399513_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920641,-75.383062,39.918944,-75.385906_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399510_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920641,-75.383062,39.918944,-75.385906_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399509_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920639,-75.381919,39.918941,-75.384762_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399508_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920639,-75.381919,39.918941,-75.384762_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399507_zpid/3-_beds/249948-650026_price/932-2423_mp/39.920783,-75.381541,39.919085,-75.384384_rect/18_zm/1_fr/

4th st

http://www.zillow.com/homes/for_sale/9398136_zpid/3-_beds/1_open/39.922624,-75.389623,39.920927,-75.392466_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398133_zpid/3-_beds/1_open/39.922624,-75.389623,39.920927,-75.392466_rect/18_zm/

5 East 4th St., Media, PA 19063

http://www.zillow.com/homes/for_sale/9398698_zpid/3-_beds/1_open/39.922095,-75.384435,39.920398,-75.387278_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398079_zpid/3-_beds/1_open/39.922095,-75.384435,39.920398,-75.387278_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398077_zpid/3-_beds/1_open/39.922095,-75.384435,39.920398,-75.387278_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398076_zpid/3-_beds/1_open/39.922095,-75.384435,39.920398,-75.387278_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398104_zpid/3-_beds/1_open/39.921815,-75.382525,39.920118,-75.385369_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398101_zpid/3-_beds/1_open/39.921815,-75.382525,39.920118,-75.385369_rect/18_zm/

5th st
http://www.zillow.com/homes/for_sale/9398051_zpid/3-_beds/249948-650026_price/930-2420_mp/39.92365,-75.391291,39.921953,-75.394134_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398050_zpid/3-_beds/249948-650026_price/930-2420_mp/39.92365,-75.391291,39.921953,-75.394134_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398049_zpid/3-_beds/249948-650026_price/930-2420_mp/39.92365,-75.391291,39.921953,-75.394134_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398933_zpid/3-_beds/249948-650026_price/930-2420_mp/39.92365,-75.391291,39.921953,-75.394134_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398934_zpid/3-_beds/249948-650026_price/930-2420_mp/39.924066,-75.391291,39.922369,-75.394134_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398391_zpid/3-_beds/249948-650026_price/930-2420_mp/39.923749,-75.387675,39.922052,-75.390518_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398056_zpid/3-_beds/249948-650026_price/930-2420_mp/39.92323,-75.385996,39.921533,-75.388839_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398030_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922823,-75.384054,39.921126,-75.386897_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398035_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922823,-75.384054,39.921126,-75.386897_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398055_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922823,-75.384054,39.921126,-75.386897_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398054_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922823,-75.384054,39.921126,-75.386897_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398053_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922788,-75.382783,39.921091,-75.385626_rect/18_zm/1_fr/
has a pool

http://www.zillow.com/homes/for_sale/9398052_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922788,-75.382783,39.921091,-75.385626_rect/18_zm/1_fr/
has a pool

http://www.zillow.com/homes/for_sale/9398027_zpid/3-_beds/249948-650026_price/930-2420_mp/39.922788,-75.382783,39.921091,-75.385626_rect/18_zm/1_fr/

2nd

this house looks good
http://www.zillow.com/homedetails/321-W-Second-St-Media-PA-19063/2131962370_zpid/

looks good
http://www.zillow.com/homes/for_sale/9399262_zpid/3-_beds/249948-650026_price/930-2420_mp/39.920207,-75.385001,39.918509,-75.387844_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399256_zpid/3-_beds/249948-650026_price/930-2420_mp/39.919709,-75.3827,39.918012,-75.385543_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399221_zpid/3-_beds/249948-650026_price/930-2420_mp/39.919707,-75.3827,39.91801,-75.385543_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399222_zpid/3-_beds/249948-650026_price/930-2420_mp/39.919707,-75.3827,39.91801,-75.385543_rect/18_zm/1_fr/

front
http://www.zillow.com/homes/for_sale/9399564_zpid/3-_beds/249948-650026_price/925-2405_mp/39.920719,-75.395019,39.919022,-75.397862_rect/18_zm/1_fr/


http://www.zillow.com/homes/for_sale/9399563_zpid/3-_beds/249948-650026_price/925-2405_mp/39.920719,-75.395019,39.919022,-75.397862_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9399552_zpid/3-_beds/249948-650026_price/925-2405_mp/39.920569,-75.393807,39.918872,-75.39665_rect/18_zm/1_fr/

no yard?
http://www.zillow.com/homes/for_sale/9398224_zpid/3-_beds/249948-650026_price/925-2405_mp/39.920569,-75.393807,39.918872,-75.39665_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/89456038_zpid/3-_beds/249948-650026_price/925-2405_mp/39.920567,-75.393807,39.91887,-75.39665_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398203_zpid/3-_beds/249948-650026_price/925-2405_mp/39.919421,-75.386995,39.917724,-75.389838_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398202_zpid/3-_beds/249948-650026_price/925-2405_mp/39.919421,-75.386995,39.917724,-75.389838_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398250_zpid/3-_beds/249948-650026_price/925-2405_mp/39.918913,-75.38263,39.917216,-75.385473_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398235_zpid/3-_beds/249948-650026_price/925-2405_mp/39.919024,-75.382075,39.917327,-75.384918_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/66899989_zpid/3-_beds/249948-650026_price/925-2405_mp/39.919024,-75.382075,39.917327,-75.384918_rect/18_zm/1_fr/

front

http://www.zillow.com/homes/for_sale/9399459_zpid/3-_beds/249948-650026_price/925-2405_mp/39.919624,-75.394418,39.917927,-75.397262_rect/18_zm/1_fr/


Franklin

http://www.zillow.com/homes/for_sale/9398178_zpid/3-_beds/249948-650026_price/926-2409_mp/39.91572,-75.380015,39.914113,-75.382858_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398179_zpid/3-_beds/249948-650026_price/926-2409_mp/39.91572,-75.380015,39.914113,-75.382858_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398168_zpid/3-_beds/249948-650026_price/926-2408_mp/39.917,-75.388335,39.915393,-75.391178_rect/18_zm/1_fr/

awesome
http://www.zillow.com/homes/for_sale/9398169_zpid/3-_beds/249948-650026_price/926-2408_mp/39.917,-75.388335,39.915393,-75.391178_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398170_zpid/3-_beds/249948-650026_price/926-2408_mp/39.917206,-75.389618,39.915599,-75.392461_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398172_zpid/3-_beds/249948-650026_price/926-2408_mp/39.917207,-75.390341,39.915601,-75.393185_rect/18_zm/1_fr/


Jefferson

http://www.zillow.com/homes/for_sale/9398445_zpid/3-_beds/249948-650026_price/924-2404_mp/39.917237,-75.388798,39.914023,-75.394485_rect/17_zm/1_fr/
likely a very small yard

http://www.zillow.com/homes/for_sale/9398444_zpid/3-_beds/249948-650026_price/924-2404_mp/39.916799,-75.385858,39.913586,-75.391544_rect/17_zm/1_fr/

for sale asking 875
http://www.zillow.com/homes/for_sale/9398443_zpid/3-_beds/249948-650026_price/924-2404_mp/39.9168,-75.385859,39.913587,-75.391545_rect/17_zm/1_fr/

zillow says it’s 2 units but I don’t think it is
http://www.zillow.com/b/24-E-Jefferson-St-Media-PA/39.914991,-75.389096_ll/

http://www.zillow.com/homes/for_sale/9398435_zpid/3-_beds/249948-650026_price/924-2404_mp/39.9168,-75.385859,39.913587,-75.391545_rect/17_zm/1_fr/

5 stories, not ideal
http://www.zillow.com/homes/for_sale/9398434_zpid/3-_beds/249948-650026_price/924-2404_mp/39.9168,-75.385859,39.913587,-75.391545_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398492_zpid/3-_beds/249948-650026_price/924-2404_mp/39.9168,-75.385859,39.913587,-75.391545_rect/17_zm/1_fr/

this one is on our comp list
http://www.zillow.com/homes/for_sale/9398491_zpid/3-_beds/249948-650026_price/924-2404_mp/39.9168,-75.385859,39.913587,-75.391545_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398433_zpid/3-_beds/249948-650026_price/924-2404_mp/39.916704,-75.383444,39.913491,-75.38913_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398419_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915968,-75.379689,39.912754,-75.385375_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398416_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915968,-75.379689,39.912754,-75.385375_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398415_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915968,-75.379689,39.912754,-75.385375_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398478_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915076,-75.38028,39.913469,-75.383124_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398477_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915076,-75.38028,39.913469,-75.383124_rect/18_zm/1_fr/

Lincoln
http://www.zillow.com/homes/for_sale/3-_beds/249948-650026_price/924-2404_mp/39.914471,-75.39128,39.912864,-75.394123_rect/18_zm/1_fr/

how did we miss this one?  just sold this summer!!!
http://www.zillow.com/homes/for_sale/9419852_zpid/3-_beds/249948-650026_price/924-2404_mp/39.914471,-75.39128,39.912864,-75.394123_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9419873_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915462,-75.382876,39.912248,-75.388562_rect/17_zm/1_fr/
could be awesome but no side walk - ok though?

awesome if central air
http://www.zillow.com/homes/for_sale/9419872_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915462,-75.382876,39.912248,-75.388562_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398658_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915462,-75.382876,39.912248,-75.388562_rect/17_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398656_zpid/3-_beds/249948-650026_price/924-2404_mp/39.915459,-75.382876,39.912246,-75.388562_rect/17_zm/1_fr/


edgemont

http://www.zillow.com/homes/for_sale/9397995_zpid/3-_beds/249948-650026_price/924-2403_mp/39.922194,-75.383298,39.920587,-75.386141_rect/18_zm/

http://www.zillow.com/homes/for_sale/64693119_zpid/3-_beds/249948-650026_price/924-2403_mp/39.921293,-75.383481,39.919687,-75.386324_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398005_zpid/3-_beds/249948-650026_price/924-2403_mp/39.921293,-75.383481,39.919687,-75.386324_rect/18_zm/

http://www.zillow.com/homes/for_sale/119187862_zpid/3-_beds/249948-650026_price/924-2403_mp/39.921291,-75.383481,39.919685,-75.386324_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399516_zpid/3-_beds/249948-650026_price/924-2403_mp/39.921291,-75.383481,39.919685,-75.386324_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398004_zpid/3-_beds/249948-650026_price/924-2403_mp/39.920414,-75.383695,39.918808,-75.386538_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398003_zpid/3-_beds/249948-650026_price/924-2403_mp/39.920414,-75.383695,39.918808,-75.386538_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398001_zpid/3-_beds/249948-650026_price/924-2403_mp/39.920414,-75.383695,39.918808,-75.386538_rect/18_zm/

http://www.zillow.com/homes/for_sale/9397989_zpid/3-_beds/249948-650026_price/924-2403_mp/39.920414,-75.383695,39.918808,-75.386538_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398000_zpid/3-_beds/249948-650026_price/924-2403_mp/39.91953,-75.38399,39.917924,-75.386834_rect/18_zm/

http://www.zillow.com/homes/for_sale/9397996_zpid/3-_beds/249948-650026_price/924-2403_mp/39.915212,-75.384902,39.913605,-75.387745_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398659_zpid/3-_beds/249948-650026_price/924-2403_mp/39.915212,-75.384902,39.913605,-75.387745_rect/18_zm/

Vernon

http://www.zillow.com/homes/for_sale/9399534_zpid/3-_beds/249948-650026_price/924-2403_mp/39.914512,-75.381321,39.912906,-75.384164_rect/18_zm/

Gayle
http://www.zillow.com/homes/for_sale/9398284_zpid/3-_beds/249948-650026_price/923-2400_mp/39.916419,-75.385731,39.914722,-75.388574_rect/18_zm/1_fr/

Monroe

out of our price range
http://www.zillow.com/homes/for_sale/9398720_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922905,-75.385562,39.921208,-75.388405_rect/18_zm/1_fr/

looks awesome if central air
http://www.zillow.com/homes/for_sale/9398719_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922905,-75.385562,39.921208,-75.388405_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398718_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922905,-75.385562,39.921208,-75.388405_rect/18_zm/1_fr/


http://www.zillow.com/homes/for_sale/9398699_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922905,-75.385562,39.921208,-75.388405_rect/18_zm/1_fr/

4?? N monroe - looks good but no listing

http://www.zillow.com/homes/for_sale/9398698_zpid/3-_beds/249948-650026_price/923-2401_mp/39.921684,-75.385613,39.919986,-75.388456_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398696_zpid/3-_beds/249948-650026_price/923-2401_mp/39.921684,-75.385613,39.919986,-75.388456_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398695_zpid/3-_beds/249948-650026_price/923-2401_mp/39.921684,-75.385613,39.919986,-75.388456_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398688_zpid/3-_beds/249948-650026_price/923-2401_mp/39.916278,-75.386716,39.914581,-75.389559_rect/18_zm/1_fr/

church st

http://www.zillow.com/homes/for_sale/9397976_zpid/3-_beds/249948-650026_price/923-2401_mp/39.916481,-75.387603,39.914784,-75.390446_rect/18_zm/1_fr/

Jackson

http://www.zillow.com/homes/for_sale/9398391_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922712,-75.38712,39.921015,-75.389963_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398388_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922712,-75.38712,39.921015,-75.389963_rect/18_zm/1_fr/

might be apartments??
http://www.zillow.com/homes/for_sale/9398387_zpid/3-_beds/249948-650026_price/923-2401_mp/39.922712,-75.38712,39.921015,-75.389963_rect/18_zm/1_fr/

zillow and google maps don’t agree about the houses on this block so it is confusing
http://www.zillow.com/homes/for_sale/9398378_zpid/3-_beds/249948-650026_price/923-2401_mp/39.916489,-75.388839,39.914792,-75.391683_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398374_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915708,-75.388891,39.914011,-75.391734_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398371_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915708,-75.388891,39.914011,-75.391734_rect/18_zm/1_fr/

this is the house we want
http://www.zillow.com/homes/for_sale/9398370_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915708,-75.388891,39.914011,-75.391734_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398369_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915169,-75.389317,39.913471,-75.39216_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9419594_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915169,-75.389317,39.913471,-75.39216_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9419595_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915169,-75.389317,39.913471,-75.39216_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398335_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915169,-75.389317,39.913471,-75.39216_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398336_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915831,-75.389003,39.914134,-75.391846_rect/18_zm/1_fr/

http://www.zillow.com/homes/for_sale/9398337_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915831,-75.389003,39.914134,-75.391846_rect/18_zm/1_fr/

plum

http://www.zillow.com/homes/for_sale/9398963_zpid/3-_beds/249948-650026_price/921-2395_mp/39.916771,-75.389271,39.915074,-75.392114_rect/18_zm/

olive

http://www.zillow.com/homes/for_sale/9398862_zpid/3-_beds/249948-650026_price/921-2395_mp/39.923109,-75.389024,39.921412,-75.391868_rect/18_zm/

http://www.zillow.com/homes/for_sale/87894757_zpid/3-_beds/249948-650026_price/921-2395_mp/39.923108,-75.389025,39.921411,-75.391868_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398798_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915887,-75.390172,39.914189,-75.393016_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398797_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915887,-75.390172,39.914189,-75.393016_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398848_zpid/3-_beds/249948-650026_price/921-2395_mp/39.916045,-75.390494,39.914348,-75.393337_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398848_zpid/3-_beds/249948-650026_price/921-2395_mp/39.916045,-75.390494,39.914348,-75.393337_rect/18_zm/

south
check out 400south block of south ave - homes might have been bought and made into twins - check addresses

http://www.zillow.com/homes/for_sale/9399382_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915897,-75.391385,39.9142,-75.394228_rect/18_zm/

http://www.zillow.com/homes/for_sale/121149621_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915897,-75.391385,39.9142,-75.394228_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399381_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915895,-75.391385,39.914198,-75.394228_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399368_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915895,-75.391385,39.914198,-75.394228_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399367_zpid/3-_beds/249948-650026_price/921-2395_mp/39.915895,-75.391385,39.914198,-75.394228_rect/18_zm/

broomall

http://www.zillow.com/homes/for_sale/9398067_zpid/3-_beds/249948-650026_price/921-2395_mp/39.924146,-75.389515,39.922449,-75.392358_rect/18_zm/

http://www.zillow.com/homes/for_sale/87894743_zpid/3-_beds/249948-650026_price/921-2395_mp/39.924146,-75.389515,39.922449,-75.392358_rect/18_zm/

orange

http://www.zillow.com/homes/for_sale/87894687_zpid/3-_beds/249948-650026_price/921-2395_mp/39.923154,-75.390642,39.921457,-75.393485_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398902_zpid/3-_beds/249948-650026_price/921-2395_mp/39.923154,-75.390642,39.921457,-75.393485_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398901_zpid/3-_beds/249948-650026_price/921-2395_mp/39.923154,-75.390642,39.921457,-75.393485_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398930_zpid/3-_beds/249948-650026_price/921-2395_mp/39.922253,-75.390803,39.920556,-75.393646_rect/18_zm/

lemon

http://www.zillow.com/homes/for_sale/9398544_zpid/3-_beds/249948-650026_price/921-2396_mp/39.925431,-75.39198,39.923734,-75.394823_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398543_zpid/3-_beds/249948-650026_price/921-2396_mp/39.925431,-75.39198,39.923734,-75.394823_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398542_zpid/3-_beds/249948-650026_price/921-2396_mp/39.924642,-75.39202,39.922945,-75.394864_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398539_zpid/3-_beds/249948-650026_price/921-2396_mp/39.924642,-75.39202,39.922945,-75.394864_rect/18_zm/

http://www.zillow.com/homes/for_sale/9398550_zpid/3-_beds/249948-650026_price/921-2396_mp/39.92085,-75.392994,39.919153,-75.395837_rect/18_zm/

west

http://www.zillow.com/homes/for_sale/9399565_zpid/3-_beds/249948-650026_price/921-2396_mp/39.92177,-75.394271,39.920073,-75.397114_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399506_zpid/3-_beds/249948-650026_price/921-2396_mp/39.92177,-75.394271,39.920073,-75.397114_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399560_zpid/3-_beds/249948-650026_price/921-2396_mp/39.92177,-75.394271,39.920073,-75.397114_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399558_zpid/3-_beds/249948-650026_price/921-2396_mp/39.92177,-75.394271,39.920073,-75.397114_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399557_zpid/3-_beds/249948-650026_price/921-2396_mp/39.921243,-75.394244,39.919546,-75.397087_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399552_zpid/3-_beds/249948-650026_price/921-2396_mp/39.920367,-75.394416,39.91867,-75.397259_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399563_zpid/3-_beds/249948-650026_price/921-2396_mp/39.920367,-75.394416,39.91867,-75.397259_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399562_zpid/3-_beds/249948-650026_price/921-2396_mp/39.920367,-75.394416,39.91867,-75.397259_rect/18_zm/

Parks Edge

http://www.zillow.com/homes/for_sale/54670837_zpid/3-_beds/249948-650026_price/922-2397_mp/39.921363,-75.395212,39.919666,-75.398055_rect/18_zm/

http://www.zillow.com/homes/for_sale/54569659_zpid/3-_beds/249948-650026_price/922-2397_mp/39.921363,-75.395212,39.919666,-75.398055_rect/18_zm/

http://www.zillow.com/homes/for_sale/53615987_zpid/3-_beds/249948-650026_price/922-2397_mp/39.921363,-75.395212,39.919666,-75.398055_rect/18_zm/

http://www.zillow.com/homes/for_sale/54670838_zpid/3-_beds/249948-650026_price/922-2397_mp/39.921363,-75.395212,39.919666,-75.398055_rect/18_zm/

http://www.zillow.com/homes/for_sale/53615986_zpid/3-_beds/249948-650026_price/922-2397_mp/39.921359,-75.395213,39.919662,-75.398056_rect/18_zm/

http://www.zillow.com/homes/for_sale/9399561_zpid/

")
