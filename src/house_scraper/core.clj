(ns house-scraper.core
  (:require [clj-http.client :as client])
  (:gen-class))

(defn zpid->filename
  [zpid]
  (str "./resources/html/" zpid))

(defn read-file-if-exists
  [filename]
  (when (->  filename
            clojure.java.io/as-file
            .exists)
    (println "SLURPING!")
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

(parse-html hh)

(defn parse-html
  [html]
  (try
    (let [[_ mo yr price] (re-find #"Last sold: (.*?) (\d+) for \$(.*?)<"
                                   html)
          mo' (mo-abbrv->num mo)
          sold-date (format "%s/01/%s" mo' yr)
          price' (-> price
                     (clojure.string/replace "," ""))]
      [mo' yr price']

      {:address (some->> html
                         (re-find (hid-field-regex "saddr"))
                         second)
       :estimate (some->> html
                          (re-find #"zestimate\":\"(.*?)\"")
                          second)
       :bed (get-value-for-name "bed" html)
       :bath (get-value-for-name "bath" html)
       :lat (some->> html
                     (re-find #"data-latitude=\"(.*?)\"")
                     second)
       :lon (some->> html
                     (re-find #"data-longitude=\"(.*?)\"")
                     second)
       :sqft (some->> html
                      (re-find #"sqft\":\"(.*?)\"")
                      second)
       :built-yr (some->> html
                          (re-find #"Built in (.*?)<")
                          second)
       :sold-date sold-date
       :sold-price price'
       :lot-sqft (some->> html
                          (re-find #"a lot of (.*?) sqft")
                          second
                          (#(clojure.string/replace % "," "")))
       :lot-width (some->> html
                           (re-find #"Lot width: (.*?) ")
                           second)
       :lot-depth (some->> html
                           (re-find #"Lot depth: (.*?) ")
                           second)
       :rooms (some->> html
                       (re-find #"Room count: (.*?)<")
                       second)
       :stories (some->> html
                         (re-find #"Stories: (.*?)<")
                         second)
       :tax (some->> html
                     (re-find #"<span class=\"vendor-cost\"><strong>\$(.*?)<")
                     second
                     (#(clojure.string/replace % "," "")))})
    (catch Exception e
      (clojure.pprint/pprint e)
      (println "vvvvvvvvvvvv")
      (println html)
      (println "^^^^^^^^^^^^"))))

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

#_ (do

     (def data (mapv #(-> %
                          sleepy-fetch-html
                          parse-html
                          )
                     (links->zpid sss)))
     (clojure.pprint/pprint data))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(def sss "
3rd st

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




")