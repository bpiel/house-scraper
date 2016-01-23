(ns house-scraper.criteria)

#_ (do


     (def b1-0
       (->> data
            (filterv (fn [m]
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
            (map #(assoc % :batch "1.0"))))

     (def b1-1
       (let [remove-ids (->> b1-0 (map :id) set)]
         (->> data
              (remove #(-> % :id remove-ids))
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
              (map #(assoc % :batch "1.1")))))

     (def b1*
       (->> (concat b1-0 b1-1)
            (sort-by :sort-key))))
