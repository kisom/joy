(ns joy.rolodex)

(defn rolo-name [person-name]
  (let [[f-name m-name l-name] person-name]
    (str l-name ", " f-name " " m-name)))

(def sample-record
  {:first-name "Guy"
   :middle-name "Lewis"
   :last-name "Steele"
   :occupation "hacker elite"
   :location "MA"
})

(defn rolo-record [rec]
  (let [{f-name :first-name
         m-name :middle-name
         l-name :last-name} rec]
    (str l-name ", " f-name " " m-name)))

(defn rolo-record2 [rec]
  (let [{:keys [first-name middle-name last-name]} rec]
    (str last-name ", " first-name " " middle-name)))

(defn rolo-record3 [{:keys [first-name middle-name last-name]}]
  (str last-name ", " first-name " " middle-name))

(defn where-is [{:keys [location first-name]}]
  (println (str first-name " is in " location ".")))
q
