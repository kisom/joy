(ns joy.udp
  (:refer-clojure :exclude [get]))

(defn beget [o p] (assoc o ::prototype p))
(def put assoc)
(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(def cat {:likes-dogs true
          :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))

(defmulti  compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx  [m] (get m :c-compiler))

(def clone (partial beget {}))
(def unix {:os ::unix
           :c-compiler "cc"
           :home "/home"
           :dev "/dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :c-compiler "gcc")
             (put :home "/Users")))

(defmulti  home :os)
(defmethod home ::unix [m] (get m :home))

(derive ::osx ::unix)
(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")

(prefer-method home ::unix ::bsd)
(remove-method home ::bsd)

(defn mm-ctx []
  (derive (make-hierarchy) ::osx ::unix))

(defmulti  compile-cmd (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"] [m]
  (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))
