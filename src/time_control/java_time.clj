(ns time-control.java-time
  (:refer-clojure :exclude [format])
  (:import [java.util Date]
           [java.time LocalDate LocalDateTime Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(set! *warn-on-reflection* true)

(defn format [format-str ^LocalDate date]
  (.format (DateTimeFormatter/ofPattern format-str) date))


(defn local-date ^LocalDate
  ([] (LocalDate/now))
  ([^Date date]
   (LocalDate/ofInstant (.toInstant date) (ZoneId/systemDefault)))
  ([format-str date-str]
   (LocalDate/parse date-str (DateTimeFormatter/ofPattern format-str))))


(defn before? [^LocalDate a ^LocalDate b]
  (.isBefore a b))


(defn after? [^LocalDate a ^LocalDate b]
  (.isAfter a b))


(defn local-date-time ^LocalDate
  ([]
   (LocalDateTime/now))
  ([^Date date]
   (LocalDateTime/ofInstant (.toInstant date) (ZoneId/systemDefault)))
  ([format-str date-str]
   (LocalDateTime/parse date-str (DateTimeFormatter/ofPattern format-str))))


(defn now ^Date []
  (Date/from (Instant/now)))
