;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          defsystem.clj
;;;; Project:       Categories
;;;; Purpose:       system loader
;;;; Author:        mikel evins
;;;; Copyright:     Copyright 2009 by mikel evins, all rights reserved
;;;; License:       Licensed under the Apache License, version 2.0
;;;;                See the accompanying file "License" for more information
;;;;
;;;; ***********************************************************************

(in-ns 'user)

;;; ----------------------------------------------------------------------
;;; build tools
;;; ----------------------------------------------------------------------

(def *system-registry* (ref {}))
(def *loaded-files* (ref {}))

(defn make-system [name args] (merge (apply hash-map args) {:name name}))

(defn defsystem [name & args] 
  (dosync (ref-set *system-registry* (merge @*system-registry* {name (make-system name args)})))
  name)

(defn mod-date [fname] (java.util.Date. (.lastModified (java.io.File. fname))))

(defn new-file? [fname] 
  (let [last-loaded-time (get @*loaded-files* fname)]
    (if last-loaded-time
      (.after (mod-date fname) last-loaded-time)
      true)))

(defn load-system-file [fname]
  (when (new-file? fname)
    (println (str "loading file " fname "..."))
    (load-file fname)
    (dosync (ref-set *loaded-files* (merge @*loaded-files* {fname (mod-date fname)})))))

(defn load-system [sysname & [loading-systems]]
  (if (some #(= sysname %1) loading-systems)
    sysname
    (let [loading-systems (conj loading-systems sysname)
          sys (get @*system-registry* sysname)
          root (:root sys)]
      ;; load dependencies
      (loop [deps (:depends-on sys)]
        (if (empty? deps)
          nil
          (do
            (println)
            (println (str "loading system " (first deps) "..."))
            (load-system (first deps) loading-systems)
            (recur (rest deps)))))
      ;; load this system's files
      (loop [fs (:files sys)]
        (if (empty? fs)
          sysname
          (let [f (first fs)]
            (load-system-file (str root f))
            (recur (rest fs))))))))

(defn list-systems [] (sort (keys @*system-registry*)))

