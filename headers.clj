(ns headers
  (use [clj-file-utils.core]
       [clojure.contrib.duck-streams :only (read-lines)])
  (import [org.apache.commons.io FileUtils]
	  [java.io File BufferedWriter FileWriter]))

(defn find-all-java-files [dir]
  (FileUtils/listFiles (File. dir) (into-array ["java"]) true))

(defn package-line? [line]
  (.startsWith line "package"))

(defn emit-with-new-header [body]
  (let [header
	["/*******************************************************************************"
	 " * Copyright (c) 2008-2011 Chair for Applied Software Engineering,"
         " * Technische Universitaet Muenchen."
	 " * All rights reserved. This program and the accompanying materials"
	 " * are made available under the terms of the Eclipse Public License v1.0"
	 " * which accompanies this distribution, and is available at"
	 " * http://www.eclipse.org/legal/epl-v10.html"
	 " * "
	 " * Contributors:"
	 " ******************************************************************************/"]]
    (concat header body)))

(defn write-lines2 [file-name lines]
  (with-open [wtr (BufferedWriter. (FileWriter. file-name))]
    (doseq [line lines]
       (.write wtr line)
       (.newLine wtr))))

(defn process-file [file]
  (let [body (drop-while (complement package-line?) (read-lines file))
	org-file (.getAbsolutePath file)
	tmp-file (str org-file ".tmp")]
     (write-lines2 tmp-file (emit-with-new-header body))
     (rm org-file)
     (mv tmp-file org-file)))

(defn change-headers [dir]
  (doseq [file (find-all-java-files dir)]
    (process-file file)))



    
  