(defpackage :restaurant-fuzzy
  (:use :cl :csv :cl-tuples :parse-float)) ; :numcl isn't use here
(in-package :restaurant-fuzzy)

; Fungsi untuk menghitung derajat keanggotaan menggunakan fungsi segitiga
(defun triangular (x a b c)
  "
  Menghitung derajat keanggotaan untuk fungsi keanggotaan segitiga.

  Args:
      x (float): Nilai input.
      a (float): Batas bawah segitiga.
      b (float): Nilai puncak segitiga.
      c (float): Batas atas segitiga.

  Returns:
      float: Derajat keanggotaan.

  Exception:
      Zero Division: Jika selisih (b - a) dan atau (c - b) sama dengan 0, keluarkan error ini. 
      Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat menghitung derajat keanggotaan.
  "
  (handler-case
      (cond
        ((and (<= a x) (<= x b)) (/ (- x a) (- b a)))
        ((and (> b x) (<= x c)) (/ (- c x) (- c b)))
        (t 0))
    (division-by-zero (e)
      (format t "Selisih pembagi sama dengan 0, melakukan terminasi dengan mengembalikan nilai 0 (diluar jangkauan)...")
      0)
    (error (e) 
      (format t "~a" e))
    (t 
      (format t "Berhasil menghitung derajat keanggotaan!"))))

; Fungsi untuk menghitung derajat keanggotaan untuk kualitas pelayanan
(defun kualitas-pelayanan-fuzzy (pelayanan)
  "
  Menghitung derajat keanggotaan fuzzy untuk kualitas pelayanan.

  Args:
      pelayanan (float): Nilai kualitas pelayanan.

  Returns:
      tuple: Derajat keanggotaan (rendah, sedang, tinggi).
  "
  (let ((rendah (triangular pelayanan 0 20 50))
        (sedang (triangular pelayanan 20 50 80))
        (tinggi (triangular pelayanan 50 80 100)))
    (cl-tuples:make-tuple 'rendah 'sedang 'tinggi)))

; Fungsi untuk menghitung derajat keanggotaan untuk harga
(defun harga-fuzzy (harga)
  "
  Menghitung derajat keanggotaan fuzzy untuk harga.

  Args:
      harga (float): Nilai harga.

  Returns:
      tuple: Derajat keanggotaan (murah, sedang, mahal).
  "
  (let ((murah (triangular harga 25000 30000 40000))
        (sedang (triangular harga 30000 40000 50000))
        (mahal (triangular harga 40000 50000 55000)))
    (cl-tuples:make-tuple 'murah 'sedang 'mahal)))

(defparameter *fuzzy-rules-tsukamoto*
  (make-hash-table)
  "Hash table for fuzzy rules.")
(setf (gethash 1 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "RENDAH") (cons 'harga "MAHAL") (cons 'kelayakan 35)))
(setf (gethash 2 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "SEDANG") (cons 'harga "SEDANG") (cons 'kelayakan 65)))
(setf (gethash 3 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "TINGGI") (cons 'harga "MURAH") (cons 'kelayakan 95)))
(setf (gethash 4 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "RENDAH") (cons 'harga "MURAH") (cons 'kelayakan 55)))
(setf (gethash 5 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "SEDANG") (cons 'harga "MURAH") (cons 'kelayakan 75)))
(setf (gethash 6 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "TINGGI") (cons 'harga "SEDANG") (cons 'kelayakan 85)))
(setf (gethash 7 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "RENDAH") (cons 'harga "SEDANG") (cons 'kelayakan 45)))
(setf (gethash 8 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "SEDANG") (cons 'harga "MAHAL") (cons 'kelayakan 60)))
(setf (gethash 9 *fuzzy-rules-tsukamoto*) (list (cons 'pelayanan "TINGGI") (cons 'harga "MAHAL") (cons 'kelayakan 70)))

(defun inferensi-tsukamoto (kualitas-pelayanan-rendah kualitas-pelayanan-sedang kualitas-pelayanan-tinggi harga-murah harga-sedang harga-mahal)
  "
  Melakukan inferensi fuzzy menggunakan metode Tsukamoto.

  Args:
    kualitas_pelayanan_rendah (float): Derajat keanggotaan kualitas pelayanan rendah.
    kualitas_pelayanan_sedang (float): Derajat keanggotaan kualitas pelayanan sedang.
    kualitas_pelayanan_tinggi (float): Derajat keanggotaan kualitas pelayanan tinggi.
    harga_murah (float): Derajat keanggotaan harga murah.
    harga_sedang (float): Derajat keanggotaan harga sedang.
    harga_mahal (float): Derajat keanggotaan harga mahal.

  Returns:
    list: List of tuples, di mana setiap tuple berisi bobot (α) dan nilai crisp hasil inferensi.
  "
  (let ((rule-outputs '()))
    (let ((pelayanan-inputs `(("RENDAH" . ,kualitas-pelayanan-rendah)
                               ("SEDANG" . ,kualitas-pelayanan-sedang)
                               ("TINGGI" . ,kualitas-pelayanan-tinggi)))
          (harga-inputs `(("MURAH" . ,harga-murah)
                          ("SEDANG" . ,harga-sedang)
                          ("MAHAL" . ,harga-mahal)))
          (output-crisp 0))
      (loop for rule-num being the hash-keys of *fuzzy-rules-tsukamoto*
            do (let* ((rule (gethash rule-num *fuzzy-rules-tsukamoto*))
                       (pelayanan-condition (cdr (assoc 'pelayanan rule)))
                       (harga-condition (cdr (assoc 'harga rule)))
                       (output-crisp (cdr (assoc 'kelayakan rule)))
                       (alpha (if (member rule-num '(1 3 5 9)) ;; Rules menggunakan OR
                                  (max (cdr (assoc pelayanan-condition pelayanan-inputs))
                                       (cdr (assoc harga-condition harga-inputs)))
                                  ;; Rules menggunakan AND
                                  (min (cdr (assoc pelayanan-condition pelayanan-inputs))
                                       (cdr (assoc harga-condition harga-inputs))))))
                  (push (list (cl-tuples:make-tuple alpha output-crisp)) rule-outputs)))
      rule-outputs)))

(defun defuzzifikasi-centroid-tsukamoto (rule-outputs)
  "
  Melakukan defuzzifikasi menggunakan metode centroid untuk Tsukamoto.

  Args:
    rule_outputs (list): List of tuples (bobot, nilai_crisp) dari hasil inferensi.

  Returns:
    float: Skor kelayakan hasil defuzzifikasi.
  Exceptions:
    Zero Division: jika hasil denominator sama dengan 0, keluarkan error ini.
    Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat melakukan defuzzifikasi.
  "
  (handler-case
      (let ((numerator (reduce #'+ (mapcar (lambda (x) (* (car x) (cdr x))) rule-outputs)))
            (denominator (reduce #'+ (mapcar #'car rule-outputs))))
        (if (zerop denominator)
            (division-by-zero "Selisih pembagi sama dengan 0, melakukan terminasi dengan mengembalikan nilai 0"
                + "(diluar jangkauan)...")
            (/ numerator denominator)))
    (division-by-zero (zde)
      (format t "Penyebab error: ~a" (error-conditions zde))
      0)
    (error (e)
      (format t "Terjadi kesalahan saat defuzzifikas: ~a" e)
      0)
    (t
      (format t "Defuzzifikasi berhasil!"))))

; Fungsi untuk membaca data dari file CSV
(defun read-csv-data (file-path)
  "
  Membaca data dari file CSV.

  Args:
    file_path (str): Path ke file CSV.

  Returns:
    list: List of dictionaries, di mana setiap dictionary merepresentasikan satu baris data.
        Mengembalikan None jika terjadi kesalahan.
    
  Exceptions:
    File not found: Jika file akan dibaca (dari input) tidak ada, maka keluarkan error ini. 
        Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat membaca file CSV.
  "
  (handler-case
      (let ((data '()))
        (with-open-file (csvfile file-path :direction :input)
          (let ((reader (csv:make-csv-reader csvfile)))
            (loop for row = (csv:read-csv-row reader)
                  while row
                  do (progn
                       ; Convert string values to appropriate types (float for pelayanan and harga, int for id)
                       (setf (getf row 'id) (parse-integer (getf row 'id)))
                       (setf (getf row 'pelayanan) (parse-float:parse-float (getf row 'pelayanan)))
                       (setf (getf row 'harga) (parse-float:parse-float (getf row 'harga)))
                       (push row data)))))
        (nreverse data))
    (file-error (e)
      (format t "File tidak ditemukan: ~a" file-path)
      nil)
    (error (e)
      (format t "Terjadi kesalahan saat membaca file CSV: ~a" e)
      nil)
    (t
      (format t "Berhasil membaca file CSV: ~a" file-path))))

; Fungsi untuk menulis data ke file CSV
(defun write-csv-data (file-path data header)
  "
  Menulis data ke file CSV.

  Args:
    file_path (str): Path ke file CSV yang akan dibuat.
    data (list): List of dictionaries yang akan ditulis ke file CSV.
    header (list): List dari string yang merupakan header dari CSV.
    
  Exceptions:
    Keluarkan error untuk kondisi tak terduga saat menulis (membuat) file CSV.
  "
  (handler-case
      (with-open-file (csvfile file-path :direction :output :if-exists :supersede)
        (let ((writer (csv:make-csv-writer csvfile)))
          (csv:write-csv-row writer header)
          (dolist (row data)
            (csv:write-csv-row writer row))))
    (error (e)
      (format t "Terjadi kesalahan saat menulis file CSV: ~a" e))
    (t
      (format t "Berhasil menyimpan data ke file CSV: ~a" file-path))))

; Fungsi untuk memilih restoran terbaik dari file CSV
(defun pilih-restoran-terbaik (csv-file-path num-restoran output-file-path)
  "
  Membaca data restoran dari file CSV, menghitung skor kelayakan,
  mengurutkan restoran berdasarkan skor, dan menyimpan hasilnya ke file CSV baru.

  Args:
    csv_file_path (str): Path ke file CSV yang berisi data restoran.
    num_restoran (int): Jumlah restoran terbaik yang akan dipilih.
    output_file_path (str): Path ke file CSV untuk menyimpan hasilnya.
  "
  (let ((data (read-csv-data csv-file-path)))
    (if (null data)
        (return))
    (let ((hasil-restoran '()))
      (dolist (row data)
        (let ((id-restoran (getf row 'id))
              (pelayanan (getf row 'pelayanan))
              (harga (getf row 'harga)))
          (multiple-value-bind (kualitas-pelayanan-rendah kualitas-pelayanan-sedang kualitas-pelayanan-tinggi)
              (kualitas-pelayanan-fuzzy pelayanan)
            (multiple-value-bind (harga-murah harga-sedang harga-mahal)
                (harga-fuzzy harga)
              (let ((rule-outputs (inferensi-fuzzy kualitas-pelayanan-rendah kualitas-pelayanan-sedang kualitas-pelayanan-tinggi
                                                    harga-murah harga-sedang harga-mahal))
                    (skor-kelayakan (defuzzifikasi rule-outputs)))
                (push `((id-restoran . ,id-restoran)
                         (pelayanan . ,pelayanan)
                         (harga . ,harga)
                         (skor-kelayakan . ,skor-kelayakan))
                      hasil-restoran)))))))
      (let ((hasil-restoran-sorted (sort hasil-restoran #'> :key (lambda (x) (getf x 'skor-kelayakan)))))
        (let ((restoran-terbaik (subseq hasil-restoran-sorted 0 num-restoran)))
          (let ((header '("id_restoran" "pelayanan" "harga" "skor_kelayakan")))
            (write-csv-data output-file-path restoran-terbaik header))
          (format t "~%5 Best Restaurants:")
          (dolist (restoran restoran-terbaik)
            (format t "ID: ~a, Service Quality: ~a, Price: ~a, Feasibility Score: ~a"
                    (getf restoran 'id_restoran)
                    (getf restoran 'pelayanan)
                    (getf restoran 'harga)
                    (getf restoran 'skor-kelayakan))))))))

; Program Utama
(defun main ()
  (let ((csv-file "restoran.csv")  ; Input CSV file
        (output-csv-file "peringkat.csv") ; Output CSV file
        (num-restoran 5))
    ; Proses Fuzzy dan dapatkan restoran terbaik dari CSV
    (pilih-restoran-terbaik csv-file num-restoran output-csv-file)))

(main)