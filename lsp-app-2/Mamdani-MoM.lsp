; Importing necessary libraries
(defpackage :restaurant-fuzzy 
  (:use :cl :csv :cl-tuples :parse-float))
(in-package :my-package)

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
    (cl-tuples:make-tuple (list rendah sedang tinggi)))) ; Using cl-tuples for tuple creation

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
    (cl-tuples:make-tuple (list murah sedang mahal)))) ; Using cl-tuples for tuple creation

(defparameter *fuzzy-rules* (make-hash-table)) ; Using hash table for fuzzy rules

(setf (gethash 1 *fuzzy-rules*) '(:pelayanan "RENDAH" :harga "MAHAL" :kelayakan "RENDAH"))
(setf (gethash 2 *fuzzy-rules*) '(:pelayanan "SEDANG" :harga "SEDANG" :kelayakan "SEDANG"))
(setf (gethash 3 *fuzzy-rules*) '(:pelayanan "TINGGI" :harga "MURAH" :kelayakan "TINGGI"))
(setf (gethash 4 *fuzzy-rules*) '(:pelayanan "RENDAH" :harga "MURAH" :kelayakan "RENDAH"))
(setf (gethash 5 *fuzzy-rules*) '(:pelayanan "SEDANG" :harga "MURAH" :kelayakan "SEDANG"))
(setf (gethash 6 *fuzzy-rules*) '(:pelayanan "TINGGI" :harga "SEDANG" :kelayakan "TINGGI"))
(setf (gethash 7 *fuzzy-rules*) '(:pelayanan "RENDAH" :harga "SEDANG" :kelayakan "RENDAH"))
(setf (gethash 8 *fuzzy-rules*) '(:pelayanan "SEDANG" :harga "MAHAL" :kelayakan "SEDANG"))
(setf (gethash 9 *fuzzy-rules*) '(:pelayanan "TINGGI" :harga "MAHAL" :kelayakan "TINGGI"))

(defun inferensi-fuzzy (kualitas-pelayanan-rendah kualitas-pelayanan-sedang kualitas-pelayanan-tinggi harga-murah harga-sedang 
  harga-mahal)
  "
  Melakukan inferensi fuzzy berdasarkan aturan-aturan yang telah didefinisikan.

  Args:
      kualitas_pelayanan_rendah (float): Derajat keanggotaan kualitas pelayanan rendah.
      kualitas_pelayanan_sedang (float): Derajat keanggotaan kualitas pelayanan sedang.
      kualitas_pelayanan_tinggi (float): Derajat keanggotaan kualitas pelayanan tinggi.
      harga_murah (float): Derajat keanggotaan harga murah.
      harga_sedang (float): Derajat keanggotaan harga sedang.
      harga_mahal (float): Derajat keanggotaan harga mahal.

  Returns:
      list of tuple: Derajat keanggotaan (kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi).
  "
  (let ((rule-outputs (make-hash-table))
        ; Mapping derajat keanggotaan ke dalam bentuk hash table untuk mempermudah akses
        (pelayanan-inputs (list (cons "RENDAH" kualitas-pelayanan-rendah)
                                (cons "SEDANG" kualitas-pelayanan-sedang)
                                (cons "TINGGI" kualitas-pelayanan-tinggi)))
        (harga-inputs (list (cons "MURAH" harga-murah)
                            (cons "SEDANG" harga-sedang)
                            (cons "MAHAL" harga-mahal))))
    ; Iterasi melalui setiap aturan dan menghitung implikasi
    (dolist (rule (loop for i from 1 to 9 collect (gethash i *fuzzy-rules*)))
      (let* ((pelayanan-condition (cdr (assoc (getf rule :pelayanan) pelayanan-inputs)))
             (harga-condition (cdr (assoc (getf rule :harga) harga-inputs)))
             (kelayakan (getf rule :kelayakan)))
        (unless (gethash kelayakan rule-outputs)
          (setf (gethash kelayakan rule-outputs) (list)))
        (if (member (car rule) '(1 3 5 9)) ; Rules menggunakan OR
            (push (max pelayanan-condition harga-condition) (gethash kelayakan rule-outputs))
            ; Rules menggunakan AND
            (push (min pelayanan-condition harga-condition) (gethash kelayakan rule-outputs)))))
    ; Menggabungkan output dari setiap aturan untuk mendapatkan derajat keanggotaan akhir, lalu kembalikan hasilnya
    (cl-tuples:make-tuple (list (apply #'max (gethash "RENDAH" rule-outputs))
                                 (apply #'max (gethash "SEDANG" rule-outputs))
                                 (apply #'max (gethash "TINGGI" rule-outputs)))))) ; Using cl-tuples for tuple creation

; Fungsi untuk melakukan defuzzifikasi (menggunakan metode Mean of Maximum - MOM)
(defun defuzzifikasi (kelayakan-rendah kelayakan-sedang kelayakan-tinggi)
  "
  Melakukan defuzzifikasi menggunakan metode Mean of Maximum (MOM).

  Args:
    kelayakan_rendah (float): Derajat keanggotaan kelayakan rendah.
    kelayakan_sedang (float): Derajat keanggotaan kelayakan sedang.
    kelayakan_tinggi (float): Derajat keanggotaan kelayakan tinggi.

  Returns:
    float: Skor kelayakan hasil defuzzifikasi.
    
  Exceptions:
    Zero Division: jika hasil denominator sama dengan 0, keluarkan error ini.
    Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat melakukan defuzzifikasi.
    "
  (handler-case
      (let ((domain (loop for i from 0 to 100 collect i)))
      (let ((output-rendah (remove-if-not (lambda (val) (<= val 30)) domain))
            (output-sedang (remove-if-not (lambda (val) (and (> val 30) (<= val 70))) domain))
            (output-tinggi (remove-if-not (lambda (val) (> val 70)) domain)))
        (let ((keanggotaan-rendah kelayakan-rendah)
              (keanggotaan-sedang kelayakan-sedang)
              (keanggotaan-tinggi kelayakan-tinggi))
          (let ((max-output-rendah (when keanggotaan-rendah output-rendah))
                (max-output-sedang (when keanggotaan-sedang output-sedang))
                (max-output-tinggi (when keanggotaan-tinggi output-tinggi)))
            (let ((max-output-values (append max-output-rendah max-output-sedang max-output-tinggi)))
              (if (null max-output-values)
                  50
                  (/ (reduce #'+ max-output-values) (length max-output-values)))))))))
    (division-by-zero (e)
      (format t "Division difference equals 0, terminating by returning value 0 (out of range)...")
      0)
    (error (e)
      (format t "An error occurred during defuzzification: ~a" e)
      0)
    (t
      (format t "Defuzzification successful!")))

; Kalau pakai lisp-xl, bikin fungsi konversi file.xlsx ke file.csv dan timpa disini

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
          (let ((reader (csv:make-reader csvfile)))
            (loop for row = (csv:read-csv-row reader)
                  while row
                  do (progn
                      ; Convert string values to appropriate types (float for pelayanan and harga, int for id)
                       (setf (getf row 'id_Pelanggan) (parse-integer (getf row 'id_Pelanggan)))
                       (setf (getf row 'Pelayanan) (parse-float:parse-float (getf row 'Pelayanan)))
                       (setf (getf row 'harga) (parse-float:parse-float (getf row 'harga)))
                       (push row data)))))
        (nreverse data)) ; mengembalikan list data
    (file-error (e)
      (format t "File tidak ditemukan: ~a" file-path)
      nil)
    (error (e)
      (format t "Terjadi kesalahan saat membaca file CSV: ~a" e)
      nil)
    (t
      (format t "Berhasil membaca file CSV: ~a" file-path))))

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
        (let ((writer (csv:make-writer csvfile)))
          (csv:write-csv-row writer header)
          (dolist (row data)
            (csv:write-csv-row writer row))))
    (error (e)
      (format t "Terjadi kesalahan saat menulis file CSV: ~a" e))
    (t
      (format t "Berhasil menyimpan data ke file CSV: ~a" file-path))))

; Kalau pakai lisp-xl, bikin fungsi konversi file.csv ke file.xlsx dan timpa disini

(defun sort-key (restoran)
  "Kunci sorting kustom."
  (list (- (getf restoran 'skor-kelayakan)) ; Urutkan skor kelayakan dari tertinggi
        (- (getf restoran 'pelayanan))    ; Urutkan pelayanan dari tertinggi
        (getf restoran 'harga)))        ; Urutkan harga dari terendah

(defun pilih-restoran-terbaik (csv-file-path num-restoran output-file-path)
  "
  Membaca data restoran dari file CSV, menghitung skor kelayakan,
  mengurutkan restoran berdasarkan skor, dan menyimpan hasilnya ke file CSV baru.
  Pengurutan dilakukan berdasarkan skor kelayakan (tertinggi),
  kemudian berdasarkan kualitas pelayanan (tertinggi), dan terakhir berdasarkan harga (terendah).

  Args:
      csv_file_path (str): Path ke file CSV yang berisi data restoran, bisa diganti untuk file.xlsx
      num_restoran (int): Jumlah restoran terbaik yang akan dipilih.
      output_file_path (str): Path ke file CSV untuk menyimpan hasilnya, bisa diganti untuk file.xlsx
  "
  ; Apabila menggunakan file excel, panggil fungsi untuk konversi file.xlsx ke file.csv dan timpa komentar ini
  (let ((data (read-csv-data csv-file-path)))
    (if (null data)
        (return))
    (let ((hasil-restoran '()))
      (dolist (row data)
        (let* ((id-restoran (getf row 'Id-pelanggan))
               (pelayanan (getf row 'Pelayanan))
               (harga (getf row 'Harga))
               (kualitas-pelayanan-rendah (kualitas-pelayanan-fuzzy pelayanan))
               (harga-murah (harga-fuzzy harga))
               (kelayakan (inferensi-fuzzy kualitas-pelayanan-rendah harga-murah)))
          (let ((skor-kelayakan (defuzzifikasi (first kelayakan) (second kelayakan) (third kelayakan))))
            (push (list 'id-restoran id-restoran
                        'pelayanan pelayanan
                        'harga harga
                        'skor-kelayakan skor-kelayakan)
                  hasil-restoran))))
      ; Urutkan restoran menggunakan kunci sorting kustom
      (let ((restoran-terbaik (subseq (sort hasil-restoran #'sort-key) 0 num-restoran)))
        (let ((header '("id_restoran" "pelayanan" "harga" "skor_kelayakan")))
          (write-csv-data output-file-path restoran-terbaik header))
        ; Apabila menggunakan file excel, panggil fungsi untuk konversi file.csv ke file.xlsx dan timpa komentar ini
        (format t "~%~a Restoran Terbaik:" num-restoran)
        (dolist (restoran restoran-terbaik)
          (format t "ID: ~a, Kualitas Pelayanan: ~a, Harga: ~a, Skor Kelayakan: ~a"
                  (getf restoran 'id-restoran)
                  (getf restoran 'pelayanan)
                  (getf restoran 'harga)
                  (getf restoran 'skor-kelayakan)))))))

; Program Utama
(defun main ()
  (let ((csv-file "restoran.csv") ;  Input CSV file, ganti jadi xlsx kalau pakai file excel
        (num-restaurant-selected 5) ; Limit up to top 5 restaurants
        (output-csv-file "peringkat.csv")) ; Output CSV file, ganti jadi xlsx kalau pakai file excel
    ; Proses Fuzzy dan dapatkan restoran terbaik dari CSV
    (pilih-restoran-terbaik csv-file num-restaurant-selected output-csv-file)))

; Uncomment the following line to run the main function
(main)