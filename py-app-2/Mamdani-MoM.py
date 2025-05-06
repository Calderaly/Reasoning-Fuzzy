import csv

# Fungsi untuk menghitung derajat keanggotaan menggunakan fungsi segitiga
def triangular(x, a, b, c):
    """
    Menghitung derajat keanggotaan untuk fungsi keanggotaan segitiga.

    Args:
        x (float): Nilai input.
        a (float): Batas bawah segitiga.
        b (float): Nilai puncak segitiga.
        c (float): Batas atas segitiga.

    Returns:
        float: Derajat keanggotaan.
    """
    if a <= x <= b:
        return (x - a) / (b - a)
    elif b < x <= c:
        return (c - x) / (c - b)
    else:
        return 0

# Fungsi untuk menghitung derajat keanggotaan untuk kualitas pelayanan
def kualitas_pelayanan_fuzzy(pelayanan):
    """
    Menghitung derajat keanggotaan fuzzy untuk kualitas pelayanan.

    Args:
        pelayanan (float): Nilai kualitas pelayanan.

    Returns:
        tuple: Derajat keanggotaan (rendah, sedang, tinggi).
    """
    rendah = triangular(pelayanan, 0, 20, 50)
    sedang = triangular(pelayanan, 20, 50, 80)
    tinggi = triangular(pelayanan, 50, 80, 100)
    return rendah, sedang, tinggi

# Fungsi untuk menghitung derajat keanggotaan untuk harga
def harga_fuzzy(harga):
    """
    Menghitung derajat keanggotaan fuzzy untuk harga.

    Args:
        harga (float): Nilai harga.

    Returns:
        tuple: Derajat keanggotaan (murah, sedang, mahal).
    """
    murah = triangular(harga, 25000, 30000, 40000)
    sedang = triangular(harga, 30000, 40000, 50000)
    mahal = triangular(harga, 40000, 50000, 55000)
    return murah, sedang, mahal

# Aturan-aturan fuzzy (sebagai variabel global)
fuzzy_rules = {
    1: {'pelayanan': 'RENDAH', 'harga': 'MAHAL', 'kelayakan': 'RENDAH'},
    2: {'pelayanan': 'SEDANG', 'harga': 'SEDANG', 'kelayakan': 'SEDANG'},
    3: {'pelayanan': 'TINGGI', 'harga': 'MURAH', 'kelayakan': 'TINGGI'},
    4: {'pelayanan': 'RENDAH', 'harga': 'MURAH', 'kelayakan': 'RENDAH'},
    5: {'pelayanan': 'SEDANG', 'harga': 'MURAH', 'kelayakan': 'SEDANG'},
    6: {'pelayanan': 'TINGGI', 'harga': 'SEDANG', 'kelayakan': 'TINGGI'},
    7: {'pelayanan': 'RENDAH', 'harga': 'SEDANG', 'kelayakan': 'RENDAH'},
    8: {'pelayanan': 'SEDANG', 'harga': 'MAHAL', 'kelayakan': 'SEDANG'},
    9: {'pelayanan': 'TINGGI', 'harga': 'MAHAL', 'kelayakan': 'TINGGI'},
}

# Fungsi untuk melakukan inferensi fuzzy
def inferensi_fuzzy(kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi, harga_murah, harga_sedang, harga_mahal):
    """
    Melakukan inferensi fuzzy berdasarkan aturan-aturan yang telah didefinisikan.

    Args:
        kualitas_pelayanan_rendah (float): Derajat keanggotaan kualitas pelayanan rendah.
        kualitas_pelayanan_sedang (float): Derajat keanggotaan kualitas pelayanan sedang.
        kualitas_pelayanan_tinggi (float): Derajat keanggotaan kualitas pelayanan tinggi.
        harga_murah (float): Derajat keanggotaan harga murah.
        harga_sedang (float): Derajat keanggotaan harga sedang.
        harga_mahal (float): Derajat keanggotaan harga mahal.

    Returns:
        tuple: Derajat keanggotaan (kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi).
    """
    rule_outputs = {}

    # Mapping derajat keanggotaan ke dalam bentuk dictionary untuk mempermudah akses
    pelayanan_inputs = {
        'RENDAH': kualitas_pelayanan_rendah,
        'SEDANG': kualitas_pelayanan_sedang,
        'TINGGI': kualitas_pelayanan_tinggi,
    }
    harga_inputs = {
        'MURAH': harga_murah,
        'SEDANG': harga_sedang,
        'MAHAL': harga_mahal,
    }

    # Iterasi melalui setiap aturan dan menghitung implikasi
    for rule_num, rule in fuzzy_rules.items():
        pelayanan_condition = pelayanan_inputs[rule['pelayanan']]
        harga_condition = harga_inputs[rule['harga']]

        if rule['kelayakan'] not in rule_outputs:
            rule_outputs[rule['kelayakan']] = []

        if rule_num in [1, 3, 5, 9]: # Rules menggunakan OR
            rule_outputs[rule['kelayakan']].append(max(pelayanan_condition, harga_condition))
        else: # Rules menggunakan AND
            rule_outputs[rule['kelayakan']].append(min(pelayanan_condition, harga_condition))

    # Menggabungkan output dari setiap aturan untuk mendapatkan derajat keanggotaan akhir
    kelayakan_rendah = max(rule_outputs.get('RENDAH', [0]))
    kelayakan_sedang = max(rule_outputs.get('SEDANG', [0]))
    kelayakan_tinggi = max(rule_outputs.get('TINGGI', [0]))
    
    return kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi

# Fungsi untuk melakukan defuzzifikasi (menggunakan metode Mean of Maximum - MOM)
def defuzzifikasi(kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi):
    """
    Melakukan defuzzifikasi menggunakan metode Mean of Maximum (MOM).

    Args:
        kelayakan_rendah (float): Derajat keanggotaan kelayakan rendah.
        kelayakan_sedang (float): Derajat keanggotaan kelayakan sedang.
        kelayakan_tinggi (float): Derajat keanggotaan kelayakan tinggi.

    Returns:
        float: Skor kelayakan hasil defuzzifikasi.
    """
    # Representasi domain output (kelayakan)
    domain = list(range(0, 101))

    # Hitung output untuk setiap kategori
    output_rendah = [domain_val for domain_val in domain if domain_val <= 30]
    output_sedang = [domain_val for domain_val in domain if 30 < domain_val <= 70]
    output_tinggi = [domain_val for domain_val in domain if domain_val > 70]

    # Dapatkan nilai keanggotaan untuk setiap kategori
    keanggotaan_rendah = kelayakan_rendah
    keanggotaan_sedang = kelayakan_sedang
    keanggotaan_tinggi = kelayakan_tinggi

    # Cari nilai maksimum dari setiap keanggotaan
    max_keanggotaan_rendah = keanggotaan_rendah
    max_keanggotaan_sedang = keanggotaan_sedang
    max_keanggotaan_tinggi = keanggotaan_tinggi

    # Ambil semua nilai domain yang memiliki keanggotaan maksimum
    max_output_rendah = [output_rendah[i] for i, val in enumerate(output_rendah) if keanggotaan_rendah]
    max_output_sedang = [output_sedang[i] for i, val in enumerate(output_sedang) if keanggotaan_sedang]
    max_output_tinggi = [output_tinggi[i] for i, val in enumerate(output_tinggi) if keanggotaan_tinggi]

    # Gabungkan semua nilai output yang memiliki keanggotaan maksimum
    max_output_values = max_output_rendah + max_output_sedang + max_output_tinggi

    # Jika tidak ada nilai maksimum, kembalikan nilai tengah
    if not max_output_values:
        return 50
    else:
        # Hitung rata-rata dari nilai-nilai output maksimum
        return sum(max_output_values) / len(max_output_values)

# Fungsi untuk membaca data dari file CSV
def read_csv_data(file_path):
    """
    Membaca data dari file CSV.

    Args:
        file_path (str): Path ke file CSV.

    Returns:
        list: List of dictionaries, di mana setiap dictionary merepresentasikan satu baris data.
              Mengembalikan None jika terjadi kesalahan.
    """
    try:
        data = []
        with open(file_path, 'r') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                # Convert string values to appropriate types (float for pelayanan and harga, int for id)
                row['id'] = int(row['id'])
                row['pelayanan'] = float(row['pelayanan'])
                row['harga'] = float(row['harga'])
                data.append(row)
        return data
    except FileNotFoundError:
        print(f"File tidak ditemukan: {file_path}")
        return None
    except Exception as e:
        print(f"Terjadi kesalahan saat membaca file CSV: {e}")
        return None

# Fungsi untuk menulis data ke file CSV
def write_csv_data(file_path, data, header):
    """
    Menulis data ke file CSV.

    Args:
        file_path (str): Path ke file CSV yang akan dibuat.
        data (list): List of dictionaries yang akan ditulis ke file CSV.
        header (list): List dari string yang merupakan header dari CSV.
    """
    try:
        with open(file_path, 'w', newline='') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=header)
            writer.writeheader()
            for row in data:
                writer.writerow(row)
        print(f"Berhasil menyimpan data ke file CSV: {file_path}")
    except Exception as e:
        print(f"Terjadi kesalahan saat menulis file CSV: {e}")

# Fungsi untuk memilih restoran terbaik dari file CSV
def pilih_restoran_terbaik(csv_file_path, num_restoran, output_file_path):
    """
    Membaca data restoran dari file CSV, menghitung skor kelayakan,
    mengurutkan restoran berdasarkan skor, dan menyimpan hasilnya ke file CSV baru.

    Args:
        csv_file_path (str): Path ke file CSV yang berisi data restoran.
        num_restoran (int): Jumlah restoran terbaik yang akan dipilih.
        output_file_path (str): Path ke file CSV untuk menyimpan hasilnya.
    """
    data = read_csv_data(csv_file_path)
    if data is None:
        return

    hasil_restoran = []
    for row in data:
        id_restoran = row['id']
        pelayanan = row['pelayanan']
        harga = row['harga']

        kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi = kualitas_pelayanan_fuzzy(pelayanan)
        harga_murah, harga_sedang, harga_mahal = harga_fuzzy(harga)
        kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi = inferensi_fuzzy(
            kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi,
            harga_murah, harga_sedang, harga_mahal
        )
        skor_kelayakan = defuzzifikasi(kelayakan_rendah, kelayakan_sedang, kelayakan_tinggi)

        hasil_restoran.append({
            'id_restoran': id_restoran,
            'pelayanan': pelayanan,
            'harga': harga,
            'skor_kelayakan': skor_kelayakan
        })

    hasil_restoran_sorted = sorted(hasil_restoran, key=lambda x: x['skor_kelayakan'], reverse=True)
    restoran_terbaik = hasil_restoran_sorted[:num_restoran]

    header = ['id_restoran', 'pelayanan', 'harga', 'skor_kelayakan']
    write_csv_data(output_file_path, restoran_terbaik, header)

    print("\n5 Restoran Terbaik:")
    for restoran in restoran_terbaik:
        print(f"ID: {restoran['id_restoran']}, Kualitas Servis: {restoran['pelayanan']:.2f}, Harga: {restoran['harga']:.2f}, Skor Kelayakan: {restoran['skor_kelayakan']:.2f}")



if _name_ == "_main_":
    csv_file = "restoran.csv"  # Input CSV file
    output_csv_file = "peringkat.csv" # Output CSV file
    num_restoran = 5

    # Proses Fuzzy dan dapatkan restoran terbaik dari CSV
    pilih_restoran_terbaik(csv_file, num_restoran, output_csv_file)