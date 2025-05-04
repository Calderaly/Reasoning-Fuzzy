import csv

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

    Exceptions:
        Zero Division: Jika selisih (b - a) dan atau (c - b) sama dengan 0, keluarkan error ini.
        Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat menghitung derajat keanggotaan.
    """
    try:
        if a <= x <= b:
            return (x - a) / (b - a)
        elif b < x <= c:
            return (c - x) / (c - b)
        else:
            return 0
    except ZeroDivisionError:
        print("Selisih pembagi sama dengan 0, melakukan terminasi dengan mengembalikan nilai 0 (diluar jangkauan)...")
        return 0
    except Exception as e:
        print(e)

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

fuzzy_rules_tsukamoto = {
    1: {'pelayanan': 'RENDAH', 'harga': 'MAHAL', 'kelayakan': 35},
    2: {'pelayanan': 'SEDANG', 'harga': 'SEDANG', 'kelayakan': 65},
    3: {'pelayanan': 'TINGGI', 'harga': 'MURAH', 'kelayakan': 95},
    4: {'pelayanan': 'RENDAH', 'harga': 'MURAH', 'kelayakan': 45},
    5: {'pelayanan': 'SEDANG', 'harga': 'MURAH', 'kelayakan': 75},
    6: {'pelayanan': 'TINGGI', 'harga': 'SEDANG', 'kelayakan': 85},
    7: {'pelayanan': 'RENDAH', 'harga': 'SEDANG', 'kelayakan': 55},
    8: {'pelayanan': 'SEDANG', 'harga': 'MAHAL', 'kelayakan': 60},
    9: {'pelayanan': 'TINGGI', 'harga': 'MAHAL', 'kelayakan': 70},
}

def inferensi_tsukamoto(kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi, harga_murah, harga_sedang, harga_mahal):
    """
    Melakukan inferensi fuzzy menggunakan metode Tsukamoto.

    Args:
        kualitas_pelayanan_rendah (float): Derajat keanggotaan kualitas pelayanan rendah.
        kualitas_pelayanan_sedang (float): Derajat keanggotaan kualitas pelayanan sedang.
        kualitas_pelayanan_tinggi (float): Derajat keanggotaan kualitas pelayanan tinggi.
        harga_murah (float): Derajat keanggotaan harga murah.
        harga_sedang (float): Derajat keanggotaan harga sedang.
        harga_mahal (float): Derajat keanggotaan harga mahal.

    Returns:
        list: List of tuples, di mana setiap tuple berisi bobot ($\alpha$) dan nilai crisp hasil inferensi.
    """
    rule_outputs = []

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

    for rule_num, rule in fuzzy_rules_tsukamoto.items():
        pelayanan_condition = pelayanan_inputs[rule['pelayanan']]
        harga_condition = harga_inputs[rule['harga']]
        output_crisp = rule['kelayakan']

        if rule_num in [1, 3, 5, 9]: # Rules menggunakan OR
            alpha = max(pelayanan_condition, harga_condition)
        else: # Rules menggunakan AND
            alpha = min(pelayanan_condition, harga_condition)
        rule_outputs.append((alpha, output_crisp))

    return rule_outputs

def defuzzifikasi_centroid_tsukamoto(rule_outputs):
    """
    Melakukan defuzzifikasi menggunakan metode centroid untuk Tsukamoto.

    Args:
        rule_outputs (list): List of tuples (bobot, nilai_crisp) dari hasil inferensi.

    Returns:
        float: Skor kelayakan hasil defuzzifikasi.
    Exceptions:
        Zero Division: jika hasil denominator sama dengan 0, keluarkan error ini
    """
    try:
        numerator = sum(alpha * crisp_value for alpha, crisp_value in rule_outputs)
        denominator = sum(alpha for alpha, _ in rule_outputs)
        if denominator == 0:
            raise ZeroDivisionError
        return numerator / denominator
    except ZeroDivisionError:
        print("Nilai pembagi sama dengan 0 pada proses defuzzifikasi, mengembalikan nilai 0.")
        return 0
    except Exception as e:
        print(f"Terjadi kesalahan saat defuzzifikasi: {e}")
        return 0
    else:
        print("Defuzzifikasi berhasil!")

def read_csv_data(file_path):
    """
    Membaca data dari file CSV.

    Args:
        file_path (str): Path ke file CSV.

    Returns:
        list: List of dictionaries, di mana setiap dictionary merepresentasikan satu baris data.
              Mengembalikan None jika terjadi kesalahan.
    Exceptions:
        File not found: Jika file akan dibaca (dari input) tidak ada, maka keluarkan error ini.
        Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat membaca file CSV.
    """
    try:
        data = []
        with open(file_path, 'r') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                # Convert string values to appropriate types (float for pelayanan and harga, int for id)
                row['id_Pelanggan'] = int(row['id_Pelanggan'])
                row['Pelayanan'] = float(row['Pelayanan'])
                row['harga'] = float(row['harga'])
                data.append(row)
        return data
    except FileNotFoundError:
        print(f"File tidak ditemukan: {file_path}")
        return None
    except Exception as e:
        print(f"Terjadi kesalahan saat membaca file CSV: {e}")
        return None
    else:
        print(f"Berhasil membaca file CSV: {file_path}")

def write_csv_data(file_path, data, header):
    """
    Menulis data ke file CSV.

    Args:
        file_path (str): Path ke file CSV yang akan dibuat.
        data (list): List of dictionaries yang akan ditulis ke file CSV.
        header (list): List dari string yang merupakan header dari CSV.

    Exceptions:
        Keluarkan error untuk kondisi tak terduga saat menulis (membuat) file CSV.
    """
    try:
        with open(file_path, 'w', newline='') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=header)
            writer.writeheader()
            for row in data:
                writer.writerow(row)
    except Exception as e:
        print(f"Terjadi kesalahan saat menulis file CSV: {e}")
    else:
        print(f"Berhasil menyimpan data ke file CSV: {file_path}")

def sort_key(restoran):
    """Kunci sorting kustom."""
    return (
        -restoran['skor_kelayakan'],  # Urutkan skor kelayakan dari tertinggi
        -restoran['pelayanan'],      # Urutkan pelayanan dari tertinggi
        restoran['harga']           # Urutkan harga dari terendah
    )

def pilih_restoran_terbaik_tsukamoto(csv_file_path, num_restoran, output_file_path):
    """
    Membaca data restoran dari file CSV, menghitung skor kelayakan menggunakan Tsukamoto,
    mengurutkan restoran berdasarkan skor, dan menyimpan hasilnya ke file CSV baru.
    Pengurutan dilakukan berdasarkan skor kelayakan (tertinggi),
    kemudian berdasarkan kualitas pelayanan (tertinggi), dan terakhir berdasarkan harga (terendah).

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
        id_restoran = row['id_Pelanggan']
        pelayanan = row['Pelayanan']
        harga = row['harga']

        kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi = kualitas_pelayanan_fuzzy(pelayanan)
        harga_murah, harga_sedang, harga_mahal = harga_fuzzy(harga)
        rule_outputs = inferensi_tsukamoto(
            kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi,
            harga_murah, harga_sedang, harga_mahal
        )
        skor_kelayakan = defuzzifikasi_centroid_tsukamoto(rule_outputs)

        hasil_restoran.append({
            'id_restoran': id_restoran,
            'pelayanan': pelayanan,
            'harga': harga,
            'skor_kelayakan': skor_kelayakan
        })

    # Urutkan restoran menggunakan kunci sorting kustom
    restoran_terbaik = sorted(hasil_restoran, key=sort_key)[:num_restoran]

    header = ['id_restoran', 'pelayanan', 'harga', 'skor_kelayakan']
    write_csv_data(output_file_path, restoran_terbaik, header)

    print(f"\n{num_restoran} Restoran Terbaik (Tsukamoto):")
    for restoran in restoran_terbaik:
        print(f"ID: {restoran['id_restoran']}, Kualitas Servis: {restoran['pelayanan']:.2f}, Harga: {restoran['harga']:.2f}, Skor Kelayakan: {restoran['skor_kelayakan']:.2f}")

#Program Utama
if __name__ == "__main__":
    csv_file = "restoran.csv"  # Input CSV file, ganti jadi xlsx kalau pakai file excel
    num_restaurant_selected = 5 # Limit up to top 5 restaurant
    output_csv_file = "peringkat.csv" # Output CSV file, ganti jadi xlsx kalau pakai file excel

    # Proses Fuzzy Tsukamoto dan dapatkan restoran terbaik dari CSV
    pilih_restoran_terbaik_tsukamoto(csv_file, num_restaurant_selected, output_csv_file)