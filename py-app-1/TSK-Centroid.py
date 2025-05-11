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
    else:
        print("Berhasil menghitung derajat keanggotaan!")

# Fungsi untuk menghitung derajat keanggotaan untuk kualitas pelayanan
def kualitas_pelayanan_fuzzy(pelayanan):
    """
    Menghitung derajat keanggotaan fuzzy untuk kualitas pelayanan.

    Args:
        pelayanan (float): Nilai kualitas pelayanan.

    Returns:
        tuple: Derajat keanggotaan (rendah, sedang, tinggi).
    
    Exception:
        Zero Division: Jika selisih (b - a) dan atau (c - b) sama dengan 0, keluarkan error ini. 
        Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat menghitung derajat keanggotaan.
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
    1: {'pelayanan': 'RENDAH', 'harga': 'MAHAL', 'kelayakan': lambda p, h: 10 + 0.2 * p + 0.1 * h},  # Contoh TSK: output = f(input)
    2: {'pelayanan': 'SEDANG', 'harga': 'SEDANG', 'kelayakan': lambda p, h: 50 + 0.5 * p - 0.1 * h},
    3: {'pelayanan': 'TINGGI', 'harga': 'MURAH', 'kelayakan': lambda p, h: 80 + 0.3 * p - 0.2 * h},
    4: {'pelayanan': 'RENDAH', 'harga': 'MURAH', 'kelayakan': lambda p, h: 20 + 0.1 * p + 0.1 * h},
    5: {'pelayanan': 'SEDANG', 'harga': 'MURAH', 'kelayakan': lambda p, h: 40 + 0.4 * p - 0.1 * h},
    6: {'pelayanan': 'TINGGI', 'harga': 'SEDANG', 'kelayakan': lambda p, h: 70 + 0.2 * p + 0.1 * h},
    7: {'pelayanan': 'RENDAH', 'harga': 'SEDANG', 'kelayakan': lambda p, h: 30 + 0.3 * p - 0.2 * h},
    8: {'pelayanan': 'SEDANG', 'harga': 'MAHAL', 'kelayakan': lambda p, h: 60 + 0.6 * p - 0.3 * h},
    9: {'pelayanan': 'TINGGI', 'harga': 'MAHAL', 'kelayakan': lambda p, h: 90 + 0.4 * p + 0.2 * h},
}

# Fungsi untuk melakukan inferensi fuzzy
def inferensi_fuzzy(kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi, harga_murah, harga_sedang, 
harga_mahal, pelayanan, harga):
    """
    Melakukan inferensi fuzzy berdasarkan aturan-aturan yang telah didefinisikan (TSK).

    Args:
        kualitas_pelayanan_rendah (float): Derajat keanggotaan kualitas pelayanan rendah.
        kualitas_pelayanan_sedang (float): Derajat keanggotaan kualitas pelayanan sedang.
        kualitas_pelayanan_tinggi (float): Derajat keanggotaan kualitas pelayanan tinggi.
        harga_murah (float): Derajat keanggotaan harga murah.
        harga_sedang (float): Derajat keanggotaan harga sedang.
        harga_mahal (float): Derajat keanggotaan harga mahal.
        pelayanan (float): Nilai crisp pelayanan
        harga (float): Nilai crisp harga

    Returns:
        list of tuple: berisi bobot aturan dan nilai output crispnya
    """
    rule_outputs = []

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

    # Iterasi melalui setiap aturan dan menghitung output crisp
    for rule_num, rule in fuzzy_rules.items():
        pelayanan_condition = pelayanan_inputs[rule['pelayanan']]
        harga_condition = harga_inputs[rule['harga']]

        if rule_num in [1, 3, 5, 9]:  # Rules menggunakan OR
            weight = max(pelayanan_condition, harga_condition)
        else:  # Rules menggunakan AND
            weight = min(pelayanan_condition, harga_condition)
        
        # Hitung output crisp dengan memanggil fungsi konsekuen
        output_value = rule['kelayakan'](pelayanan, harga)  # Calculate output using TSK function
        rule_outputs.append((weight, output_value))  # Store weight and crisp output

    return rule_outputs



# Fungsi untuk melakukan defuzzifikasi (menggunakan metode Centroid untuk TSK)
def defuzzifikasi(rule_outputs):
    """
    Melakukan defuzzifikasi menggunakan metode Centroid untuk logika TSK.

    Args:
        rule_outputs (list): List of tuple berisi bobot aturan dan nilai output crispnya.
            Setiap tuple berisi (weight, output_value).

    Returns:
        float: Skor kelayakan hasil defuzzifikasi.
    
    Exceptions:
        Zero Division: jika hasil denominator sama dengan 0, keluarkan error ini.
        Selain itu, keluarkan error untuk kondisi tak terduga lainnya saat melakukan defuzzifikasi.
    """
    try:    
        numerator = 0
        denominator = 0

        for weight, output_value in rule_outputs:
            numerator += weight * output_value
            denominator += weight

        if denominator == 0:
            raise ZeroDivisionError("Selisih pembagi sama dengan 0, melakukan terminasi dengan mengembalikan nilai 50 "
            + "(diluar jangkauan)...")
        else:
            return numerator / denominator
    except ZeroDivisionError as zde:
        print(f"Penyebab error: {zde.args}")
        return 50 # Mengembalikan nilai tengah jika tidak ada aturan yang aktif
    except Exception as e:
        print(f"Terjadi kesalahan saat defuzzifikasi: {e}")
        return 0
    else:
        print("Defuzzifikasi berhasil!")

# Fungsi untuk membaca data dari file CSV
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
    else:
        print(f"Berhasil membaca file CSV: {file_path}")

# Fungsi untuk menulis data ke file CSV
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
        print(f"Berhasil menyimpan data ke file CSV: {file_path}")
    except Exception as e:
        print(f"Terjadi kesalahan saat menulis file CSV: {e}")
    else:
        print(f"Berhasil menyimpan data ke file CSV: {file_path}")

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
        rule_outputs = inferensi_fuzzy(
            kualitas_pelayanan_rendah, kualitas_pelayanan_sedang, kualitas_pelayanan_tinggi,
            harga_murah, harga_sedang, harga_mahal, pelayanan, harga
        )
        skor_kelayakan = defuzzifikasi(rule_outputs)

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
        print(f"ID: {restoran['id_restoran']}, Kualitas Pelayanan: {restoran['pelayanan']:.2f}, Harga: {restoran['harga']:.2f}, 
        Skor Kelayakan: {restoran['skor_kelayakan']:.2f}")



if __name__ == "__main__":
    csv_file = "restoran.csv"  # Input CSV file
    output_csv_file = "peringkat.csv" # Output CSV file
    num_restoran = 5

    # Proses Fuzzy dan dapatkan restoran terbaik dari CSV
    pilih_restoran_terbaik(csv_file, num_restoran, output_csv_file)