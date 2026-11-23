# 📐 CỬA HÀNG BÁN LISPCAD TỰ ĐỘNG CÓ TRANG QUẢN TRỊ ADMIN

Dự án này là giải pháp trọn gói giúp bạn bán các tiện ích AutoLisp mà **không cần biết lập trình**:
- **Trang bán hàng (`index.html`)**: Đẹp mắt, chuyên nghiệp, tự động cập nhật sản phẩm từ trang Admin.
- **Trang quản trị (`admin.html`)**: Cho phép bạn đăng nhập, tải ảnh mã QR ngân hàng lên, chỉnh giá bán, thêm LispCAD và dán link tải trực tiếp trên trình duyệt.
- **Tải file tự động (`success.html`)**: Khách chuyển khoản xong bấm xác nhận là hệ thống tự động tải file `.lsp` về máy khách ngay lập tức 24/7.

---

## 🔑 TRANG QUẢN TRỊ ADMIN CỦA BẠN

- Đường dẫn truy cập: `/admin.html` (Ví dụ trên máy: `http://localhost:3000/admin.html`)
- **Mật khẩu mặc định:** `admin` *(Bạn có thể đổi mật khẩu bất kỳ lúc nào trong tab Đổi Mật Khẩu)*.

### Các chức năng trong trang Admin:
1. **💳 Cài đặt Mã QR & Ngân hàng:**
   - Tải lên ảnh chụp mã QR ngân hàng của bạn.
   - Hoặc nhập: Tên ngân hàng, Số tài khoản, Tên chủ thẻ (hệ thống sẽ tự động tạo mã VietQR chuẩn của Napas).
   - Chọn chế độ: **Tự động cho tải file ngay** hoặc **Duyệt thủ công**.
2. **📦 Quản lý LispCAD & Giá tiền:**
   - Bấm **+ Thêm Sản Phẩm Mới**.
   - Điền Tên Lisp, Giá bán (VNĐ), Danh sách lệnh CAD (`TL, STT, DTL...`).
   - Tải file `.lsp`/`.zip` trực tiếp từ máy lên HOẶC dán link Google Drive.
   - Bấm Lưu là trang chủ tự động cập nhật ngay lập tức!
3. **📑 Quản lý Đơn hàng:**
   - Xem ai đã mua, số điện thoại, thời gian và số tiền.

---

## 🚀 CHẠY THỬ TRÊN MÁY TÍNH CỦA BẠN

1. Mở PowerShell hoặc Command Prompt tại thư mục dự án:
   ```bash
   node server.js
   ```
2. Mở trình duyệt vào:
   - Trang bán hàng: `http://localhost:3000`
   - Trang quản trị: `http://localhost:3000/admin.html`

---

## 🌐 ĐƯA LÊN MẠNG INTERNET 100% MIỄN PHÍ

Để website chạy 24/7 trên mạng Internet cho bất kỳ ai truy cập:

### Cách 1: Đưa lên Render.com (Khuyên dùng - Miễn phí 100%)
1. Tải toàn bộ thư mục `lispcad-store` lên tài khoản GitHub của bạn (tạo một repository mới).
2. Truy cập [https://render.com](https://render.com) $\rightarrow$ Đăng ký/Đăng nhập bằng GitHub.
3. Bấm **New +** $\rightarrow$ Chọn **Web Service** $\rightarrow$ Kết nối với repository GitHub bạn vừa tạo.
4. Render sẽ tự động nhận diện Node.js:
   - Build Command: `npm install`
   - Start Command: `node server.js`
5. Bấm **Create Web Service**.
6. Sau khoảng 1-2 phút, Render sẽ cấp cho bạn một đường link miễn phí (ví dụ: `https://lispcad-store.onrender.com`).
   👉 Bạn chỉ việc vào `https://lispcad-store.onrender.com/admin.html` đăng nhập và quản lý toàn bộ cửa hàng!
