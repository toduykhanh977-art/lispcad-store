const express = require("express");
const fs = require("fs");
const path = require("path");
const multer = require("multer");
const cookieParser = require("cookie-parser");

const app = express();
const PORT = process.env.PORT || 3000;
const DB_PATH = path.join(__dirname, "data", "db.json");
const UPLOAD_DIR = path.join(__dirname, "public", "uploads");

// Đảm bảo thư mục lưu trữ tồn tại
if (!fs.existsSync(UPLOAD_DIR)) {
  fs.mkdirSync(UPLOAD_DIR, { recursive: true });
}

// Cấu hình Multer để upload ảnh QR và file Lisp
const storage = multer.diskStorage({
  destination: (req, file, cb) => cb(null, UPLOAD_DIR),
  filename: (req, file, cb) => {
    const ext = path.extname(file.originalname);
    const safeName = Date.now() + "_" + Math.round(Math.random() * 1e4) + ext;
    cb(null, safeName);
  }
});
const upload = multer({ storage });

app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, "public")));
app.use("/files", express.static(path.join(__dirname, "files")));

// Helper đọc/ghi Database JSON
function readDb() {
  try {
    const raw = fs.readFileSync(DB_PATH, "utf8");
    return JSON.parse(raw);
  } catch (e) {
    return {
      adminPassword: "admin",
      paymentSettings: {
        mode: "auto_confirm",
        bankName: "MBBank",
        accountNumber: "9999999999",
        accountHolder: "NGUYEN VAN A",
        qrImageUrl: "",
        transferSyntax: "LISPCAD"
      },
      products: [],
      orders: []
    };
  }
}

function writeDb(data) {
  fs.writeFileSync(DB_PATH, JSON.stringify(data, null, 2), "utf8");
}

// Middleware xác thực Admin
function requireAdmin(req, res, next) {
  const token = req.cookies.admin_auth || req.headers.authorization;
  const db = readDb();
  if (token && token === "auth_" + db.adminPassword) {
    return next();
  }
  return res.status(401).json({ error: "Chưa đăng nhập quyền Admin" });
}

// ==========================================
// 1. API DÀNH CHO KHÁCH HÀNG (PUBLIC)
// ==========================================

// Lấy thông tin cửa hàng & danh sách LispCAD đang bán
app.get("/api/public/store", (req, res) => {
  const db = readDb();
  const activeProducts = (db.products || []).filter(p => p.isActive);
  res.json({
    paymentSettings: {
      mode: db.paymentSettings.mode,
      bankName: db.paymentSettings.bankName,
      accountNumber: db.paymentSettings.accountNumber,
      accountHolder: db.paymentSettings.accountHolder,
      qrImageUrl: db.paymentSettings.qrImageUrl,
      transferSyntax: db.paymentSettings.transferSyntax,
      note: db.paymentSettings.note
    },
    products: activeProducts
  });
});

// Khách chọn mua sản phẩm -> Tạo đơn hàng
app.post("/api/public/create-order", (req, res) => {
  const { productId, customerName, customerPhone } = req.body;
  const db = readDb();
  const product = db.products.find(p => p.id === productId);

  if (!product) {
    return res.status(404).json({ error: "Không tìm thấy sản phẩm này" });
  }

  // Sinh mã đơn 6 số ngẫu nhiên
  const orderCode = "CAD" + Math.floor(100000 + Math.random() * 900000);
  const order = {
    orderCode: orderCode,
    productId: product.id,
    productName: product.name,
    amount: product.price,
    customerName: customerName || "Khách hàng",
    customerPhone: customerPhone || "",
    status: "PENDING", // PENDING hoặc PAID
    createdAt: new Date().toISOString()
  };

  db.orders.unshift(order);
  writeDb(db);

  // Tạo link mã VietQR Động chuẩn Napas (Tự động điền Số tiền + Nội dung chuyển khoản khi khách quét mã)
  let bankCode = (db.paymentSettings.bankName || "MB").trim();
  if (bankCode.toUpperCase().includes("MB")) bankCode = "MB";
  else if (bankCode.toUpperCase().includes("VIETCOM")) bankCode = "VCB";
  else if (bankCode.toUpperCase().includes("TECHCOM")) bankCode = "TCB";

  const accNumber = db.paymentSettings.accountNumber || "0329778016";
  const accName = encodeURIComponent(db.paymentSettings.accountHolder || "TO DUY KHANH");
  const transferContent = `${db.paymentSettings.transferSyntax || "LISPCAD"} ${order.orderCode}`;
  const encodedContent = encodeURIComponent(transferContent);

  // Chuẩn VietQR động của Napas
  let qrUrl = `https://img.vietqr.io/image/${bankCode}-${accNumber}-compact2.png?amount=${product.price}&addInfo=${encodedContent}&accountName=${accName}`;

  // Nếu trong trang quản trị chọn cố định dùng ảnh tĩnh đã upload
  if (db.paymentSettings.qrType === "static" && db.paymentSettings.qrImageUrl) {
    qrUrl = db.paymentSettings.qrImageUrl;
  }

  res.json({
    success: true,
    orderCode: order.orderCode,
    amount: order.amount,
    productName: order.productName,
    qrUrl: qrUrl,
    bankName: db.paymentSettings.bankName || "MBBank",
    accountNumber: accNumber,
    accountHolder: db.paymentSettings.accountHolder || "TO DUY KHANH",
    transferContent: transferContent
  });
});

// Khách kiểm tra / gửi yêu cầu xác nhận
app.post("/api/public/confirm-payment", (req, res) => {
  const { orderCode } = req.body;
  const db = readDb();
  const order = db.orders.find(o => o.orderCode === orderCode);

  if (!order) {
    return res.status(404).json({ error: "Không tìm thấy mã đơn hàng" });
  }

  // Bảo vệ an toàn: Giữ trạng thái đang chờ, chỉ đổi sang PAID khi Webhook ngân hàng báo có tiền hoặc Admin duyệt
  if (order.status !== "PAID") {
    order.status = "PENDING_CONFIRMATION";
    writeDb(db);
  }

  return res.json({
    success: true,
    status: order.status,
    isPaid: order.status === "PAID",
    message: order.status === "PAID" ? "Đã thanh toán thành công!" : "Đang chờ hệ thống ghi nhận giao dịch từ ngân hàng."
  });
});

// Webhook Ngân Hàng Tự Động (Hỗ trợ SePay, Casso, Bot thông báo biến động số dư)
app.post("/api/webhook/bank", (req, res) => {
  try {
    const { content, transferAmount, description } = req.body || {};
    const searchStr = (content || description || JSON.stringify(req.body) || "").toUpperCase();
    const db = readDb();
    
    // Tìm đơn hàng có mã trùng trong nội dung chuyển khoản
    const order = db.orders.find(o => searchStr.includes(o.orderCode.toUpperCase()));
    if (order) {
      order.status = "PAID";
      writeDb(db);
      console.log(`[Webhook] Tự động duyệt thành công đơn hàng: #${order.orderCode} - ${order.productName}`);
      return res.json({ success: true, message: `Đã duyệt đơn #${order.orderCode}` });
    }
    return res.json({ success: false, message: "Không tìm thấy mã đơn hàng phù hợp trong nội dung" });
  } catch (err) {
    console.error("Lỗi webhook:", err);
    return res.status(500).json({ error: "Lỗi xử lý webhook" });
  }
});

// Khách kiểm tra trạng thái đơn hàng
app.get("/api/public/check-order/:orderCode", (req, res) => {
  const { orderCode } = req.params;
  const db = readDb();
  const order = db.orders.find(o => o.orderCode === orderCode);

  if (!order) {
    return res.status(404).json({ error: "Không tìm thấy đơn hàng" });
  }

  const product = db.products.find(p => p.id === order.productId);

  res.json({
    orderCode: order.orderCode,
    status: order.status,
    isPaid: order.status === "PAID",
    productName: order.productName,
    amount: order.amount,
    downloadUrl: order.status === "PAID" ? `/download/${order.orderCode}` : null
  });
});

// Tải file an toàn: Bắt buộc đơn hàng phải là PAID
app.get("/download/:orderCode", (req, res) => {
  const { orderCode } = req.params;
  const db = readDb();
  const order = db.orders.find(o => o.orderCode === orderCode);

  if (!order || order.status !== "PAID") {
    return res.status(403).send("Đơn hàng chưa thanh toán thành công hoặc không hợp lệ.");
  }

  const product = db.products.find(p => p.id === order.productId);
  if (!product) {
    return res.status(404).send("Sản phẩm không tồn tại.");
  }

  // Nếu là link ngoài (Google Drive, OneDrive, v.v.)
  if (product.downloadType === "link" && product.downloadUrl) {
    return res.redirect(product.downloadUrl);
  }

  // Nếu là file upload hoặc file cục bộ
  let filePath = "";
  if (product.downloadUrl.startsWith("/uploads/")) {
    filePath = path.join(UPLOAD_DIR, path.basename(product.downloadUrl));
  } else {
    filePath = path.join(__dirname, "files", path.basename(product.downloadUrl));
  }

  if (fs.existsSync(filePath)) {
    const safeFilename = `${product.name.replace(/[^a-zA-Z0-9_\-\.]/g, "_")}.lsp`;
    res.setHeader("Content-Disposition", `attachment; filename="${safeFilename}"`);
    res.setHeader("Content-Type", "application/octet-stream");
    return fs.createReadStream(filePath).pipe(res);
  } else {
    return res.status(404).send("File LispCAD chưa được đính kèm trên hệ thống.");
  }
});

// ==========================================
// 2. API DÀNH CHO ADMIN QUẢN TRỊ
// ==========================================

// Đăng nhập Admin
app.post("/api/admin/login", (req, res) => {
  const { password } = req.body;
  const db = readDb();

  if (password === db.adminPassword) {
    const token = "auth_" + db.adminPassword;
    res.cookie("admin_auth", token, { httpOnly: true, maxAge: 30 * 24 * 3600 * 1000 });
    return res.json({ success: true, token });
  }
  return res.status(401).json({ error: "Mật khẩu Admin không chính xác" });
});

// Đăng xuất
app.post("/api/admin/logout", (req, res) => {
  res.clearCookie("admin_auth");
  res.json({ success: true });
});

// Lấy toàn bộ dữ liệu quản trị
app.get("/api/admin/data", requireAdmin, (req, res) => {
  const db = readDb();
  res.json({
    paymentSettings: db.paymentSettings,
    products: db.products || [],
    orders: db.orders || []
  });
});

// Cập nhật cấu hình thanh toán & Upload ảnh QR
app.post("/api/admin/payment", requireAdmin, upload.single("qrImage"), (req, res) => {
  const db = readDb();
  const { mode, bankName, accountNumber, accountHolder, transferSyntax, note } = req.body;

  db.paymentSettings.mode = mode || db.paymentSettings.mode || "auto_confirm";
  db.paymentSettings.bankName = bankName || db.paymentSettings.bankName;
  db.paymentSettings.accountNumber = accountNumber || db.paymentSettings.accountNumber;
  db.paymentSettings.accountHolder = accountHolder || db.paymentSettings.accountHolder;
  db.paymentSettings.transferSyntax = transferSyntax || db.paymentSettings.transferSyntax;
  db.paymentSettings.note = note || db.paymentSettings.note;

  // Nếu có upload ảnh QR mới
  if (req.file) {
    db.paymentSettings.qrImageUrl = `/uploads/${req.file.filename}`;
  }

  writeDb(db);
  res.json({ success: true, paymentSettings: db.paymentSettings });
});

// Đổi mật khẩu Admin
app.post("/api/admin/password", requireAdmin, (req, res) => {
  const { newPassword } = req.body;
  if (!newPassword || newPassword.length < 4) {
    return res.status(400).json({ error: "Mật khẩu mới phải có ít nhất 4 ký tự" });
  }
  const db = readDb();
  db.adminPassword = newPassword;
  writeDb(db);

  const token = "auth_" + newPassword;
  res.cookie("admin_auth", token, { httpOnly: true, maxAge: 30 * 24 * 3600 * 1000 });
  res.json({ success: true, message: "Đã đổi mật khẩu thành công!" });
});

// Thêm / Cập nhật sản phẩm LispCAD
app.post("/api/admin/product", requireAdmin, upload.single("lispFile"), (req, res) => {
  const db = readDb();
  const { id, name, price, oldPrice, description, commands, instructions, videoUrl, downloadType, downloadUrl, isActive } = req.body;

  let product = db.products.find(p => p.id === id);

  if (!product) {
    product = {
      id: "prod_" + Date.now(),
      createdAt: new Date().toISOString()
    };
    db.products.unshift(product);
  }

  product.name = name || "LispCAD Mới";
  product.price = Number(price) || 0;
  product.oldPrice = Number(oldPrice) || 0;
  product.description = description || "";
  product.commands = commands || "";
  product.instructions = instructions || "";
  product.videoUrl = videoUrl || "";
  product.downloadType = downloadType || "local";
  product.isActive = isActive === "true" || isActive === true;

  if (downloadType === "link") {
    product.downloadUrl = downloadUrl || "";
  } else if (req.file) {
    product.downloadUrl = `/uploads/${req.file.filename}`;
  } else if (!product.downloadUrl) {
    product.downloadUrl = "/files/AutoCAD_Pro_Toolbox.lsp";
  }

  writeDb(db);
  res.json({ success: true, product });
});

// Xóa sản phẩm
app.delete("/api/admin/product/:id", requireAdmin, (req, res) => {
  const { id } = req.params;
  const db = readDb();
  db.products = db.products.filter(p => p.id !== id);
  writeDb(db);
  res.json({ success: true });
});

// Admin bấm duyệt đơn thủ công
app.post("/api/admin/order/approve", requireAdmin, (req, res) => {
  const { orderCode } = req.body;
  const db = readDb();
  const order = db.orders.find(o => o.orderCode === orderCode);

  if (!order) {
    return res.status(404).json({ error: "Không tìm thấy đơn hàng" });
  }

  order.status = "PAID";
  writeDb(db);
  res.json({ success: true, message: "Đã duyệt đơn hàng thành công!" });
});

// Xóa đơn hàng
app.delete("/api/admin/order/:orderCode", requireAdmin, (req, res) => {
  const { orderCode } = req.params;
  const db = readDb();
  db.orders = db.orders.filter(o => o.orderCode !== orderCode);
  writeDb(db);
  res.json({ success: true });
});

// Khởi chạy Server
app.listen(PORT, () => {
  console.log(`\n======================================================`);
  console.log(`🚀 Trang web đang chạy tại: http://localhost:${PORT}`);
  console.log(`🔑 Trang quản trị Admin: http://localhost:${PORT}/admin.html`);
  console.log(`👉 Mật khẩu Admin mặc định: admin`);
  console.log(`======================================================\n`);
});
