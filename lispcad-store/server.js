const express = require("express");
const fs = require("fs");
const path = require("path");
const multer = require("multer");
const cookieParser = require("cookie-parser");
const nodemailer = require("nodemailer");

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
// Route phục vụ file upload: kiểm tra ổ đĩa local, nếu không có tự kéo từ MongoDB Atlas về
app.get("/uploads/:filename", async (req, res, next) => {
  const filename = req.params.filename;
  const localPath = path.join(UPLOAD_DIR, filename);

  const ext = path.extname(filename).toLowerCase();
  let mimeType = "image/png";
  if (ext === ".jpg" || ext === ".jpeg") mimeType = "image/jpeg";
  else if (ext === ".gif") mimeType = "image/gif";
  else if (ext === ".svg") mimeType = "image/svg+xml";
  else if (ext === ".pdf") mimeType = "application/pdf";
  else if (ext === ".lsp") mimeType = "text/plain";

  if (fs.existsSync(localPath)) {
    res.setHeader("Content-Type", mimeType);
    res.setHeader("Cache-Control", "public, max-age=31536000");
    return fs.createReadStream(localPath).pipe(res);
  }

  if (filesCollection) {
    try {
      const fileDoc = await filesCollection.findOne({ filename });
      if (fileDoc && fileDoc.data) {
        res.setHeader("Content-Type", fileDoc.contentType || fileDoc.mimetype || mimeType);
        res.setHeader("Cache-Control", "public, max-age=31536000");
        try { fs.writeFileSync(localPath, fileDoc.data.buffer || fileDoc.data); } catch (e) {}
        return res.send(fileDoc.data.buffer || fileDoc.data);
      }
    } catch (e) {
      console.error("Lỗi lấy file từ MongoDB:", e.message);
    }
  }

  next();
});

app.use(express.static(path.join(__dirname, "public")));
app.use("/files", express.static(path.join(__dirname, "files")));

// Helper đọc/ghi Database JSON & Đồng bộ Đám mây MongoDB Atlas Vĩnh Viễn
const { MongoClient } = require("mongodb");
const MONGODB_URI = process.env.MONGODB_URI || "mongodb+srv://toduykhanh977_db_user:2odZaj6HuCQI7PzQ@cluster0.vg5hm2a.mongodb.net/lispcad?retryWrites=true&w=majority";

let mongoClient = null;
let mongoDb = null;
let storeCollection = null;
let filesCollection = null;
let dbCache = null;

async function persistFileToMongo(file) {
  if (!file || !filesCollection) return;
  try {
    const dataBuffer = fs.readFileSync(file.path);
    await filesCollection.replaceOne(
      { filename: file.filename },
      {
        filename: file.filename,
        mimetype: file.mimetype || "image/png",
        data: dataBuffer,
        size: file.size,
        uploadedAt: new Date()
      },
      { upsert: true }
    );
    console.log(`☁️ [MongoDB] Đã lưu file vĩnh viễn: ${file.filename}`);
  } catch (err) {
    console.error("Lỗi lưu file lên MongoDB:", err.message);
  }
}

function readDbFromFile() {
  try {
    const raw = fs.readFileSync(DB_PATH, "utf8");
    return JSON.parse(raw);
  } catch (e) {
    return {
      adminPassword: "admin",
      paymentSettings: {
        mode: "strict",
        bankName: "MBBank",
        accountNumber: "0329778016",
        accountHolder: "TO DUY KHANH",
        qrImageUrl: "",
        transferSyntax: "LISPCAD"
      },
      products: [],
      documents: [],
      courses: [],
      orders: []
    };
  }
}

async function initMongo() {
  try {
    mongoClient = new MongoClient(MONGODB_URI, { serverSelectionTimeoutMS: 8000 });
    await mongoClient.connect();
    mongoDb = mongoClient.db("lispcad");
    storeCollection = mongoDb.collection("store_state");
    filesCollection = mongoDb.collection("uploaded_files");
    console.log("🚀 [MongoDB Atlas] Kết nối đám mây thành công!");

    // Đồng bộ dữ liệu mới nhất từ MongoDB Atlas về bộ nhớ cache
    const cloudData = await storeCollection.findOne({ _id: "main_db" });
    if (cloudData) {
      delete cloudData._id;
      dbCache = cloudData;
      try {
        fs.writeFileSync(DB_PATH, JSON.stringify(cloudData, null, 2), "utf8");
      } catch (e) {}
      console.log(`📦 [MongoDB Atlas] Đã nạp ${dbCache.products?.length || 0} sản phẩm và ${dbCache.documents?.length || 0} tài liệu từ đám mây!`);
    } else {
      const local = readDbFromFile();
      await storeCollection.replaceOne({ _id: "main_db" }, { ...local, _id: "main_db" }, { upsert: true });
      dbCache = local;
      console.log("📦 [MongoDB Atlas] Đã khởi tạo dữ liệu ban đầu lên đám mây!");
    }
  } catch (err) {
    console.warn("⚠️ [MongoDB Atlas] Chưa kết nối được đám mây, sử dụng file db.json dự phòng:", err.message);
  }
}

// Khởi chạy kết nối ngay khi nạp file
initMongo();

function readDb() {
  if (dbCache) {
    return JSON.parse(JSON.stringify(dbCache));
  }
  return readDbFromFile();
}

function writeDb(data) {
  dbCache = JSON.parse(JSON.stringify(data));
  // 1. Ghi vào file local làm dự phòng
  try {
    fs.writeFileSync(DB_PATH, JSON.stringify(data, null, 2), "utf8");
  } catch (e) {
    console.error("Lỗi ghi file local:", e);
  }
  // 2. Tự động lưu vĩnh viễn lên đám mây MongoDB Atlas
  if (storeCollection) {
    storeCollection.replaceOne({ _id: "main_db" }, { ...data, _id: "main_db" }, { upsert: true })
      .then(() => console.log("☁️ [MongoDB Atlas] ĐÃ LƯU VĨNH VIỄN LÊN ĐÁM MÂY THÀNH CÔNG!"))
      .catch(err => console.error("❌ [MongoDB Atlas] Lỗi lưu đám mây:", err.message));
  }
}

// API Khôi phục / Nạp toàn bộ dữ liệu từ file backup JSON
app.post("/api/admin/restore-db", requireAdmin, (req, res) => {
  const incoming = req.body;
  if (!incoming || typeof incoming !== "object") {
    return res.status(400).json({ error: "Dữ liệu backup không hợp lệ" });
  }
  const db = readDb();
  if (incoming.products) db.products = incoming.products;
  if (incoming.documents) db.documents = incoming.documents;
  if (incoming.courses) db.courses = incoming.courses;
  if (incoming.lessons) db.lessons = incoming.lessons;
  if (incoming.paymentSettings) db.paymentSettings = incoming.paymentSettings;
  if (incoming.users) db.users = incoming.users;
  writeDb(db);
  res.json({ success: true, message: "Đã khôi phục dữ liệu thành công lên đám mây MongoDB Atlas!" });
});

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
// HỆ THỐNG GỬI EMAIL THÔNG BÁO TỰ ĐỘNG (GMAIL)
// ==========================================
function getEmailConfig(db) {
  const saved = (db && db.emailSettings) ? db.emailSettings : {};
  const notifyEmail = (saved.notifyEmail || process.env.NOTIFY_EMAIL || "khanhksxd30@gmail.com").trim();
  const smtpUser = (saved.smtpUser || process.env.SMTP_USER || notifyEmail).trim();
  const smtpAppPassword = (saved.smtpAppPassword || process.env.SMTP_PASS || "").replace(/\s+/g, "");
  const webhookEmailUrl = (saved.webhookEmailUrl || process.env.EMAIL_WEBHOOK_URL || "").trim();
  return {
    enabled: saved.enabled !== false,
    notifyEmail,
    smtpUser,
    smtpAppPassword,
    webhookEmailUrl,
    notifyOnPaid: saved.notifyOnPaid !== false,
    notifyOnDownload: saved.notifyOnDownload !== false
  };
}

function getOrderItemNames(order) {
  if (order && Array.isArray(order.items) && order.items.length > 0) {
    return order.items.map(it => it.name || it.title || it.id).filter(Boolean).join(", ");
  }
  return (order && order.productName) ? order.productName : "Sản phẩm CAD";
}

function formatVietnamTime(dateInput) {
  try {
    return new Date(dateInput || Date.now()).toLocaleString("vi-VN", { timeZone: "Asia/Ho_Chi_Minh" });
  } catch (e) {
    return new Date().toLocaleString("vi-VN");
  }
}

async function sendAdminNotificationEmail(db, { subject, html }) {
  const cfg = getEmailConfig(db);
  if (!cfg.enabled) {
    return { sent: false, reason: "Thông báo Email đang tắt" };
  }

  // Ưu tiên 1: Gửi qua Google Apps Script Webhook (HTTPS cổng 443 - không bao giờ bị Render Free chặn)
  if (cfg.webhookEmailUrl) {
    try {
      const resp = await fetch(cfg.webhookEmailUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          to: cfg.notifyEmail,
          subject,
          html
        })
      });
      const text = await resp.text();
      if (resp.ok) {
        console.log(`📧 [Email Webhook] Đã gửi thông báo tới ${cfg.notifyEmail}: ${subject}`);
        return { sent: true };
      }
      console.error("❌ [Email Webhook] Lỗi HTTP:", resp.status, text);
      return { sent: false, reason: `Webhook lỗi ${resp.status}: ${text}` };
    } catch (err) {
      console.error("❌ [Email Webhook] Lỗi kết nối:", err.message);
      return { sent: false, reason: err.message };
    }
  }

  // Ưu tiên 2: Gửi qua SMTP Gmail (dành cho localhost hoặc Render gói trả phí)
  if (!cfg.notifyEmail || !cfg.smtpUser || !cfg.smtpAppPassword) {
    console.log("⚠️ [Email] Chưa cấu hình Link Google Apps Script hoặc Mật khẩu ứng dụng Gmail:", subject);
    return { sent: false, reason: "Chưa cấu hình Link Google Apps Script (khuyên dùng trên Render Free)" };
  }

  try {
    const transporter = nodemailer.createTransport({
      host: "smtp.gmail.com",
      port: 465,
      secure: true,
      family: 4,
      connectionTimeout: 10000,
      auth: {
        user: cfg.smtpUser,
        pass: cfg.smtpAppPassword
      }
    });

    await transporter.sendMail({
      from: `"AutoCAD Tin Học Click" <${cfg.smtpUser}>`,
      to: cfg.notifyEmail,
      subject,
      html
    });
    console.log(`📧 [Email] Đã gửi thông báo tới ${cfg.notifyEmail}: ${subject}`);
    return { sent: true };
  } catch (err) {
    console.error("❌ [Email] Lỗi gửi Gmail:", err.message);
    return { sent: false, reason: err.message };
  }
}

function notifyOrderPaid(order, db) {
  const cfg = getEmailConfig(db);
  if (!cfg.notifyOnPaid) return;
  const itemNames = getOrderItemNames(order);
  const itemsListHtml = (order.items && order.items.length > 0)
    ? order.items.map(it => `<li><strong>${it.name || it.title}</strong> — ${Number(it.price || 0).toLocaleString()}đ</li>`).join("")
    : `<li><strong>${order.productName}</strong> — ${Number(order.amount || 0).toLocaleString()}đ</li>`;

  const subject = `[Đã Thanh Toán] ${itemNames} - Đơn #${order.orderCode}`;
  const html = `
    <div style="font-family: Arial, sans-serif; max-width: 620px; margin: 0 auto; border: 1px solid #e2e8f0; border-radius: 10px; overflow: hidden;">
      <div style="background: #0284c7; color: #ffffff; padding: 16px 20px;">
        <h2 style="margin: 0; font-size: 18px;">💰 KHÁCH HÀNG ĐÃ CHUYỂN TIỀN THÀNH CÔNG</h2>
      </div>
      <div style="padding: 20px; color: #0f172a; line-height: 1.6;">
        <p style="margin-top: 0;">Hệ thống vừa ghi nhận thanh toán thành công cho đơn hàng <strong>#${order.orderCode}</strong>:</p>
        <div style="background: #f0f9ff; border-left: 4px solid #0284c7; padding: 12px 15px; margin: 14px 0; border-radius: 4px;">
          <div style="font-size: 13px; color: #0369a1; font-weight: bold; margin-bottom: 4px;">SẢN PHẨM / FILE CAD / TÀI LIỆU KHÁCH MUA:</div>
          <ul style="margin: 4px 0 0 18px; padding: 0;">${itemsListHtml}</ul>
        </div>
        <table style="width: 100%; border-collapse: collapse; font-size: 14px;">
          <tr><td style="padding: 6px 0; color: #64748b; width: 150px;">Tổng số tiền:</td><td style="padding: 6px 0; font-weight: bold; color: #dc2626;">${Number(order.amount || 0).toLocaleString()}đ</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Tên khách hàng:</td><td style="padding: 6px 0; font-weight: bold;">${order.customerName || "Khách hàng"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Số điện thoại:</td><td style="padding: 6px 0;">${order.customerPhone || "Không có"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Email khách:</td><td style="padding: 6px 0;">${order.customerEmail || "Không có"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Hình thức nhận:</td><td style="padding: 6px 0;">${order.deliveryMethod === "zalo" ? "Nhận qua Zalo" : "Tải trực tiếp trên Web"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Thời gian:</td><td style="padding: 6px 0;">${formatVietnamTime()}</td></tr>
        </table>
      </div>
    </div>
  `;
  sendAdminNotificationEmail(db, { subject, html });
}

function notifyOrderDownloaded(order, product, db) {
  const cfg = getEmailConfig(db);
  if (!cfg.notifyOnDownload) return;
  const itemNames = getOrderItemNames(order);
  const downloadedTitle = (product && (product.name || product.title)) ? (product.name || product.title) : itemNames;
  const totalDl = product ? (Number(product.downloadCount) || 1) : 1;

  const subject = `[Khách Đã Tải File] ${downloadedTitle} - Đơn #${order.orderCode}`;
  const html = `
    <div style="font-family: Arial, sans-serif; max-width: 620px; margin: 0 auto; border: 1px solid #e2e8f0; border-radius: 10px; overflow: hidden;">
      <div style="background: #16a34a; color: #ffffff; padding: 16px 20px;">
        <h2 style="margin: 0; font-size: 18px;">📥 KHÁCH HÀNG VỪA TẢI FILE ĐÃ MUA VỀ</h2>
      </div>
      <div style="padding: 20px; color: #0f172a; line-height: 1.6;">
        <p style="margin-top: 0;">Khách hàng của đơn <strong>#${order.orderCode}</strong> vừa bấm tải file về máy:</p>
        <div style="background: #f0fdf4; border-left: 4px solid #16a34a; padding: 12px 15px; margin: 14px 0; border-radius: 4px;">
          <div style="font-size: 13px; color: #15803d; font-weight: bold; margin-bottom: 4px;">TÊN FILE CAD / LISP / TÀI LIỆU VỪA TẢI:</div>
          <div style="font-size: 16px; font-weight: bold; color: #0f172a;">${downloadedTitle}</div>
          ${order.items && order.items.length > 1 ? `<div style="font-size: 13px; color: #475569; margin-top: 6px;">Đơn gồm: ${itemNames}</div>` : ""}
        </div>
        <table style="width: 100%; border-collapse: collapse; font-size: 14px;">
          <tr><td style="padding: 6px 0; color: #64748b; width: 150px;">Mã đơn hàng:</td><td style="padding: 6px 0; font-weight: bold;">#${order.orderCode}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Tên khách hàng:</td><td style="padding: 6px 0; font-weight: bold;">${order.customerName || "Khách hàng"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Số điện thoại:</td><td style="padding: 6px 0;">${order.customerPhone || "Không có"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Email khách:</td><td style="padding: 6px 0;">${order.customerEmail || "Không có"}</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Tổng lượt tải SP:</td><td style="padding: 6px 0; font-weight: bold; color: #0284c7;">${totalDl.toLocaleString()} lượt</td></tr>
          <tr><td style="padding: 6px 0; color: #64748b;">Thời gian tải:</td><td style="padding: 6px 0;">${formatVietnamTime()}</td></tr>
        </table>
      </div>
    </div>
  `;
  sendAdminNotificationEmail(db, { subject, html });
}

// Helper tự động kích hoạt Khóa học khi đơn hàng được thanh toán (PAID)
function completeOrderPaid(order, db) {
  const wasPaid = order.status === "PAID";
  order.status = "PAID";
  if (!wasPaid) {
    notifyOrderPaid(order, db);
  }
  const hasCourse = (order.items && order.items.some(it => it.id === "prod_course_2d")) || order.productId === "prod_course_2d";
  if (hasCourse) {
    if (!db.users) db.users = [];
    let user = null;
    if (order.userId) {
      user = db.users.find(u => u.id === order.userId);
    }
    if (!user && order.customerPhone) {
      user = db.users.find(u => u.phone === order.customerPhone);
    }
    if (!user && order.customerEmail) {
      user = db.users.find(u => u.email === order.customerEmail);
    }

    if (user) {
      if (!user.enrolledCourses) user.enrolledCourses = [];
      if (!user.enrolledCourses.includes("prod_course_2d")) {
        user.enrolledCourses.push("prod_course_2d");
      }
      console.log(`[AutoCAD 2D] Đã tự động kích hoạt khóa học cho học viên: ${user.name} (${user.email || user.phone})`);
    } else {
      const newUser = {
        id: "usr_" + Date.now(),
        name: order.customerName || "Học viên AutoCAD",
        email: order.customerEmail || "",
        phone: order.customerPhone || "",
        password: order.customerPhone ? order.customerPhone.slice(-6) : "123456",
        role: "Học viên",
        enrolledCourses: ["prod_course_2d"],
        createdAt: new Date().toISOString()
      };
      db.users.push(newUser);
      console.log(`[AutoCAD 2D] Tự động tạo tài khoản học viên: ${newUser.phone || newUser.name}`);
    }
  }
}

// ==========================================
// 1. API DÀNH CHO KHÁCH HÀNG (PUBLIC)
// ==========================================

// ==========================================
// API CLOUD FONT SERVER CHO FONTFIX / FONTSYNC (LỆNH CF)
// ==========================================
let cachedFontIndex = null;
function getFontIndex() {
  if (cachedFontIndex) return cachedFontIndex;
  const idxPath = path.join(__dirname, "data", "font-index.json");
  if (fs.existsSync(idxPath)) {
    try {
      cachedFontIndex = JSON.parse(fs.readFileSync(idxPath, "utf8"));
      return cachedFontIndex;
    } catch (e) {
      console.error("❌ Lỗi đọc font-index.json:", e.message);
    }
  }
  return {};
}

app.get("/api/fonts", (req, res) => {
  const fontIndex = getFontIndex();
  const rawName = String(req.query.name || "").trim();
  if (!rawName) {
    return res.json({ ok: true, totalKeys: Object.keys(fontIndex).length });
  }

  const keyExact = rawName.toLowerCase();
  const keyBase = keyExact.replace(/\.(shx|ttf|ttc|otf)$/i, "");
  const relPath = fontIndex[keyExact] || fontIndex[keyBase];

  if (!relPath) {
    return res.status(404).send("Font not found: " + rawName);
  }

  const cdnUrl = "https://raw.githubusercontent.com/toduykhanh977-art/autocad-fonts-cloud/main/" +
    relPath.split("/").map(encodeURIComponent).join("/");
  return res.redirect(302, cdnUrl);
});

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
      zaloLink: db.paymentSettings.zaloLink || "https://zalo.me/0329778016",
      facebookLink: db.paymentSettings.facebookLink || "https://www.facebook.com/",
      supportPhone: db.paymentSettings.supportPhone || "0329778016",
      note: db.paymentSettings.note
    },
    products: activeProducts,
    documents: db.documents || []
  });
});

// ==========================================
// HỆ THỐNG MÃ GIẢM GIÁ (COUPON / VOUCHER)
// ==========================================
function evaluateCoupon(db, rawCode, originalAmount) {
  const code = String(rawCode || "").trim().toUpperCase();
  if (!code) return { valid: false, message: "Vui lòng nhập mã giảm giá!" };
  const coupon = (db.coupons || []).find(c => String(c.code || "").toUpperCase() === code);
  if (!coupon) return { valid: false, message: "Mã giảm giá không tồn tại!" };
  if (coupon.isActive === false) return { valid: false, message: "Mã giảm giá này đang tạm khóa!" };
  const maxUses = Number(coupon.maxUses) || 0;
  const usedCount = Number(coupon.usedCount) || 0;
  if (maxUses > 0 && usedCount >= maxUses) {
    return { valid: false, message: "Mã giảm giá này đã hết lượt sử dụng!" };
  }

  const base = Math.max(0, Number(originalAmount) || 0);
  let discountAmount = 0;
  if (coupon.type === "percent") {
    const pct = Math.min(100, Math.max(0, Number(coupon.value) || 0));
    discountAmount = Math.round((base * pct) / 100);
  } else {
    discountAmount = Math.min(base, Math.max(0, Number(coupon.value) || 0));
  }
  const finalAmount = Math.max(0, base - discountAmount);
  const label = coupon.type === "percent"
    ? `Giảm ${Number(coupon.value)}%`
    : `Giảm ${Number(coupon.value).toLocaleString()}đ`;
  return { valid: true, coupon, discountAmount, finalAmount, label };
}

app.post("/api/public/validate-coupon", (req, res) => {
  const { code, amount } = req.body;
  const db = readDb();
  const result = evaluateCoupon(db, code, amount);
  if (!result.valid) {
    return res.status(400).json({ valid: false, message: result.message });
  }
  res.json({
    valid: true,
    code: result.coupon.code,
    label: result.label,
    discountAmount: result.discountAmount,
    finalAmount: result.finalAmount,
    message: `Áp dụng thành công (${result.label})!`
  });
});

// Khách chọn mua sản phẩm hoặc mua giỏ hàng -> Tạo đơn hàng
app.post("/api/public/create-order", (req, res) => {
  const { productId, cartItems, customerName, customerPhone, customerEmail, userId, deliveryMethod, couponCode } = req.body;
  const db = readDb();

  let orderItems = [];
  let totalAmount = 0;
  let orderTitle = "";
  const allAvailableItems = [...(db.products || []), ...(db.documents || [])];

  if (cartItems && Array.isArray(cartItems) && cartItems.length > 0) {
    // Thanh toán toàn bộ giỏ hàng
    orderItems = cartItems.map(item => {
      const prod = allAvailableItems.find(p => p.id === item.id) || item;
      return {
        id: prod.id,
        name: prod.name || prod.title,
        price: Number(prod.price) || 0,
        downloadUrl: prod.downloadUrl || (prod.files && prod.files[0] ? prod.files[0].url : ""),
        downloadType: prod.downloadType || (prod.files && prod.files[0] ? prod.files[0].type : "link")
      };
    });
    totalAmount = orderItems.reduce((sum, it) => sum + it.price, 0);
    orderTitle = orderItems.length === 1 ? orderItems[0].name : `Giỏ hàng (${orderItems.length} sản phẩm & tài liệu CAD)`;
  } else {
    // Mua lẻ 1 sản phẩm / tài liệu
    let product = allAvailableItems.find(p => p.id === productId);
    if (!product && (productId === "prod_course_2d" || productId === "course_2d")) {
      product = allAvailableItems.find(p => p.id === "prod_course_2d" || p.category === "Khóa học" || (p.name && p.name.includes("Khóa học")));
    }
    if (!product && (productId === "prod_course_2d" || productId === "course_2d")) {
      product = {
        id: "prod_course_2d",
        name: "Khóa học AutoCAD 2D Từ Cơ Bản Đến Nâng Cao",
        price: 299000
      };
    }
    if (!product) {
      return res.status(404).json({ error: "Không tìm thấy sản phẩm này" });
    }
    orderItems = [{
      id: product.id,
      name: product.name || product.title,
      price: Number(product.price) || 0,
      downloadUrl: product.downloadUrl || (product.files && product.files[0] ? product.files[0].url : ""),
      downloadType: product.downloadType || (product.files && product.files[0] ? product.files[0].type : "link")
    }];
    totalAmount = Number(product.price) || 0;
    orderTitle = product.name || product.title;
  }

  // Áp dụng mã giảm giá nếu khách có nhập
  let appliedCouponCode = "";
  let discountAmount = 0;
  if (couponCode && String(couponCode).trim()) {
    const check = evaluateCoupon(db, couponCode, totalAmount);
    if (!check.valid) {
      return res.status(400).json({ error: check.message });
    }
    appliedCouponCode = check.coupon.code;
    discountAmount = check.discountAmount;
    totalAmount = check.finalAmount;
    check.coupon.usedCount = (Number(check.coupon.usedCount) || 0) + 1;
  }

  // Sinh mã đơn theo Tên khách (không dấu) + 4 số cuối SĐT (VD: TO DUY KHANH + 0329778016 => KHANH8016)
  const cleanNameParts = String(customerName || "")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/đ/g, "d")
    .replace(/Đ/g, "D")
    .toUpperCase()
    .trim()
    .split(/\s+/)
    .map(w => w.replace(/[^A-Z0-9]/g, ""))
    .filter(Boolean);
  const firstName = cleanNameParts.length > 0 ? cleanNameParts[cleanNameParts.length - 1] : "CAD";
  const digits = String(customerPhone || "").replace(/\D/g, "");
  const last4 = digits.length >= 4 ? digits.slice(-4) : String(Math.floor(1000 + Math.random() * 9000));
  const baseCode = `${firstName}${last4}`;
  let orderCode = baseCode;
  let suffix = 2;
  while ((db.orders || []).some(o => String(o.orderCode || "").toUpperCase() === orderCode)) {
    orderCode = `${baseCode}${suffix}`;
    suffix++;
  }
  const order = {
    orderCode: orderCode,
    productId: orderItems[0].id,
    productName: orderTitle,
    items: orderItems,
    amount: totalAmount,
    discountAmount: discountAmount,
    couponCode: appliedCouponCode || null,
    userId: userId || null,
    customerName: customerName || "Khách hàng",
    customerPhone: customerPhone || "",
    customerEmail: customerEmail || "",
    deliveryMethod: deliveryMethod === "zalo" ? "zalo" : "download",
    status: "PENDING", // PENDING hoặc PAID
    createdAt: new Date().toISOString()
  };

  // Nếu sau khi áp mã giảm giá tổng tiền = 0đ -> tự động duyệt PAID và mở file ngay lập tức!
  if (totalAmount <= 0) {
    completeOrderPaid(order, db);
    db.orders.unshift(order);
    writeDb(db);
    return res.json({
      success: true,
      isPaid: true,
      orderCode: order.orderCode,
      amount: 0,
      productName: order.productName
    });
  }

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
  let qrUrl = `https://img.vietqr.io/image/${bankCode}-${accNumber}-compact2.png?amount=${totalAmount}&addInfo=${encodedContent}&accountName=${accName}`;

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
      completeOrderPaid(order, db);
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

  const isPaid = order.status === "PAID";
  let requireLicense = false;
  const allItems = [...(db.products || []), ...(db.documents || [])];
  if (order.items && Array.isArray(order.items) && order.items.length > 0) {
    requireLicense = order.items.some(it => {
      const dbIt = allItems.find(x => x.id === it.id);
      return dbIt && dbIt.requireLicense === true;
    });
  } else {
    const prod = allItems.find(x => x.id === order.productId);
    requireLicense = !!(prod && prod.requireLicense === true);
  }

  if (isPaid) {
    const hasCourse = (order.items && order.items.some(it => it.id === "prod_course_2d")) || order.productId === "prod_course_2d";
    if (hasCourse && !req.session.user) {
      const user = (db.users || []).find(u => (order.userId && u.id === order.userId) || (order.customerPhone && u.phone === order.customerPhone) || (order.customerEmail && u.email === order.customerEmail));
      if (user) {
        req.session.user = { id: user.id, name: user.name, phone: user.phone, email: user.email, role: user.role, enrolledCourses: user.enrolledCourses };
      }
    }
  }

  let downloadFiles = [];
  if (isPaid) {
    const prod = allItems.find(p => p.id === order.productId);
    if (prod && Array.isArray(prod.files) && prod.files.length > 0) {
      downloadFiles = prod.files.map((f, idx) => ({
        name: f.name || `File ${idx + 1}`,
        url: `/download/${order.orderCode}?fileIndex=${idx}`
      }));
    } else if (prod) {
      downloadFiles = [{ name: prod.name || prod.title || order.productName, url: `/download/${order.orderCode}` }];
    }
  }

  res.json({
    orderCode: order.orderCode,
    status: order.status,
    isPaid: isPaid,
    productName: order.productName,
    amount: order.amount,
    customerName: order.customerName,
    customerPhone: order.customerPhone,
    deliveryMethod: order.deliveryMethod || "download",
    requireLicense: requireLicense,
    downloadUrl: isPaid ? `/download/${order.orderCode}` : null,
    downloadFiles,
    adminZalo: (db.paymentSettings && db.paymentSettings.zaloLink) || "https://zalo.me/0329778016"
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

  const allAvailableItems = [...(db.products || []), ...(db.documents || [])];
  const product = allAvailableItems.find(p => p.id === order.productId);
  if (!product) {
    return res.status(404).send("Sản phẩm không tồn tại.");
  }

  // Tăng lượt tải của sản phẩm hoặc các món trong đơn hàng
  product.downloadCount = (Number(product.downloadCount) || 0) + 1;
  if (order.items && Array.isArray(order.items)) {
    order.items.forEach(it => {
      const dbIt = allAvailableItems.find(p => p.id === it.id);
      if (dbIt && dbIt !== product) {
        dbIt.downloadCount = (Number(dbIt.downloadCount) || 0) + 1;
      }
    });
  }
  writeDb(db);
  notifyOrderDownloaded(order, product, db);

  // Lấy link tải từ fileIndex (nếu có nhiều file như PDF + RAR) hoặc file đầu tiên
  const fileIndex = req.query.fileIndex !== undefined ? parseInt(req.query.fileIndex) : 0;
  const chosenFile = (product.files && product.files.length > 0)
    ? (product.files[fileIndex] || product.files[0])
    : null;
  const dlUrl = (chosenFile && chosenFile.url) ? chosenFile.url : (product.downloadUrl || "");
  const dlType = (chosenFile && chosenFile.type) ? chosenFile.type : (product.downloadType || "link");

  // Nếu là link ngoài (Google Drive, OneDrive, v.v.)
  if (dlType === "link" && dlUrl) {
    let targetUrl = dlUrl.trim();
    // Tự động chuyển link Google Drive /view thành link tải trực tiếp (Direct Download)
    const driveMatch = targetUrl.match(/\/file\/d\/([a-zA-Z0-9_-]+)/);
    if (driveMatch && driveMatch[1]) {
      targetUrl = `https://drive.google.com/uc?export=download&id=${driveMatch[1]}`;
    }
    return res.redirect(targetUrl);
  }

  // Nếu là file upload hoặc file cục bộ
  let filePath = "";
  if (dlUrl && dlUrl.startsWith("/uploads/")) {
    filePath = path.join(UPLOAD_DIR, path.basename(dlUrl));
  } else if (dlUrl) {
    filePath = path.join(__dirname, "files", path.basename(dlUrl));
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

// API ghi nhận lượt tải cho sản phẩm / tài liệu (khi khách bấm tải file)
app.post("/api/public/track-download", (req, res) => {
  const { id } = req.body;
  if (!id) return res.status(400).json({ error: "Thiếu ID" });
  const db = readDb();
  const allAvailableItems = [...(db.products || []), ...(db.documents || [])];
  const item = allAvailableItems.find(x => x.id === id);
  if (item) {
    item.downloadCount = (Number(item.downloadCount) || 0) + 1;
    writeDb(db);
    return res.json({ success: true, downloadCount: item.downloadCount });
  }
  res.status(404).json({ error: "Không tìm thấy" });
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
    emailSettings: getEmailConfig(db),
    products: db.products || [],
    documents: db.documents || [],
    orders: db.orders || [],
    lessons: db.lessons || [],
    users: db.users || [],
    coupons: db.coupons || []
  });
});

// Tạo / Cập nhật Mã Giảm Giá
app.post("/api/admin/coupon", requireAdmin, (req, res) => {
  const { code, type, value, maxUses, note } = req.body;
  const cleanCode = String(code || "").trim().toUpperCase().replace(/\s+/g, "");
  if (!cleanCode) return res.status(400).json({ error: "Vui lòng nhập mã giảm giá!" });

  const db = readDb();
  if (!db.coupons) db.coupons = [];
  let coupon = db.coupons.find(c => c.code === cleanCode);
  if (!coupon) {
    coupon = {
      code: cleanCode,
      usedCount: 0,
      isActive: true,
      createdAt: new Date().toISOString()
    };
    db.coupons.unshift(coupon);
  }
  coupon.type = type === "fixed" ? "fixed" : "percent";
  coupon.value = Math.max(0, Number(value) || 0);
  coupon.maxUses = Math.max(0, parseInt(maxUses) || 0);
  coupon.note = note || "";

  writeDb(db);
  res.json({ success: true, coupon });
});

// Bật / Tắt Mã Giảm Giá
app.post("/api/admin/coupon/:code/toggle", requireAdmin, (req, res) => {
  const code = String(req.params.code || "").toUpperCase();
  const db = readDb();
  const coupon = (db.coupons || []).find(c => c.code === code);
  if (!coupon) return res.status(404).json({ error: "Không tìm thấy mã" });
  coupon.isActive = coupon.isActive === false ? true : false;
  writeDb(db);
  res.json({ success: true, isActive: coupon.isActive });
});

// Xóa Mã Giảm Giá
app.delete("/api/admin/coupon/:code", requireAdmin, (req, res) => {
  const code = String(req.params.code || "").toUpperCase();
  const db = readDb();
  db.coupons = (db.coupons || []).filter(c => c.code !== code);
  writeDb(db);
  res.json({ success: true });
});

// Cập nhật cấu hình Email thông báo Gmail
app.post("/api/admin/email-settings", requireAdmin, (req, res) => {
  const db = readDb();
  const { enabled, notifyEmail, smtpUser, smtpAppPassword, webhookEmailUrl, notifyOnPaid, notifyOnDownload } = req.body;
  if (!db.emailSettings) db.emailSettings = {};

  db.emailSettings.enabled = enabled !== false && enabled !== "false";
  db.emailSettings.notifyEmail = (notifyEmail || "khanhksxd30@gmail.com").trim();
  db.emailSettings.smtpUser = (smtpUser || db.emailSettings.notifyEmail).trim();
  if (webhookEmailUrl !== undefined) {
    db.emailSettings.webhookEmailUrl = String(webhookEmailUrl).trim();
  }
  if (smtpAppPassword !== undefined && smtpAppPassword !== "") {
    db.emailSettings.smtpAppPassword = String(smtpAppPassword).replace(/\s+/g, "");
  }
  db.emailSettings.notifyOnPaid = notifyOnPaid !== false && notifyOnPaid !== "false";
  db.emailSettings.notifyOnDownload = notifyOnDownload !== false && notifyOnDownload !== "false";

  writeDb(db);
  res.json({ success: true, emailSettings: getEmailConfig(db) });
});

// Gửi Email Test thử nghiệm từ Admin
app.post("/api/admin/email-test", requireAdmin, async (req, res) => {
  const db = readDb();
  const cfg = getEmailConfig(db);
  if (!cfg.webhookEmailUrl && !cfg.smtpAppPassword) {
    return res.status(400).json({
      success: false,
      error: "Chưa dán Link Google Apps Script (hoặc Mật khẩu ứng dụng Gmail). Vui lòng nhập và bấm Lưu cấu hình trước khi test!"
    });
  }
  const result = await sendAdminNotificationEmail(db, {
    subject: "[Kiểm Tra Kết Nối] Hệ thống thông báo đơn hàng & lượt tải đã hoạt động!",
    html: `
      <div style="font-family: Arial, sans-serif; padding: 20px; border: 1px solid #cbd5e1; border-radius: 8px; max-width: 560px;">
        <h2 style="color: #0284c7; margin-top: 0;">✅ Kết nối Gmail thành công!</h2>
        <p>Từ bây giờ, mỗi khi có khách hàng:</p>
        <ul>
          <li><strong>Chuyển tiền thành công</strong> (tự động hoặc duyệt tay)</li>
          <li><strong>Bấm tải file đã mua</strong> về máy</li>
        </ul>
        <p>Hệ thống sẽ tự động gửi thông báo kèm <strong>tên chính xác sản phẩm / File CAD / Tài liệu</strong> về địa chỉ <strong>${cfg.notifyEmail}</strong>.</p>
        <p style="color: #64748b; font-size: 12px;">Thời gian test: ${formatVietnamTime()}</p>
      </div>
    `
  });
  if (result.sent) {
    return res.json({ success: true, message: `Đã gửi email test thành công tới ${cfg.notifyEmail}!` });
  }
  return res.status(500).json({ success: false, error: `Không gửi được email: ${result.reason}` });
});

// Cập nhật cấu hình thanh toán & Upload ảnh QR
app.post("/api/admin/payment", requireAdmin, upload.single("qrImage"), (req, res) => {
  const db = readDb();
  const { mode, bankName, accountNumber, accountHolder, transferSyntax, zaloLink, facebookLink, supportPhone, note } = req.body;

  db.paymentSettings.mode = mode || db.paymentSettings.mode || "auto_confirm";
  db.paymentSettings.bankName = bankName || db.paymentSettings.bankName;
  db.paymentSettings.accountNumber = accountNumber || db.paymentSettings.accountNumber;
  db.paymentSettings.accountHolder = accountHolder || db.paymentSettings.accountHolder;
  db.paymentSettings.transferSyntax = transferSyntax || db.paymentSettings.transferSyntax;
  db.paymentSettings.zaloLink = zaloLink || db.paymentSettings.zaloLink || "https://zalo.me/0329778016";
  db.paymentSettings.facebookLink = facebookLink || db.paymentSettings.facebookLink || "https://www.facebook.com/";
  db.paymentSettings.supportPhone = supportPhone || db.paymentSettings.supportPhone || "0329778016";
  db.paymentSettings.note = note || db.paymentSettings.note;

  // Nếu có upload ảnh QR mới
  if (req.file) {
    db.paymentSettings.qrImageUrl = `/uploads/${req.file.filename}`;
    persistFileToMongo(req.file);
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

// Thêm / Cập nhật sản phẩm LispCAD (hỗ trợ file Lisp và nhiều ảnh minh họa/GIF)
app.post("/api/admin/product", requireAdmin, upload.fields([
  { name: "lispFile", maxCount: 1 },
  { name: "imageFile", maxCount: 20 },
  { name: "imageFiles", maxCount: 20 },
  { name: "prodImages", maxCount: 20 }
]), (req, res) => {
  const db = readDb();
  const { id, name, category, price, oldPrice, description, commands, instructions, videoUrl, downloadType, downloadUrl, isActive, imageUrl, existingImages } = req.body;

  let product = db.products.find(p => p.id === id);

  if (!product) {
    product = {
      id: id || ("prod_" + Date.now()),
      createdAt: new Date().toISOString()
    };
    db.products.unshift(product);
  }

  product.name = name || "LispCAD Mới";
  product.category = category || "Vẽ nhanh";
  product.price = Number(price) || 0;
  product.oldPrice = Number(oldPrice) || 0;
  product.description = description || "";
  product.commands = commands || "";
  product.instructions = instructions || "";
  product.videoUrl = videoUrl || "";
  product.downloadType = downloadType || "local";
  product.requireLicense = (req.body.requireLicense === "true" || req.body.requireLicense === true);
  product.isActive = isActive === "true" || isActive === true;
  if (req.body.downloadCount !== undefined && req.body.downloadCount !== "") {
    product.downloadCount = parseInt(req.body.downloadCount) || 0;
  } else if (product.downloadCount === undefined) {
    product.downloadCount = 0;
  }

  // Xử lý ảnh minh họa / GIF (Hỗ trợ nhiều ảnh + giữ ảnh cũ)
  let parsedExistingImages = [];
  try {
    if (existingImages) parsedExistingImages = JSON.parse(existingImages);
  } catch (e) {
    parsedExistingImages = [];
  }
  if (!Array.isArray(parsedExistingImages)) parsedExistingImages = [];

  const newUploadedImages = [];
  const imageFieldNames = ['imageFile', 'imageFiles', 'prodImages'];
  imageFieldNames.forEach(field => {
    if (req.files && req.files[field]) {
      const fls = Array.isArray(req.files[field]) ? req.files[field] : [req.files[field]];
      fls.forEach(f => {
        newUploadedImages.push(`/uploads/${f.filename}`);
        persistFileToMongo(f);
      });
    }
  });

  const combinedImages = [...parsedExistingImages, ...newUploadedImages];
  const manualImageUrl = (imageUrl || "").trim();
  if (manualImageUrl && !combinedImages.includes(manualImageUrl)) {
    combinedImages.push(manualImageUrl);
  }

  // Cập nhật danh sách ảnh images và ảnh đại diện chính imageUrl
  if (combinedImages.length > 0) {
    product.images = combinedImages;
    product.imageUrl = combinedImages[0];
  } else if (imageUrl !== undefined) {
    product.imageUrl = manualImageUrl;
    product.images = manualImageUrl ? [manualImageUrl] : [];
  } else if (!product.images) {
    product.images = product.imageUrl ? [product.imageUrl] : [];
  }

  // Xử lý file tải về cho khách hàng
  if (downloadType === "link") {
    product.downloadUrl = downloadUrl || "";
  } else if (req.files && req.files['lispFile'] && req.files['lispFile'][0]) {
    const lspFile = req.files['lispFile'][0];
    product.downloadUrl = `/uploads/${lspFile.filename}`;
    persistFileToMongo(lspFile);
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

// Thêm / Cập nhật Tài liệu & Giáo trình CAD (Hỗ trợ 2-3 ảnh minh họa + nhiều file & link Google Drive)
app.post("/api/admin/document", requireAdmin, upload.any(), (req, res) => {
  const db = readDb();
  if (!db.documents) db.documents = [];
  const { id, title, itemType, category, format, size, author, description, isFree, price, oldPrice, previewPages, previewAll, existingImages, filesJson } = req.body;

  let doc = db.documents.find(d => d.id === id);
  if (!doc) {
    doc = {
      id: "doc_" + Date.now(),
      createdAt: new Date().toISOString()
    };
    db.documents.unshift(doc);
  }

  doc.title = title || "Tài liệu AutoCAD";
  doc.itemType = (itemType === "filecad" || category === "File CAD") ? "filecad" : "document";
  doc.category = category || (doc.itemType === "filecad" ? "File CAD" : "Giáo trình");
  doc.format = format || "PDF";
  doc.size = size || "10 MB";
  doc.author = author || "Tin Học Click";
  doc.description = description || "";
  if (req.body.downloadCount !== undefined && req.body.downloadCount !== "") {
    doc.downloadCount = parseInt(req.body.downloadCount) || 0;
  } else if (doc.downloadCount === undefined) {
    doc.downloadCount = 0;
  }

  // Giới hạn số trang đọc thử (-1 = xem hết toàn bộ trang, mặc định 10 trang)
  const rawPreview = parseInt(req.body.previewPages);
  if (previewAll === "true" || previewAll === true || rawPreview === -1) {
    doc.previewPages = -1;
  } else {
    doc.previewPages = rawPreview > 0 ? rawPreview : 10;
  }

  // Giá bán & Thiết lập có phí / miễn phí
  const isFreeFlag = (isFree === 'true' || isFree === true || Number(price) === 0);
  doc.isFree = isFreeFlag;
  doc.price = isFreeFlag ? 0 : (Number(price) || 0);
  doc.oldPrice = (!isFreeFlag && oldPrice) ? Number(oldPrice) : null;

  // 1. Xử lý danh sách ảnh minh họa (2 - 3 ảnh)
  let parsedExistingImages = [];
  try {
    if (existingImages) parsedExistingImages = JSON.parse(existingImages);
  } catch (e) {
    parsedExistingImages = [];
  }

  if (req.files && Array.isArray(req.files)) {
    req.files.forEach(f => persistFileToMongo(f));
  }

  const uploadedImageFiles = (req.files || []).filter(f => f.fieldname === 'docImages' || (f.mimetype && f.mimetype.startsWith('image/')));
  const newImageUrls = uploadedImageFiles.map(f => `/uploads/${f.filename}`);
  doc.images = [...parsedExistingImages, ...newImageUrls];

  // 2. Xử lý danh sách nhiều File & Link Google Drive
  let parsedFiles = [];
  try {
    if (filesJson) parsedFiles = JSON.parse(filesJson);
  } catch (e) {
    parsedFiles = [];
  }

  // Khớp các file upload mới từ máy tính với fileKey tương ứng
  const uploadedDocFiles = (req.files || []).filter(f => f.fieldname.startsWith('file_upload_') || f.fieldname === 'docFile');
  parsedFiles = parsedFiles.map((item, idx) => {
    if (item.type === 'local') {
      const matchFile = uploadedDocFiles.find(f => f.fieldname === item.fileKey || f.fieldname === `file_upload_${idx}`);
      if (matchFile) {
        item.url = `/uploads/${matchFile.filename}`;
      }
    }
    return item;
  });

  doc.files = parsedFiles;

  // Cập nhật downloadUrl & downloadType chính (để tương thích ngược)
  if (doc.files.length > 0) {
    doc.downloadUrl = doc.files[0].url || "";
    doc.downloadType = doc.files[0].type || "link";
  } else if (req.body.downloadUrl) {
    doc.downloadUrl = req.body.downloadUrl || "";
    doc.downloadType = req.body.downloadType || "link";
    doc.files = [{ id: 'f_' + Date.now(), name: doc.title, url: doc.downloadUrl, type: doc.downloadType, size: doc.size }];
  }

  writeDb(db);
  res.json({ success: true, document: doc });
});

// Helper tìm URL Google Drive của file PDF (ưu tiên file PDF, bỏ qua file RAR/ZIP)
function findPdfDriveUrl(doc) {
  if (doc.files && doc.files.length > 0) {
    const driveFiles = doc.files.filter(f => f.url && f.url.includes("drive.google.com"));
    if (driveFiles.length > 0) {
      const explicitPdf = driveFiles.find(f =>
        (f.name && f.name.toLowerCase().includes("pdf")) ||
        (f.url && f.url.toLowerCase().includes(".pdf"))
      );
      if (explicitPdf) return explicitPdf.url;
      const nonArchive = driveFiles.find(f => {
        const n = (f.name || "").toLowerCase();
        return !n.includes(".rar") && !n.includes(".zip") && !n.includes("rar") && !n.includes(".dwg");
      });
      if (nonArchive) return nonArchive.url;
      return driveFiles[0].url;
    }
  }
  if (doc.downloadUrl && doc.downloadUrl.includes("drive.google.com")) {
    return doc.downloadUrl;
  }
  return null;
}

// ==============================================================
// API CẮT & XEM TRƯỚC GIỚI HẠN SỐ TRANG CHO FILE PDF (PREVIEW LIMIT)
// Khách chỉ được xem N trang đầu (hoặc xem hết nếu previewPages === -1)
// ==============================================================
app.get("/api/public/pdf-preview-slice/:docId", async (req, res) => {
  try {
    const { docId } = req.params;
    const db = readDb();
    const doc = (db.documents || []).find(d => d.id === docId);
    if (!doc) {
      return res.status(404).send("Không tìm thấy tài liệu");
    }

    const { PDFDocument, StandardFonts, rgb } = require("pdf-lib");
    const isFullPreview = Number(doc.previewPages) === -1;
    const previewPagesLimit = isFullPreview ? 99999 : Math.max(1, parseInt(doc.previewPages) || 10);

    // Tìm file PDF nguồn
    let sourceBuffer = null;
    let foundPath = null;

    if (doc.files && doc.files.length > 0) {
      const pdfFile = doc.files.find(f => (f.name && f.name.toLowerCase().includes('.pdf')) || (f.url && f.url.toLowerCase().includes('.pdf')));
      if (pdfFile && pdfFile.url && pdfFile.url.startsWith('/uploads/')) {
        foundPath = path.join(__dirname, 'public', pdfFile.url);
      }
    }
    if (!foundPath && doc.downloadUrl && doc.downloadUrl.startsWith('/uploads/')) {
      foundPath = path.join(__dirname, 'public', doc.downloadUrl);
    }

    if (foundPath && fs.existsSync(foundPath)) {
      sourceBuffer = fs.readFileSync(foundPath);
    }

    // Nếu có link Google Drive, tự động tải và cache file PDF thật từ Google Drive
    if (!sourceBuffer) {
      const gdriveUrl = findPdfDriveUrl(doc);

      if (gdriveUrl) {
        const match = gdriveUrl.match(/\/file\/d\/([a-zA-Z0-9_-]+)/) || gdriveUrl.match(/id=([a-zA-Z0-9_-]+)/);
        if (match && match[1]) {
          const driveId = match[1];
          const cachePath = path.join(__dirname, 'public/uploads', `gdrive_${driveId}.pdf`);
          if (fs.existsSync(cachePath) && fs.statSync(cachePath).size > 1000) {
            sourceBuffer = fs.readFileSync(cachePath);
          } else {
            try {
              console.log(`[PDF Preview] Đang tự động tải file thật từ Google Drive (ID: ${driveId})...`);
              const resp = await fetch(`https://drive.google.com/uc?export=download&id=${driveId}`, {
                headers: { 'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)' }
              });
              if (resp.ok) {
                const ab = await resp.arrayBuffer();
                if (ab.byteLength > 1000) {
                  fs.writeFileSync(cachePath, Buffer.from(ab));
                  sourceBuffer = Buffer.from(ab);
                  console.log(`[PDF Preview] Đã tải file PDF thật từ Google Drive (${ab.byteLength} bytes)`);
                }
              }
            } catch (errG) {
              console.error("[PDF Preview] Lỗi tải từ Google Drive:", errG.message);
            }
          }
        }
      }
    }

    // Nếu là file ngoài hoặc chưa có, dùng file 20 trang mẫu tiêu chuẩn
    const fallbackSamplePath = path.join(__dirname, 'public/uploads/sample_giao_trinh_20_trang.pdf');
    if (!sourceBuffer && fs.existsSync(fallbackSamplePath)) {
      sourceBuffer = fs.readFileSync(fallbackSamplePath);
    }

    if (!sourceBuffer) {
      return res.status(404).send("Chưa có file PDF cho tài liệu này");
    }

    // Tiến hành nạp và cắt đúng số trang giới hạn với pdf-lib
    const srcDoc = await PDFDocument.load(sourceBuffer, { ignoreEncryption: true });
    const totalPages = srcDoc.getPageCount();
    const allowedPages = Math.min(previewPagesLimit, totalPages);

    const outDoc = await PDFDocument.create();
    const pageIndices = Array.from({ length: allowedPages }, (_, i) => i);
    const copiedPages = await outDoc.copyPages(srcDoc, pageIndices);
    copiedPages.forEach(p => outDoc.addPage(p));

    // Thêm Trang Khóa Thông Báo (Paywall Lock Page) ở cuối nếu còn các trang chưa mở
    if (totalPages > allowedPages) {
      const fontBold = await outDoc.embedFont(StandardFonts.HelveticaBold);
      const fontRegular = await outDoc.embedFont(StandardFonts.Helvetica);

      const lockPage = outDoc.addPage([595, 842]);
      lockPage.drawRectangle({ x: 0, y: 0, width: 595, height: 842, color: rgb(0.05, 0.08, 0.14) });
      lockPage.drawRectangle({ x: 35, y: 35, width: 525, height: 772, borderColor: rgb(0.88, 0.11, 0.28), borderWidth: 2 });

      lockPage.drawText('TIN HOC CLICK - BAN DOC THU GIOI HAN', { x: 65, y: 720, size: 20, font: fontBold, color: rgb(0.22, 0.74, 0.97) });
      lockPage.drawText('DA XEM HET ' + allowedPages + ' TRANG DOC THU MIEN PHI!', { x: 65, y: 660, size: 18, font: fontBold, color: rgb(0.96, 0.62, 0.04) });
      lockPage.drawText('Tai lieu goc gom toan bo ' + totalPages + ' trang bai tap & ly thuyet day du.', { x: 65, y: 615, size: 13, font: fontRegular, color: rgb(0.8, 0.85, 0.9) });

      lockPage.drawRectangle({ x: 65, y: 380, width: 465, height: 180, color: rgb(0.08, 0.12, 0.2), borderColor: rgb(0.01, 0.52, 0.78), borderWidth: 1 });
      lockPage.drawText('HUONG DAN MO KHOA TOAN BO TAI LIEU GOC:', { x: 85, y: 530, size: 13, font: fontBold, color: rgb(1, 1, 1) });
      lockPage.drawText('1. Quay lai website va bam nut "MUA NGAY (Thanh Toan)"', { x: 95, y: 495, size: 11, font: fontRegular, color: rgb(0.2, 0.9, 0.4) });
      const priceText = Number(doc.price || 0) > 0 ? Number(doc.price).toLocaleString() + 'd' : '0d (Mien phi)';
      lockPage.drawText('2. Quet ma VietQR de thanh toan tu dong (Gia: ' + priceText + ')', { x: 95, y: 465, size: 11, font: fontRegular, color: rgb(0.2, 0.9, 0.4) });
      lockPage.drawText('3. He thong se tu dong mo khoa toan bo file goc de ban tai ve!', { x: 95, y: 435, size: 11, font: fontRegular, color: rgb(0.2, 0.9, 0.4) });
      lockPage.drawText('Cam on ban da ung ho khoa hoc & tai lieu cua Tin Hoc Click!', { x: 95, y: 405, size: 11, font: fontBold, color: rgb(0.96, 0.62, 0.04) });

      lockPage.drawText('Trang ' + (allowedPages + 1) + ' (Trang Khoa Gioi Han) - Tong so trang goc: ' + totalPages, { x: 65, y: 55, size: 10, font: fontRegular, color: rgb(0.5, 0.5, 0.5) });
    }

    const slicedPdfBytes = await outDoc.save();
    res.setHeader("Content-Type", "application/pdf");
    res.setHeader("Content-Disposition", 'inline; filename="preview_' + doc.id + '.pdf"');
    res.send(Buffer.from(slicedPdfBytes));
  } catch (err) {
    console.error("Lỗi cắt file PDF xem trước:", err);
    res.status(500).send("Lỗi xử lý file PDF");
  }
});

// API LẤY THÔNG TIN SỐ TRANG ĐỌC THỬ CỦA TÀI LIỆU (TÍNH TỔNG SỐ TRANG THỰC TẾ)
app.get("/api/public/pdf-doc-info/:docId", async (req, res) => {
  try {
    const { docId } = req.params;
    const db = readDb();
    const doc = (db.documents || []).find(d => d.id === docId);
    if (!doc) return res.status(404).json({ error: "Không tìm thấy" });

    let totalPages = 20;
    try {
      const { PDFDocument } = require("pdf-lib");
      let buf = null;
      const gdriveUrl = findPdfDriveUrl(doc);
      if (gdriveUrl) {
        const match = gdriveUrl.match(/\/file\/d\/([a-zA-Z0-9_-]+)/) || gdriveUrl.match(/id=([a-zA-Z0-9_-]+)/);
        if (match && match[1]) {
          const cachePath = path.join(__dirname, 'public/uploads', `gdrive_${match[1]}.pdf`);
          if (fs.existsSync(cachePath)) {
            buf = fs.readFileSync(cachePath);
          }
        }
      }
      if (!buf && doc.downloadUrl && doc.downloadUrl.startsWith('/uploads/')) {
        const p = path.join(__dirname, 'public', doc.downloadUrl);
        if (fs.existsSync(p)) buf = fs.readFileSync(p);
      }
      if (buf) {
        const d = await PDFDocument.load(buf, { ignoreEncryption: true });
        totalPages = d.getPageCount();
      }
    } catch (e) {}

    const isFullPreview = Number(doc.previewPages) === -1;
    res.json({
      id: doc.id,
      title: doc.title,
      previewPages: isFullPreview ? -1 : (doc.previewPages || 10),
      previewAll: isFullPreview,
      totalPages: totalPages,
      price: doc.price || 0,
      isFree: doc.isFree
    });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Xóa tài liệu
app.delete("/api/admin/document/:id", requireAdmin, (req, res) => {
  const { id } = req.params;
  const db = readDb();
  if (!db.documents) db.documents = [];
  db.documents = db.documents.filter(d => d.id !== id);
  writeDb(db);
  res.json({ success: true });
});

// ==============================================================
// API KÍCH HOẠT & KHÓA BẢN QUYỀN LISPCAD THEO 1 MÁY DUY NHẤT (MACHINE ID)
// ==============================================================
function handleActivateLisp(req, res) {
  const orderCodeRaw = (req.body && req.body.orderCode) || (req.query && req.query.orderCode) || "";
  const machineIdRaw = (req.body && req.body.machineId) || (req.query && req.query.machineId) || "";
  const format = (req.query && req.query.format) || (req.body && req.body.format) || "json";

  const orderCode = String(orderCodeRaw).trim().toUpperCase();
  const machineId = String(machineIdRaw).trim();

  const reply = (ok, statusCode, status, message, extra = {}) => {
    if (format === "text") {
      return res.status(statusCode).type("text/plain; charset=utf-8").send(ok ? "OK" : `DENIED:${message}`);
    }
    return res.status(statusCode).json({ ok, status, message, ...extra });
  };

  if (!orderCode || !machineId) {
    return reply(false, 400, "MISSING_PARAMS", "Thieu ma don hang hoac ma may tinh!");
  }

  const db = readDb();
  const order = (db.orders || []).find(o => String(o.orderCode || "").toUpperCase() === orderCode);
  if (!order) {
    return reply(false, 404, "NOT_FOUND", "Ma don hang khong ton tai tren he thong!");
  }
  if (order.status !== "PAID") {
    return reply(false, 403, "UNPAID", "Don hang nay chua thanh toan thanh cong!");
  }

  if (!order.activatedMachineId) {
    order.activatedMachineId = machineId;
    order.activatedAt = new Date().toISOString();
    writeDb(db);
    console.log(`🔐 [License] Kích hoạt lần đầu đơn #${order.orderCode} cho máy: ${machineId}`);
    return reply(true, 200, "ACTIVATED", "Kich hoat ban quyen 1 may thanh cong!", { machineId });
  }

  if (order.activatedMachineId === machineId) {
    return reply(true, 200, "VERIFIED", "Ban quyen hop le!", { machineId });
  }

  console.log(`⛔ [License] Từ chối đơn #${order.orderCode}: đã khóa máy ${order.activatedMachineId}, thử chạy trên ${machineId}`);
  return reply(false, 403, "MACHINE_MISMATCH", "Ma don hang nay da duoc kich hoat tren 1 may tinh khac!");
}

app.get("/api/public/activate-lisp", handleActivateLisp);
app.post("/api/public/activate-lisp", handleActivateLisp);

// Admin bấm duyệt đơn thủ công
app.post("/api/admin/order/approve", requireAdmin, (req, res) => {
  const { orderCode } = req.body;
  const db = readDb();
  const order = db.orders.find(o => o.orderCode === orderCode);

  if (!order) {
    return res.status(404).json({ error: "Không tìm thấy đơn hàng" });
  }

  completeOrderPaid(order, db);
  writeDb(db);
  res.json({ success: true, message: "Đã duyệt đơn hàng thành công!" });
});

// Admin reset khóa mã máy (cho phép khách kích hoạt lại trên máy mới)
app.post("/api/admin/order/reset-machine", requireAdmin, (req, res) => {
  const { orderCode } = req.body;
  const db = readDb();
  const order = (db.orders || []).find(o => o.orderCode === orderCode);
  if (!order) {
    return res.status(404).json({ error: "Không tìm thấy đơn hàng" });
  }
  order.activatedMachineId = null;
  order.activatedAt = null;
  writeDb(db);
  res.json({ success: true, message: `Đã reset mã máy cho đơn #${order.orderCode}!` });
});

// Xóa đơn hàng
app.delete("/api/admin/order/:orderCode", requireAdmin, (req, res) => {
  const { orderCode } = req.params;
  const db = readDb();
  db.orders = db.orders.filter(o => o.orderCode !== orderCode);
  writeDb(db);
  res.json({ success: true });
});

// ==========================================
// 3. API TÀI KHOẢN HỌC VIÊN & KHÓA HỌC 2D
// ==========================================

// Đăng ký tài khoản học viên
app.post("/api/user/register", (req, res) => {
  const { name, email, phone, password } = req.body;
  if (!name || !password || (!email && !phone)) {
    return res.status(400).json({ error: "Vui lòng nhập đầy đủ Họ tên, Số điện thoại/Email và Mật khẩu" });
  }
  const db = readDb();
  if (!db.users) db.users = [];

  const cleanPhone = (phone || "").trim();
  const cleanEmail = (email || "").trim().toLowerCase();

  const exists = db.users.find(u => 
    (cleanEmail && u.email && u.email.toLowerCase() === cleanEmail) || 
    (cleanPhone && u.phone && u.phone === cleanPhone)
  );

  if (exists) {
    return res.status(400).json({ error: "Số điện thoại hoặc Email này đã có tài khoản. Vui lòng bấm Đăng nhập!" });
  }

  const newUser = {
    id: "usr_" + Date.now(),
    name: name.trim(),
    email: cleanEmail,
    phone: cleanPhone,
    password: password,
    role: "Học viên",
    enrolledCourses: [],
    createdAt: new Date().toISOString()
  };

  db.users.push(newUser);
  writeDb(db);

  res.cookie("student_token", newUser.id, { httpOnly: true, maxAge: 90 * 24 * 3600 * 1000 });
  return res.json({
    success: true,
    user: {
      id: newUser.id,
      name: newUser.name,
      email: newUser.email,
      phone: newUser.phone,
      enrolledCourses: newUser.enrolledCourses,
      role: newUser.role
    }
  });
});

// Đăng nhập học viên
app.post("/api/user/login", (req, res) => {
  const { emailOrPhone, password } = req.body;
  if (!emailOrPhone || !password) {
    return res.status(400).json({ error: "Vui lòng nhập số điện thoại/email và mật khẩu" });
  }
  const db = readDb();
  if (!db.users) db.users = [];

  const q = emailOrPhone.trim().toLowerCase();
  const user = db.users.find(u => 
    ((u.email && u.email.toLowerCase() === q) || (u.phone && u.phone === q)) && 
    u.password === password
  );

  if (!user) {
    return res.status(401).json({ error: "Tài khoản hoặc mật khẩu không chính xác!" });
  }

  res.cookie("student_token", user.id, { httpOnly: true, maxAge: 90 * 24 * 3600 * 1000 });
  return res.json({
    success: true,
    user: {
      id: user.id,
      name: user.name,
      email: user.email,
      phone: user.phone,
      enrolledCourses: user.enrolledCourses || [],
      role: user.role
    }
  });
});

// Lấy thông tin tài khoản đang đăng nhập
app.get("/api/user/me", (req, res) => {
  const studentId = req.cookies.student_token || req.headers["x-student-id"];
  const db = readDb();
  if (!db.users || !studentId) {
    return res.json({ user: null });
  }
  const user = db.users.find(u => u.id === studentId);
  if (!user) {
    return res.json({ user: null });
  }
  return res.json({
    user: {
      id: user.id,
      name: user.name,
      email: user.email,
      phone: user.phone,
      enrolledCourses: user.enrolledCourses || [],
      role: user.role
    }
  });
});

// Đăng xuất học viên
app.post("/api/user/logout", (req, res) => {
  res.clearCookie("student_token");
  res.json({ success: true });
});

// Lấy danh sách tất cả bài học (Kèm trạng thái Khóa / Mở khóa đối với học viên)
app.get("/api/course/lessons", (req, res) => {
  const db = readDb();
  const studentId = req.cookies.student_token || req.headers["x-student-id"];
  const adminToken = req.cookies.admin_auth || req.headers.authorization;
  const isAdmin = adminToken && adminToken === "auth_" + db.adminPassword;

  let hasEnrolled = false;
  let currentUser = null;
  if (db.users && studentId) {
    currentUser = db.users.find(u => u.id === studentId);
    if (currentUser && currentUser.enrolledCourses && currentUser.enrolledCourses.includes("prod_course_2d")) {
      hasEnrolled = true;
    }
  }

  const lessons = (db.lessons || []).map(l => {
    const isLocked = Boolean(l.isLocked);
    const canAccess = isAdmin || hasEnrolled || !isLocked;
    return {
      id: l.id,
      lessonNumber: l.lessonNumber,
      title: l.title,
      duration: l.duration,
      isLocked: isLocked,
      canAccess: canAccess,
      description: l.description,
      hasQuiz: Boolean(l.quiz && l.quiz.length > 0)
    };
  });

  const courseProd = (db.products || []).find(p => p.id === "prod_course_2d" || p.category === "Khóa học" || (p.name && p.name.includes("Khóa học")));

  res.json({
    success: true,
    courseTitle: "Khóa học AutoCAD 2D",
    coursePrice: courseProd ? Number(courseProd.price) : 299000,
    courseOldPrice: courseProd ? Number(courseProd.oldPrice) : 590000,
    courseId: courseProd ? courseProd.id : "prod_course_2d",
    hasEnrolled: hasEnrolled,
    isAdmin: isAdmin,
    currentUser: currentUser ? { name: currentUser.name, phone: currentUser.phone } : null,
    totalLessons: lessons.length,
    lessons: lessons
  });
});

// Cập nhật học phí Khóa học AutoCAD 2D từ trang Admin
app.post("/api/admin/course-price", requireAdmin, (req, res) => {
  const db = readDb();
  const { price, oldPrice } = req.body;
  if (!db.products) db.products = [];
  let course = db.products.find(p => p.id === "prod_course_2d" || p.category === "Khóa học" || (p.name && p.name.includes("Khóa học")));
  if (!course) {
    course = {
      id: "prod_course_2d",
      name: "Khóa học AutoCAD 2D Từ Cơ Bản Đến Nâng Cao",
      category: "Khóa học",
      price: Number(price) || 299000,
      oldPrice: Number(oldPrice) || 590000,
      description: "Khóa học video bài giảng AutoCAD 2D bài bản từ cơ bản đến nâng cao, thực chiến bản vẽ kỹ thuật.",
      commands: "ACAD 2D",
      instructions: "Khóa học trực tuyến kèm bộ giáo trình và file bài tập thực hành. Học viên được hỗ trợ trọn đời.",
      downloadType: "local",
      downloadUrl: "/files/AutoCAD_Pro_Toolbox.lsp",
      isActive: true,
      imageUrl: "https://images.unsplash.com/photo-1503387762-592deb58ef4e?w=600",
      createdAt: new Date().toISOString()
    };
    db.products.push(course);
  } else {
    course.id = "prod_course_2d";
    course.category = "Khóa học";
    if (price !== undefined) course.price = Number(price);
    if (oldPrice !== undefined) course.oldPrice = Number(oldPrice);
  }
  writeDb(db);
  res.json({ success: true, course });
});

// Xem chi tiết 1 bài học
app.get("/api/course/lesson/:id", (req, res) => {
  const { id } = req.params;
  const db = readDb();
  const lesson = (db.lessons || []).find(l => l.id === id);
  if (!lesson) {
    return res.status(404).json({ error: "Không tìm thấy bài học này" });
  }

  const studentId = req.cookies.student_token || req.headers["x-student-id"];
  const adminToken = req.cookies.admin_auth || req.headers.authorization;
  const isAdmin = adminToken && adminToken === "auth_" + db.adminPassword;

  let hasEnrolled = false;
  if (db.users && studentId) {
    const u = db.users.find(u => u.id === studentId);
    if (u && u.enrolledCourses && u.enrolledCourses.includes("prod_course_2d")) {
      hasEnrolled = true;
    }
  }

  const isLocked = Boolean(lesson.isLocked);
  const canAccess = isAdmin || hasEnrolled || !isLocked;

  if (!canAccess) {
    return res.json({
      id: lesson.id,
      lessonNumber: lesson.lessonNumber,
      title: lesson.title,
      duration: lesson.duration,
      description: lesson.description,
      isLocked: true,
      canAccess: false,
      message: "Bài học đã bị khóa. Vui lòng đăng nhập và mua Khóa học AutoCAD 2D để mở khóa toàn bộ bài giảng."
    });
  }

  res.json({
    id: lesson.id,
    lessonNumber: lesson.lessonNumber,
    title: lesson.title,
    duration: lesson.duration,
    isLocked: isLocked,
    canAccess: true,
    videoUrl: lesson.videoUrl,
    description: lesson.description,
    summary: lesson.summary || [],
    keyNotes: lesson.keyNotes || [],
    pdfUrl: lesson.pdfUrl || "",
    dwgUrl: lesson.dwgUrl || "",
    quiz: lesson.quiz || []
  });
});

// Admin toggle Khóa / Mở khóa bài giảng 1-click
app.post("/api/admin/lessons/:id/toggle-lock", requireAdmin, (req, res) => {
  const { id } = req.params;
  const db = readDb();
  const lesson = (db.lessons || []).find(l => l.id === id);
  if (!lesson) {
    return res.status(404).json({ error: "Không tìm thấy bài học" });
  }
  lesson.isLocked = !lesson.isLocked;
  writeDb(db);
  res.json({ success: true, isLocked: lesson.isLocked, message: lesson.isLocked ? "Đã khóa bài học" : "Đã mở khóa bài học (cho học thử)" });
});

// Admin thêm / sửa bài giảng
app.post("/api/admin/lesson", requireAdmin, (req, res) => {
  const { id, title, duration, videoUrl, description, summary, keyNotes, pdfUrl, dwgUrl, isLocked, lessonNumber } = req.body;
  const db = readDb();
  if (!db.lessons) db.lessons = [];

  let lesson = db.lessons.find(l => l.id === id);
  if (!lesson) {
    lesson = {
      id: "lesson-2d-" + Date.now(),
      courseId: "prod_course_2d",
      lessonNumber: db.lessons.length + 1,
      createdAt: new Date().toISOString()
    };
    db.lessons.push(lesson);
  }

  lesson.title = title || "Bài học mới";
  lesson.duration = duration || "15:00";
  lesson.videoUrl = videoUrl || "";
  lesson.description = description || "";
  lesson.summary = Array.isArray(summary) ? summary : (summary ? summary.split("\n").map(s => s.trim()).filter(Boolean) : []);
  lesson.keyNotes = Array.isArray(keyNotes) ? keyNotes : (keyNotes ? keyNotes.split("\n").map(s => s.trim()).filter(Boolean) : []);
  lesson.pdfUrl = pdfUrl || "";
  lesson.dwgUrl = dwgUrl || "";
  lesson.isLocked = isLocked === true || isLocked === "true";
  if (lessonNumber) lesson.lessonNumber = Number(lessonNumber);

  writeDb(db);
  res.json({ success: true, lesson });
});

// Admin xóa bài giảng
app.delete("/api/admin/lesson/:id", requireAdmin, (req, res) => {
  const { id } = req.params;
  const db = readDb();
  db.lessons = (db.lessons || []).filter(l => l.id !== id);
  writeDb(db);
  res.json({ success: true });
});

// Admin xem danh sách học viên
app.get("/api/admin/students", requireAdmin, (req, res) => {
  const db = readDb();
  res.json({ success: true, students: db.users || [] });
});

// Admin kích hoạt / hủy kích hoạt khóa học cho học viên thủ công
app.post("/api/admin/students/:id/toggle-enroll", requireAdmin, (req, res) => {
  const { id } = req.params;
  const db = readDb();
  const user = (db.users || []).find(u => u.id === id);
  if (!user) return res.status(404).json({ error: "Không tìm thấy học viên" });
  if (!user.enrolledCourses) user.enrolledCourses = [];
  const idx = user.enrolledCourses.indexOf("prod_course_2d");
  if (idx > -1) {
    user.enrolledCourses.splice(idx, 1);
  } else {
    user.enrolledCourses.push("prod_course_2d");
  }
  writeDb(db);
  res.json({ success: true, enrolledCourses: user.enrolledCourses });
});

// Khởi chạy Server
app.listen(PORT, () => {
  console.log(`\n======================================================`);
  console.log(`🚀 Trang web đang chạy tại: http://localhost:${PORT}`);
  console.log(`🔑 Trang quản trị Admin: http://localhost:${PORT}/admin.html`);
  console.log(`👉 Mật khẩu Admin mặc định: admin`);
  console.log(`======================================================\n`);
});
