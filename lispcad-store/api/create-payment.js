const PayOS = require("@payos/node");

module.exports = async (req, res) => {
  // Bật CORS cho phép gọi từ frontend
  res.setHeader("Access-Control-Allow-Credentials", true);
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET,OPTIONS,PATCH,DELETE,POST,PUT");
  res.setHeader(
    "Access-Control-Allow-Headers",
    "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version"
  );

  if (req.method === "OPTIONS") {
    return res.status(200).end();
  }

  if (req.method !== "POST") {
    return res.status(405).json({ error: "Method not allowed" });
  }

  try {
    const clientId = process.env.PAYOS_CLIENT_ID;
    const apiKey = process.env.PAYOS_API_KEY;
    const checksumKey = process.env.PAYOS_CHECKSUM_KEY;

    if (!clientId || !apiKey || !checksumKey) {
      return res.status(500).json({
        error: "Chưa cấu hình API Keys của PayOS trong file .env hoặc Vercel Environment Variables"
      });
    }

    const payOS = new PayOS(clientId, apiKey, checksumKey);

    // Lấy thông tin từ request body
    const { buyerName, buyerEmail, buyerPhone } = req.body || {};

    // Sinh mã đơn hàng ngẫu nhiên (PayOS yêu cầu orderCode là số nguyên duy nhất)
    const orderCode = Number(String(Date.now()).slice(-6) + Math.floor(Math.random() * 90 + 10));
    const amount = Number(process.env.PRICE_VND || 99000);

    // Xác định domain hiện tại để điều hướng sau khi thanh toán
    const protocol = req.headers["x-forwarded-proto"] || "http";
    const host = req.headers["x-forwarded-host"] || req.headers.host;
    const baseUrl = `${protocol}://${host}`;

    const paymentData = {
      orderCode: orderCode,
      amount: amount,
      description: `LISP ${orderCode}`.slice(0, 25),
      items: [
        {
          name: "Bộ LispCAD Pro",
          quantity: 1,
          price: amount
        }
      ],
      returnUrl: `${baseUrl}/success.html?orderCode=${orderCode}`,
      cancelUrl: `${baseUrl}/index.html`
    };

    if (buyerName) paymentData.buyerName = buyerName;
    if (buyerEmail) paymentData.buyerEmail = buyerEmail;
    if (buyerPhone) paymentData.buyerPhone = buyerPhone;

    const paymentLinkRes = await payOS.createPaymentLink(paymentData);

    return res.status(200).json({
      success: true,
      checkoutUrl: paymentLinkRes.checkoutUrl,
      orderCode: orderCode
    });
  } catch (error) {
    console.error("Lỗi tạo link thanh toán PayOS:", error);
    return res.status(500).json({
      error: error.message || "Không thể tạo liên kết thanh toán"
    });
  }
};
