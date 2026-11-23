const PayOS = require("@payos/node");

module.exports = async (req, res) => {
  res.setHeader("Access-Control-Allow-Credentials", true);
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET,OPTIONS");

  if (req.method === "OPTIONS") {
    return res.status(200).end();
  }

  const { orderCode } = req.query;

  if (!orderCode) {
    return res.status(400).json({ error: "Thiếu mã đơn hàng orderCode" });
  }

  try {
    const clientId = process.env.PAYOS_CLIENT_ID;
    const apiKey = process.env.PAYOS_API_KEY;
    const checksumKey = process.env.PAYOS_CHECKSUM_KEY;

    if (!clientId || !apiKey || !checksumKey) {
      return res.status(500).json({ error: "Chưa cấu hình API Keys PayOS" });
    }

    const payOS = new PayOS(clientId, apiKey, checksumKey);
    const paymentInfo = await payOS.getPaymentLinkInformation(orderCode);

    return res.status(200).json({
      success: true,
      orderCode: paymentInfo.orderCode,
      status: paymentInfo.status, // "PAID", "PENDING", "CANCELLED"
      amount: paymentInfo.amount,
      isPaid: paymentInfo.status === "PAID"
    });
  } catch (error) {
    console.error("Lỗi kiểm tra đơn hàng:", error);
    return res.status(500).json({
      error: error.message || "Không thể kiểm tra trạng thái đơn hàng"
    });
  }
};
