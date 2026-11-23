const PayOS = require("@payos/node");
const fs = require("fs");
const path = require("path");

module.exports = async (req, res) => {
  const { orderCode } = req.query;

  if (!orderCode) {
    return res.status(400).send("Thiếu mã đơn hàng");
  }

  try {
    const clientId = process.env.PAYOS_CLIENT_ID;
    const apiKey = process.env.PAYOS_API_KEY;
    const checksumKey = process.env.PAYOS_CHECKSUM_KEY;

    if (!clientId || !apiKey || !checksumKey) {
      return res.status(500).send("Chưa cấu hình API Keys PayOS");
    }

    const payOS = new PayOS(clientId, apiKey, checksumKey);
    const paymentInfo = await payOS.getPaymentLinkInformation(orderCode);

    // Kiểm tra bảo mật: Bắt buộc đơn hàng phải ở trạng thái ĐÃ THANH TOÁN (PAID)
    if (!paymentInfo || paymentInfo.status !== "PAID") {
      return res.status(403).send("Đơn hàng chưa thanh toán thành công hoặc không hợp lệ.");
    }

    // Tùy chọn 1: Nếu cấu hình link Google Drive / OneDrive trong .env thì redirect
    if (process.env.DOWNLOAD_REDIRECT_URL) {
      return res.redirect(process.env.DOWNLOAD_REDIRECT_URL);
    }

    // Tùy chọn 2: Tải trực tiếp file Lisp từ thư mục files/
    const filePath = path.join(process.cwd(), "files", "AutoCAD_Pro_Toolbox.lsp");

    if (!fs.existsSync(filePath)) {
      return res.status(404).send("File LispCAD chưa được tải lên hệ thống.");
    }

    const fileContent = fs.readFileSync(filePath);
    res.setHeader("Content-Type", "application/octet-stream");
    res.setHeader("Content-Disposition", 'attachment; filename="AutoCAD_Pro_Toolbox.lsp"');
    return res.send(fileContent);
  } catch (error) {
    console.error("Lỗi xác thực tải file:", error);
    return res.status(500).send("Có lỗi xảy ra khi xác thực tải file.");
  }
};
