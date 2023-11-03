# Install packages
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("ggmosaic")
install.packages("readr")


# Thư viện packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmosaic)
library(readr)

### DATASET DATA_TRANSACTION
# Xem kiểu dữ liệu
str(data_transaction)

# Chuyển đổi kiểu dữ liệu của DATE & PROD_NAME
data_transaction <- as.data.table(data_transaction)

data_transaction$DATE <- as.Date(data_transaction$DATE,origin = "1899-12-30")
data_transaction$PROD_NAME <- as.character(data_transaction$PROD_NAME)

# Cột PROD_NAME chứa nhiều thông tin, bao gồm BRAND, cân nặng sản phẩm, danh mục
# Chia lại sản phẩm theo danh mục
productWords <- data.table(unlist(strsplit(unique(data_transaction$PROD_NAME), " ")))
setnames(productWords, "words")
productWords <- productWords[grepl("\\d",words)==FALSE,]
productWords <- productWords[grepl("[:alpha:]",words),]
productWords[, .N,words][order(N,decreasing = TRUE)]

# Tổng quan dữ liệu
summary(data_transaction)

# Thấy PROD_QTY có giá trị MAX=200 tăng đột biến so với giá trị Media
# Kiểm tra giá trị outlet của PROD_QTY
data_transaction[data_transaction$PROD_QTY ==200,]
# Nhận thấy giá trị outlet này đến từ cùng 1 người mua
# Phân tích mã số khách hàng
# Nhận thấy khách hàng mua tại hai thời điểm khác nhau và tại hai thời điểm này không rơi vào thời gian đặc biệt
# Loại bỏ ra khỏi dataset để tiếp tục kiểm tra
data_transaction[data_transaction$LYLTY_CARD_NBR==226000,]

# Đếm bao nhiêu khách hàng đến của hàng trong 1 ngày
data_transaction %>% group_by(DATE) %>% count() %>% arrange()

# Nhận thấy có 364 ngày (thiếu một ngày) -> Kiểm tra ngày không có khách tới mua
# Tạo table chứa tất cả các ngày từ 01/07/2018 - 30/06/2019 (365 ngày)
allDates <-data.table(seq(as.Date("2018-07-01"), as.Date("2019-06-30"),by="day"))
setnames(allDates,"DATE")

# Join 2 table (allDates làm table gốc) đối chiếu ngày sales = 0
transaction_day <- data_transaction %>% right_join(allDates,by=c("DATE")) %>% group_by(DATE) %>% arrange(DATE) %>% count()

# Vẽ đồ thị của 1 năm và tìm ra ngày có sale = 0
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust=0.5))
ggplot(transaction_day,aes(x=DATE,y=n)) + 
  geom_line() +
  labs(x="Day",y="Number of transactions",title="Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle=90,vjust=0.5)) 

# Từ đồ thị thấy tháng 12 gồm có max và min , phân tích thêm đồ thị của tháng 12
# Tạo một biến mới chứa tháng
transaction_day$Month <- lubridate::month(transaction_day$DATE)

# Trích xuất các dòng có tháng là 12
december_data <- transaction_day[transaction_day$Month == 12, ]

# Vẽ đồ thị của tháng 12
ggplot(december_data, aes(x = DATE, y = n)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time (December)") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Kết luận: Ngày doanh số = 0 chính là ngày lễ Giáng Sinh, các cửa hàng đều nghỉ vào ngày này, sales = 0 là đúng, không có giá trị ngoại lai
# Xem xét sản phẩm nào được ưa chuộng nhất?
# Tạo table PACK xem xét khối lượng nào được mua nhiều nhất
PACK <- data.table(unlist(strsplit(data_transaction$PROD_NAME, " ")))
setnames(PACK, "PACK_SIZE")
PACK <- PACK[grepl("\\d",PACK_SIZE)==TRUE,]
PACK <- PACK[, PACK_SIZE := gsub("[A-Za-z&/]", "", PACK_SIZE)][]
PACK <- PACK[grepl("[:alpha:]",PACK_SIZE)==FALSE,]
PACK_TABLE <- PACK %>% group_by(PACK_SIZE) %>% count() %>% arrange()
# Khối lượng lớn nhất là 380g và nhỏ nhất là 70g

# Cột PACK_SIZE đang ở kiểu dữ liệu rời rạc, để vẽ biểu đồ histogram, chuyển sang kiểu dữ liệu liên tục (numeric)
PACK_TABLE$PACK_SIZE <- as.numeric(PACK_TABLE$PACK_SIZE) 

# Vẽ đồ thị mô tả số lượng mua hàng theo khối lượng
ggplot(PACK_TABLE,aes(x=PACK_SIZE,y=n,fill=PACK_SIZE)) + 
  geom_bar(stat = "identity", width = 10,color="black") + 
  labs(x="Pack size of Product",y="Frequency",title="Number of size Product") +
  geom_text(aes(label = PACK_SIZE), vjust = -0.5, size = 3)
# Kết luận: Sản phẩm có khối lượng được mua nhiều nhất là 175g

# Xem xét BRAND sản phẩm nào được ưa chuộng nhất?
# Từ đầu tiên trong PROD_NAME chính là tên BRAND
# Lọc BRAND ra khỏi PROD_NAME, lấy chữ đầu tiên trước dấu ' ' thứ nhất và in hoa
data_transaction[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(' ', PROD_NAME)-1))]

# Xem tổng quan tần xuất của BRAND (đã được nhóm lại)
data_transaction[, .N, by=BRAND] [order(-N)]

# Nhận thấy có những BRAND bị trùng/viết nhầm -> FIX
data_transaction[BRAND=="RED", BRAND :="RRD"]
data_transaction[BRAND=="SNBTS", BRAND :="SUNBITES"]
data_transaction[BRAND=="INFZNS", BRAND :="INFUZIONS"]
data_transaction[BRAND=="WW", BRAND :="WOOLWORTHS"]
data_transaction[BRAND=="SMITH", BRAND :="SMITHS"]
data_transaction[BRAND=="NCC", BRAND :="NATURAL"]
data_transaction[BRAND=="DORITO", BRAND :="DORITOS"]
data_transaction[BRAND=="GRAIN", BRAND :="GRNWVES"]

# Kiểm tra lại 
data_transaction[, .N, by=BRAND] [order(-N)]

### DATASET PURCHASE_BEHAVIOUR
purchase_behaviour <- as.data.table(purchase_behaviour)

str(purchase_behaviour)
summary(purchase_behaviour)

purchase_behaviour[, .N, by=LIFESTAGE] [order(-N)]
purchase_behaviour[, .N, by=PREMIUM_CUSTOMER] [order(-N)]

data_transaction[, PACK_SIZE:=parse_number(PROD_NAME)]
data_total <- data_transaction %>% left_join(purchase_behaviour, by="LYLTY_CARD_NBR")

### PHÂN TÍCH PHÂN KHÚC KHÁCH HÀNG
# 1. Ai là người chi têu nhiều nhất cho việc mua Chips(tổng doanh sô bán hàng), mô tả về khách hàng theo giai đoạn cuộc sống và mức độ cao cấp của hành vi mua sắm tổng quan?
# 2. Có bao nhiêu phân khúc khách hàng trong mỗi nhóm?
# 3. Có bao nhiêu gói Chips được mua bởi khách hàng trong từng nhóm?
# 3. Giá trung bình của Chips theo từng nhóm khách hàng khác nhau là bao nhiêu?

# 1. Ai là người chi tiêu nhiều nhất?
data_total <- as.data.table(data_total)

# Tạo biểu đồ doanh số theo phân khúc khách hàng
sales <- data_total[, .(SALES=sum(TOT_SALES)), .(LIFESTAGE,PREMIUM_CUSTOMER)]
sales <- as.data.frame(sales)

p <- ggplot(sales) +
  geom_mosaic(aes(weight=SALES, x=product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x="Lifestage", y="Premium customer flag", title = "Proportion of sales") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

text_data <- data.frame(
  x = (ggplot_build(p)$data[[1]]$xmin + ggplot_build(p)$data[[1]]$xmax) / 2,
  y = (ggplot_build(p)$data[[1]]$ymin + ggplot_build(p)$data[[1]]$ymax) / 2,
  label = as.character(paste(round(ggplot_build(p)$data[[1]]$.wt / sum(ggplot_build(p)$data[[1]]$.wt), 3) * 100, "%"))
)

p + geom_text(data = text_data, aes(x = x, y = y, label = label))

# KẾT LUẬN: Doanh số chủ yếu đến từ 3 nguồn chính
# 1. 8.7% từ phân khúc Older Family - Budget
# 2. 8.2% từ phân khúc Young Single/Couple - Mainstream
# 3. 8.1% từ phân khúc Retirees

# Tạo biểu đồ số lượng khách hàng trong mỗi phân khúc
customer <- data_total %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(CUSTOMERS = n_distinct(LYLTY_CARD_NBR), .groups = 'drop')

p <- ggplot(customer) +
  geom_mosaic(aes(weight=CUSTOMERS, x=product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x="Lifestage", y="Premium customer flag", title = "Proportion of Customers") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

text_data <- data.frame(
  x = (ggplot_build(p)$data[[1]]$xmin + ggplot_build(p)$data[[1]]$xmax) / 2,
  y = (ggplot_build(p)$data[[1]]$ymin + ggplot_build(p)$data[[1]]$ymax) / 2,
  label = as.character(paste(round(ggplot_build(p)$data[[1]]$.wt / sum(ggplot_build(p)$data[[1]]$.wt), 3) * 100, "%"))
)

p + geom_text(data = text_data, aes(x = x, y = y, label = label))

# KẾT LUẬN: Các phân khúc có số lượng khách hàng lớn
# 1. 11.1% khách hàng là Young Single/Couple - Mainstream
# 2. 8.9% khách hàng là Retirees - Mainstream
# Tuy nhiên phân khúc Older Family lại không có số lượng khách hàng lớn, trong khi doanh số bán của phân khúc này là lớn nhất?
# Vậy nên số lượng khách hàng không phải là nguyên nhân giúp doanh số ở phân khúc Older Family phát triển.
# Nguyên nhân có thể do số lượng khách hàng mua nhiều sản phẩm.
# Kiểm tra phần trăm số lượng sản phẩm/số lượng khách hàng 

avg_units <- data_total[, .(AVG=sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE,PREMIUM_CUSTOMER)][order(-AVG)]

ggplot(avg_units,aes(weight=AVG, x=LIFESTAGE,fill=PREMIUM_CUSTOMER)) +
  geom_bar(position=position_dodge()) +
  labs(x= "Lifestage", y="Avg units per transaction", title="Units per Customer") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5))
# KẾT LUẬN: Trung bình số lượng sản phẩm 1 người mua ở phân khúc Older Family và Young Family là lớn nhất

# Kiểm tra giá trung bình cho mỗi sản phẩm ở mỗi phân khúc

avg_price <- data_total[, .(AVG=sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE,PREMIUM_CUSTOMER)][order(-AVG)]

ggplot(avg_price,aes(weight=AVG, x=LIFESTAGE, fill=PREMIUM_CUSTOMER)) +
  geom_bar(position=position_dodge()) +
  labs(x="Lifestage", y="Avg price per unit", title="Price per Unit") +
  theme(axis.text.x = element_text(angle=90,vjust=0.5))

# Kiểm tra ý nghĩa thống kê bằng t-test
pricePerUnit <- data_total[, price:=TOT_SALES/PROD_QTY]
t.test(data_total[LIFESTAGE %in% c("YOUNG SINGLES/COUNPLES","MIDAGE SINGLES/COUPLES")& PREMIUM_CUSTOMER == "Mainstream",price]
       , data_total[LIFESTAGE %in% c("YOUNG SINGLES/COUNPLES","MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream",price]
       , alternative ="greater")
# Hệ số tin cậy p-value = 2.2e-16 <0.001 => có ý nghĩa thống kê, tứclà mức giá trung bình ở phân khúc Mainstream nhóm khách hàng Midage Singles/Couple & Yong Singles/Couples lớn hơn so với phân khúc Budget và Premium trong cùng nhóm khách hàng

# Nghiên cứu sâu hơn: Tìm nhóm khách hàng đóng góp nhiều nhất cho doanh số bán hàng và tìm cách giữ chân họ lại và tăng doanh số thêm.
# Xem xét ở nhóm khách hàng Mainstream (Young Singles/Couples), tìm xem họ có thường xuyên mua một thường hiệu bánh snack cụ thể không? 
segment1 <- data_total[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data_total[LIFESTAGE != "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream", ]

quantity_segment1 <- segment1[,sum(PROD_QTY)]
quantity_other <- other[,sum(PROD_QTY)]

quantity_segment1_by_brand <- segment1[, .(targetSegment=sum(PROD_QTY)/quantity_segment1), by=BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]

brand_proportions <- merge(quantity_segment1_by_brand,quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

# Nhóm YOUNG SINGLES/COUPLES có khả năng mua TYRRELLS cao hơn 23% so với dân số và khả năng mua BURGER KING thấp hơn 56% so với dân số

quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by=PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by=PACK_SIZE]

pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

# Kết luận: Mainstream độ tuổi trẻ độc thân/cặp đôi có khả năng mua gói chips 270g cao hơn 27% so với phần còn lại của dân số.
# Điều này có nghĩa là nhóm Mainstream độ tuổi trẻ độc thân/cặp đôi có khả năng mua gói chips 270g nhiều hơn so với những người khác. 
# Tuy nhiên, để hiểu rõ hơn về nguyên nhân tại sao nhóm này thích gói 270g, ta cần xem xét thêm về các thương hiệu cụ thể bán gói này.

data_total[PACK_SIZE==270, unique(PROD_NAME)]
# Kết luận: Trên thực tế gói sản phẩm có khối lượng 270g chỉ có ở BRAND Twisties 
# vì vậy khả năng mua gói này cao hơn có thể phản ánh sự ưa thích của nhóm Mainstream độ tuổi trẻ độc thân/cặp đôi đối với sản phẩm của Twisties.

### TỔNG KẾT: 
# 1. Doanh số bán hàng chủ yếu do các nhóm khách hàng BUDGET - OLDER FAMILY, MAINSTREAM - YOUNG SINGLES/COUPLES, và MAINSTREAM - RETIREES. 
# Đối với nhóm MAINSTREAM - YOUNG SINGLES/COUPLES và MAINSTREAM - RETIREES doanh số bán lớn là do có nhiều người mua.
# Đối với nhóm BUDGET - OLDER FAMILY doanh số bán lớn là do một người mua với số lượng nhiều.
# 2. Nhóm MAINSTREAM - RETIREES và YOUNG SINGLES/COUPLES có xu hướng sẵn sàng chi trả nhiều hơn cho mỗi gói Chips.
# Thể hiện hành vi mua sẵm tức thì.
# 3. Sau khi phân tích, tiềm nâng nhất chính là nhóm YOUNG SINGLES/COUPLES - MAINSTREAM, họ có khả năng mua Chips cao hơn 23% các nhóm còn lại
#  Quản lý danh mục sản phẩm muốn tăng hiệu suất của danh mục sản phẩm nên bố trí một số sản phẩm TYRRELLS và các gói nhỏ hơn ở các vị trí dễ thấy và dễ tiếp cận hơn đối với những địa điểm mà người mua hàng YOUNG SINGLES/COUPLES thường xuyên ghé thăm để tạo sự thú vị và kích thích hành vi mua sắm tức thì.
# Đặc biệt, gói sản phẩm có trọng lượng 270g được ưa chuộng là do duy nhất BRAND TWISTIES phát triển, chính vì vậy khả năng sản phầm này được mua cao hơn phản ánh sự ưa thích của nhốm MAINSTREAM - YOUNG SINGLES/COUPLES ưa thích sản phẩm của thương hiệu TWISTIES.