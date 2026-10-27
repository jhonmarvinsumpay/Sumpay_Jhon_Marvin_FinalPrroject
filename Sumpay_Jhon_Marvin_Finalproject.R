shop <- read.csv("computer_shop_dataset.csv", stringsAsFactors = FALSE)
shop
str(shop)   # expect 20 observations of 6 variables

storage_stock <- subset(shop, Category == "Storage" & Stock_Qty >= 10)
storage_stock

top_storage <- storage_stock[which.max(storage_stock$Price_PHP), ]
top_storage

storage_ranked <- storage_stock[order(-storage_stock$Price_PHP), ]

storage_summary <- data.frame(
  average = mean(storage_stock$Price_PHP),
  median  = median(storage_stock$Price_PHP),
  lowest  = min(storage_stock$Price_PHP),
  highest = max(storage_stock$Price_PHP)
)

category_summary <- aggregate(cbind(Price_PHP, Stock_Qty) ~ Category, shop,
                              function(x) round(mean(x), 1))
names(category_summary) <- c("Category", "Avg_Price_PHP", "Avg_Stock_Qty")

top_by_category <- shop[
  ave(shop$Price_PHP, shop$Category, FUN = max) == shop$Price_PHP, ]
top_by_category <- top_by_category[order(top_by_category$Category), ]

shop$Inventory_Value <- shop$Price_PHP * shop$Stock_Qty
inventory_value <- shop[order(-shop$Inventory_Value),
                        c("Product_ID", "Product_Name", "Category",
                          "Price_PHP", "Stock_Qty", "Inventory_Value")]

storage_ranked
storage_summary
category_summary
top_by_category
head(inventory_value, 5)

write.csv(top_storage,       "Highest_Storage_Result.csv",       row.names = FALSE)
write.csv(storage_ranked,    "Storage_Ranked_Results.csv",       row.names = FALSE)
write.csv(category_summary,  "Category_Summary.csv",             row.names = FALSE)
write.csv(top_by_category,   "Top_Priced_Item_by_Category.csv",  row.names = FALSE)
write.csv(inventory_value,   "Inventory_Value_by_Item.csv",      row.names = FALSE)