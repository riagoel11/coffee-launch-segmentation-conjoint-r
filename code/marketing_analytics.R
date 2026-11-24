#=====================================================================================
# Ria Goel 
# Marketing Analytics 
#=====================================================================================

my_palette <- c(
  "#2C6E8A",  # dark blue
  "#5BA4A4",  # teal
  "#3D9970",  # emerald green
  "#C9B8E8",  # soft lavender
  "#F2E0C8",  # warm cream
  "#7B5EA7",  # plum
  "#F9E784",  # buttercup
  "#FFD1DC",  # blush
  "#F4A89A",  # salmon
  "#B5547A"   # raspberry
)
## Single colour for single-colour plots
my_blue   <- "#2C6E8A"
my_teal   <- "#5BA4A4"
my_green  <- "#A8D5BA"
my_plum   <- "#7B5EA7"

#====================================================================================
#====================================================================================
#                              Part 1 
#====================================================================================
#====================================================================================

# Load Packages 
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(kableExtra)
library(cluster)
library(patchwork)

set.seed(40481810)

#Load Data 
df <- fread("Retail Transaction Data.csv")
#View(df)

#Overall view of data 
head(df)
str(df)
summary(df)

#-----------------------------Data Cleaning-----------------------------------------
#Convert date
df[, InvoiceDate := as.Date(InvoiceDate)]

#Missing Values 
colSums(is.na(df))
unique(df$ProductCategory)

## Remove rows with missing ProductCategory
df <- df[!is.na(ProductCategory)]

#Check for duplicate rows 
any(duplicated(df))
#Remove duplicates
df <- unique(df)

#Check for negatives or zero prices 
df[UnitPrice <= 0]
df[Quantity <= 0]

## Create TotalSpend column
df[, TotalSpend := UnitPrice * Quantity]

## Confirm clean dataset
cat("Rows after cleaning:", nrow(df), "\n")
cat("Unique customers:", uniqueN(df$CustomerID), "\n")
cat("Unique invoices:", uniqueN(df$InvoiceNo), "\n")
cat("Date range:", as.character(min(df$InvoiceDate)), "to", as.character(max(df$InvoiceDate)), "\n")

#===================================================================================
# 1. Exploratory Data Analysis
#===================================================================================

#-----------Customer level table----------------------------------------------------
cust_demo <- df[, .(
  Age           = first(Age),
  Income        = first(Income),
  HouseholdSize = first(HouseholdSize),
  Married       = first(Married),
  Education     = first(Education),
  TotalSpend    = sum(TotalSpend)
), by = CustomerID]

cust_demo[, Education_Label := factor(Education, levels = c(1,2,3),
                                      labels = c("Below Undergrad", 
                                                 "Undergraduate", 
                                                 "Postgraduate"))]
cust_demo[, Married_Label := ifelse(Married == 1, "Married", "Not Married")]

#-----------Descriptive statistics table--------------------------------------------
vars <- c("Age", "Income", "HouseholdSize", "Married", "Education", "TotalSpend")
desc_table <- data.frame(
  Variable = c("Age (years)", "Income (£k)", "Household Size",
               "Married (0/1)", "Education (1-3)", "Total Spend (£)"),
  Mean   = round(sapply(vars, function(v) mean(cust_demo[[v]])), 2),
  Median = round(sapply(vars, function(v) median(cust_demo[[v]])), 2),
  SD     = round(sapply(vars, function(v) sd(cust_demo[[v]])), 2),
  Min    = round(sapply(vars, function(v) min(cust_demo[[v]])), 2),
  Max    = round(sapply(vars, function(v) max(cust_demo[[v]])), 2)
)

kable(desc_table, caption = "Table 1. Customer Descriptive Statistics (N = 922)") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_palette[1], color = "white", bold = TRUE)

#-----------Revenue tables----------------------------------------------------------
cat_revenue <- df[, .(Revenue = sum(TotalSpend), 
                      Transactions = .N), by = ProductCategory]
cat_revenue <- cat_revenue[order(-Revenue)]

df[, Month := format(InvoiceDate, "%Y-%m")]
monthly_rev <- df[, .(Revenue = sum(TotalSpend)), by = Month]
monthly_rev <- monthly_rev[order(Month)]

cat_month <- df[, .(Revenue = sum(TotalSpend)), 
                by = .(ProductCategory, Month)]

#======================Section 1 — Business Context=================================

#-------------------------Plot: Monthly Revenue Trend--------------------------------
ggplot(monthly_rev, aes(x = Month, y = Revenue)) +
  geom_col(fill = my_palette[1], alpha = 0.9) +
  geom_line(aes(group = 1), colour = my_palette[10],
            linewidth = 1, linetype = "dashed") +
  geom_point(colour = my_palette[10], size = 2.5) +
  labs(title = "Figure 3 : Monthly Revenue Trend (Jan–Aug 2025)",
       x = "Month", y = "Total Revenue (£)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold"))

#--------------Plot: Revenue Heatmap (Category by Month)-----------------------------
ggplot(cat_month, aes(x = Month, 
                      y = reorder(ProductCategory, Revenue), 
                      fill = Revenue)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradient(low = my_palette[5], high = my_palette[3],
                      labels = scales::comma) +
  labs(title = "Figure 4 : Revenue Heatmap — Category by Month",
       x = "Month", y = "Product Category",
       fill = "Revenue (£)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#======================Section 2 — Customer Demographics============================


coffee_buyers <- df[ProductCategory == "Coffee"]
coffee_demo <- coffee_buyers[, .(
  Age           = first(Age),
  Income        = first(Income),
  HouseholdSize = first(HouseholdSize),
  Married       = first(Married),
  Education     = first(Education)
), by = CustomerID]

coffee_demo[, Education_Label := factor(Education, levels = c(1,2,3),
                                        labels = c("Below Undergrad", 
                                                   "Undergraduate", 
                                                   "Postgraduate"))]
coffee_demo[, Married_Label := ifelse(Married == 1, "Married", "Not Married")]

#======== Plot: Income by Marital Status (Coffee Buyers)============================
ggplot(coffee_demo, aes(x = Married_Label, y = Income, fill = Married_Label)) +
  geom_boxplot(alpha = 0.7, outlier.colour = my_palette[10], outlier.shape = 16) +
  scale_fill_manual(values = c(my_palette[1], my_palette[7])) +
  labs(title = "Income Distribution by Marital Status (Coffee Buyers)",
       x = "", y = "Income (£k)") +
  theme_classic() +
  theme(legend.position = "none")

#======================Section 3 — Spend Behaviour==================================

#---------------------- Plot: Customer Spend Distribution---------------------------
ggplot(cust_demo, aes(x = TotalSpend)) +
  geom_histogram(bins = 30, fill = my_palette[1], colour = "white", alpha = 0.85) +
  scale_x_log10(labels = scales::comma) +
  geom_vline(aes(xintercept = median(TotalSpend)),
             colour = my_palette[6], linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(cust_demo$TotalSpend) * 1.3, y = 80,
           label = paste0("Median: £", round(median(cust_demo$TotalSpend), 0)),
           colour = my_palette[6], size = 3.5) +
  labs(title = "Customer Total Spend Distribution (Log Scale)",
       x = "Total Spend (£, log scale)", y = "Count") +
  theme_classic()

#-------------- Plot: Revenue Concentration (Top 10% vs Bottom 90%)----------------
cust_demo[, spend_group := ifelse(TotalSpend >= quantile(TotalSpend, 0.9),
                                  "Top 10%", "Bottom 90%")]

top_bottom <- cust_demo[, .(
  Customers     = .N,
  Avg_Spend     = round(mean(TotalSpend), 2),
  Total_Revenue = round(sum(TotalSpend), 2),
  Revenue_Share = round(sum(TotalSpend) / sum(cust_demo$TotalSpend) * 100, 1)
), by = spend_group]

print(top_bottom)

ggplot(top_bottom, aes(x = spend_group, y = Revenue_Share, fill = spend_group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Revenue_Share, "%")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(my_palette[1], my_palette[4])) +
  labs(title = "Figure 2 : Revenue Concentration — Top 10% vs Bottom 90% Customers",
       x = "", y = "Share of Total Revenue (%)") +
  theme_classic() +
  theme(legend.position = "none")
#======================Section 4 — Coffee Buyer Profile=============================

#-------------- Plot: What Else Do Coffee Buyers Purchase (Treemap)-----------------
library(treemapify)
library(patchwork)

coffee_customer_ids <- unique(df[ProductCategory == "Coffee"]$CustomerID)

coffee_basket <- df[CustomerID %in% coffee_customer_ids &
                      ProductCategory != "Coffee",
                    .(Revenue  = sum(TotalSpend),
                      Customers = uniqueN(CustomerID)),
                    by = ProductCategory]

coffee_basket <- coffee_basket[order(-Customers)]
coffee_basket[, Pct := round(Customers/length(coffee_customer_ids)*100, 1)]

ggplot(coffee_basket, aes(area = Customers, fill = Revenue,
                          label = paste0(ProductCategory, "\n", Pct, "%"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 12) +
  scale_fill_gradient(low = my_palette[5], high = my_palette[3],
                      labels = scales::comma) +
  labs(title = "Figure 5: What Else Do Coffee Buyers Purchase?",
       fill = "Revenue (£)") +
  theme_classic()

#-------------- Plots: Demographics of Coffee Buyers (Combined)---------------------
df[, CoffeeBuyer := ifelse(CustomerID %in% coffee_customer_ids,
                           "Coffee Buyer", "Non-Coffee Buyer")]

cust_all <- df[, .(
  Age         = first(Age),
  Income      = first(Income),
  CoffeeBuyer = first(CoffeeBuyer)
), by = CustomerID]

## Plot A: Income violin
p1 <- ggplot(cust_all, aes(x = CoffeeBuyer, y = Income, fill = CoffeeBuyer)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c(my_palette[1], my_palette[7])) +
  labs(title = "Income Distribution",
       x = "", y = "Household Income (£k)") +
  theme_classic() +
  theme(legend.position = "none")

## Plot B: Age boxplot
p2 <- ggplot(cust_all, aes(x = CoffeeBuyer, y = Age, fill = CoffeeBuyer)) +
  geom_boxplot(alpha = 0.7, outlier.colour = my_palette[10]) +
  scale_fill_manual(values = c(my_palette[1], my_palette[7])) +
  labs(title = "Age Distribution",
       x = "", y = "Age") +
  theme_classic() +
  theme(legend.position = "none")

## Plot C: Income histogram facet
p3 <- ggplot(cust_all, aes(x = Income, fill = CoffeeBuyer)) +
  geom_histogram(bins = 20, colour = "white", alpha = 0.8) +
  facet_wrap(~CoffeeBuyer) +
  scale_fill_manual(values = c(my_palette[1], my_palette[7])) +
  labs(title = "Income Distribution by Group",
       x = "Income (£k)", y = "Count") +
  theme_classic() +
  theme(legend.position = "none")

## Combine all three
(p1 | p2) / p3 +
  plot_annotation(
    title    = "Figure 6 : Coffee Buyer Profile vs Non-Coffee Buyers",
    subtitle = "Despite similar demographics, coffee buyers represent a distinct behavioural segment",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

#======================Section 5 — Why Segmentation?================================

#-------------- Plot: Spend Distribution by Household Size (Ridgeline)--------------
library(ggridges)

ggplot(cust_demo, aes(x = TotalSpend, 
                      y = factor(HouseholdSize),
                      fill = factor(HouseholdSize))) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_manual(values = my_palette) +
  labs(title = "Figure 1: Spend Distribution by Household Size",
       subtitle = "Wide variation within groups justifies segmentation",
       x = "Total Spend (£, log scale)",
       y = "Household Size") +
  theme_classic() +
  theme(legend.position = "none")

#-------------- Plot: High Value vs Low Value Customer Profile (Dot plot)-----------

cust_demo[, value_group := ifelse(TotalSpend >= quantile(TotalSpend, 0.75),
                                  "High Value", "Low Value")]

value_profile <- cust_demo[, .(
  Avg_Age      = round(mean(Age), 1),
  Avg_Income   = round(mean(Income), 1),
  Avg_HHSize   = round(mean(HouseholdSize), 1),
  Married_Rate = round(mean(Married)*100, 1)
), by = value_group]

value_long <- tidyr::pivot_longer(value_profile,
                                  cols = -value_group,
                                  names_to  = "Metric",
                                  values_to = "Value")

value_wide <- tidyr::pivot_wider(value_long,
                                 names_from  = value_group,
                                 values_from = Value)

ggplot(value_long, aes(x = Value, y = Metric, colour = value_group)) +
  geom_line(aes(group = Metric), colour = "grey80", linewidth = 2) +
  geom_point(size = 5) +
  scale_colour_manual(values = c(my_palette[1], my_palette[9])) +
  facet_wrap(~ Metric, scales = "free_x", ncol = 1) +
  labs(title    = "Figure 7 :High Value vs Low Value Customer Profile",
       subtitle = "Near-identical demographics despite very different spend — justifies behavioural segmentation",
       x = "Value", y = "",
       colour = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = my_palette[1]),
        strip.text = element_text(colour = "white", face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
#=======================================================================================
#                       RFM Analysis 
#=======================================================================================

# Set reference date (day after last transaction)
ref_date <- as.Date("2025-08-17")

# Calculate Recency, Frequency, Monetary at customer level
rfm <- df[, .(
  recency_days     = as.numeric(ref_date - max(InvoiceDate)),
  number_of_orders = uniqueN(InvoiceNo),
  revenue          = sum(TotalSpend)
), by = CustomerID]

#Quick check
head(rfm)
summary(rfm)

## Number of groups (quintiles)
groups <- 5

## --------------------- INDEPENDENT SORT --------------------------------------------
rfm[, recency_score_indep   := ntile(recency_days * -1, groups)]
rfm[, frequency_score_indep := ntile(number_of_orders, groups)]
rfm[, monetary_score_indep  := ntile(revenue, groups)]

rfm[, rfm_score_indep := paste0(recency_score_indep,
                                frequency_score_indep,
                                monetary_score_indep)]

## ---------------------- SEQUENTIAL SORT --------------------------------------
rfm[, recency_score_seq := ntile(recency_days * -1, groups)]

r_groups  <- NULL
rf_groups <- NULL
temp      <- NULL

for (r in 1:groups) {
  r_groups[[r]] <- rfm[recency_score_seq == r]
  r_groups[[r]][, frequency_score_seq := ntile(number_of_orders, groups)]
  
  for (m in 1:groups) {
    rf_groups[[m]] <- r_groups[[r]][frequency_score_seq == m]
    rf_groups[[m]][, monetary_score_seq := ntile(revenue, groups)]
    temp <- rbind(temp, rf_groups[[m]])
  }
}

rfm_result <- temp[order(CustomerID)]
rfm_result[, rfm_score_seq := paste0(recency_score_seq,
                                     frequency_score_seq,
                                     monetary_score_seq)]

## Check scores
head(rfm_result)
summary(rfm_result[, .(recency_score_seq, frequency_score_seq, monetary_score_seq)])

## Compare independent vs sequential
table(rfm_result[, .(rfm_score_indep, rfm_score_seq)])[1:10, 1:10]

## ---------------------------SEGMENT LABELS --------------------------------------
rfm_result[, segment := case_when(
  recency_score_seq >= 4 & frequency_score_seq >= 4 ~ "Elite Buyers",
  recency_score_seq >= 3 & frequency_score_seq >= 3 ~ "Loyal Customers",
  recency_score_seq >= 4 & frequency_score_seq <= 2 ~ "Recent Customers",
  recency_score_seq <= 2 & frequency_score_seq >= 4 ~ "At Risk",
  recency_score_seq <= 2 & frequency_score_seq <= 2 ~ "Lost Customers",
  TRUE                                               ~ "Potential Loyalists"
)]

##--------------------------- RFM SUMMARY TABLE---------------------------------------
rfm_summary <- rfm_result[, .(
  Customers     = .N,
  Avg_Recency   = round(mean(recency_days), 1),
  Avg_Frequency = round(mean(number_of_orders), 1),
  Avg_Revenue   = round(mean(revenue), 2)
), by = segment][order(-Avg_Revenue)]

print(rfm_summary)

kable(rfm_summary,
      caption = "Table 3. RFM Segment Summary",
      col.names = c("Segment", "Customers", "Avg Recency (days)",
                    "Avg Frequency", "Avg Revenue (£)")) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = "#2C6E8A", color = "white", bold = TRUE)

#------------------------------- VISUALISATIONS -------------------------------------

## Plot 1: Customer count by segment - BAR CHART
ggplot(rfm_result, aes(x = reorder(segment, table(segment)[segment]),
                       fill = segment)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = my_palette) +
  coord_flip() +
  labs(title = "Number of Customers by RFM Segment",
       x = "", y = "Number of Customers") +
  theme_classic() +
  theme(legend.position = "none")

## Plot 2: Average revenue by segment - LOLLIPOP
seg_rev <- rfm_result[, .(AvgRevenue = mean(revenue)), by = segment]

ggplot(seg_rev, aes(x = reorder(segment, AvgRevenue), y = AvgRevenue)) +
  geom_segment(aes(xend = segment, y = 0, yend = AvgRevenue),
               colour = "#2C6E8A", linewidth = 1.2) +
  geom_point(size = 5, colour = "#2C6E8A") +
  coord_flip() +
  labs(title = "Figure 8: Average Revenue by RFM Segment",
       x = "", y = "Average Revenue (£)") +
  theme_classic()

## Plot 3: RFM Heatmap — recency vs frequency coloured by revenue
## Pre-aggregate first
heatmap_data <- rfm_result[, .(AvgRevenue = mean(revenue)), 
                           by = .(recency_score_seq, frequency_score_seq)]

ggplot(heatmap_data, aes(x = factor(recency_score_seq),
                         y = factor(frequency_score_seq),
                         fill = AvgRevenue)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#A8D5BA", high = "#2C6E8A") +
  labs(title = "Figure 9: RFM Heatmap — Avg Revenue by Recency & Frequency Score",
       x = "Recency Score (5 = most recent)",
       y = "Frequency Score (5 = most frequent)",
       fill = "Avg Revenue (£)") +
  theme_classic()

## Compare Sequential vs Independent RFM scoring
rfm_result[, score_diff := ifelse(rfm_score_seq == rfm_score_indep, 
                                  "Same", "Different")]

cat("Customers scored differently by sequential vs independent:\n")
print(table(rfm_result$score_diff))
print(round(prop.table(table(rfm_result$score_diff)) * 100, 1))

## Visualise difference
ggplot(rfm_result, aes(x = recency_score_indep, 
                       y = recency_score_seq,
                       colour = score_diff)) +
  geom_jitter(alpha = 0.5, size = 2) +
  scale_colour_manual(values = c(my_palette[1], my_palette[10])) +
  labs(title = "Figure 10: Sequential vs Independent RFM Score Comparison",
       subtitle = "Shows where the two methods assign customers differently",
       x = "Recency Score (Independent)",
       y = "Recency Score (Sequential)",
       colour = "Score Agreement") +
  theme_classic()

print(table(rfm_result$score_diff))
print(round(prop.table(table(rfm_result$score_diff)) * 100, 1))
#=====================================================================================
#                             Clustering Analysis 
#=====================================================================================

# Merge RFM results with customer demographics
# We cluster on BOTH behavioural (RFM) and demographic variables
cluster_data <- merge(
  rfm_result[, .(CustomerID, recency_days, number_of_orders, revenue,
                 recency_score_seq, frequency_score_seq, monetary_score_seq)],
  cust_demo[, .(CustomerID, Age, Income, HouseholdSize, Married, Education)],
  by = "CustomerID"
)

# Select variables for clustering
# We use RFM scores + key demographics
var.name <- c("recency_days", "number_of_orders", "revenue",
              "Age", "Income", "HouseholdSize")

# Standardise all variables (essential before clustering)
# Different scales (days vs £ vs count) would bias results
df_scaled <- scale(cluster_data[, ..var.name], 
                   center = TRUE, scale = TRUE)

## Check scaled data
head(df_scaled)

## Remove outliers for cleaner clustering
cluster_data_clean <- cluster_data[revenue < quantile(revenue, 0.99)]

## Rescale clean data
df_scaled_clean <- scale(
  as.data.frame(cluster_data_clean)[, var.name],
  center = TRUE, scale = TRUE
)

#------------------------Hierarchal Clustering------------------------------------

#Calculating the Euclidean distance between all the customers 
dist_matrix <- dist(df_scaled_clean, method = "euclidean")
## Printing distance matrix sample (first 10 customers)
cat("Distance matrix sample (first 10 customers):\n")
print(round(as.matrix(dist_matrix)[1:10, 1:10], 2))

#Running hierarchal clustering using Ward's method
hclust_result <- hclust(dist_matrix, method = "ward.D2")

#Plot dendogram
plot(hclust_result, main = "Dendogram- Hierarchal Clustering", xlab = "Customers",
     ylab= "Height (dissimilarity)", cex= 0.3, hang=-1)

#----------------------Elbow Plot--------------------------------------------------

sort_height = data.frame(x = c(1:length(hclust_result$height)), 
                         height = sort(hclust_result$height, decreasing = TRUE))
ggplot(sort_height[1:15, ], aes(x = x, y = height)) +
  geom_line(colour = "#89A6C9", linewidth = 1) +
  geom_point(colour = "#89A6C9", size = 3) +
  labs(title = "Figure 14: Elbow Plot — Hierarchical Clustering",
       x = "Step",
       y = "Height (dissimilarity)") +
  theme_classic()

# ------------------K means Clustering---------------------------------------------
set.seed(40481810)
#Running kmeans with k=4 
#fist only doing with nstart=1 
kmeans_k4 <- kmeans(x= df_scaled_clean, centers= 4, nstart=1)

#Trying with multiple nstart values for more stable results 

set.seed(40481810)

kmeans_n1 <- kmeans(x= df_scaled_clean, centers= 4, nstart=1)
kmeans_n10 <- kmeans(x= df_scaled_clean, centers= 4, nstart=10)
kmeans_n25 <- kmeans(x= df_scaled_clean, centers= 4, nstart=25)                 
kmeans_n50 <- kmeans(x= df_scaled_clean, centers= 4, nstart=50)

# Comparing total within-cluster sum of squares across nstart values
n_comparison <- data.frame(nstart   = c(1, 10, 25, 50),tot_withinss = c(
  kmeans_n1$tot.withinss, kmeans_n10$tot.withinss,kmeans_n25$tot.withinss,kmeans_n50$tot.withinss))
print(n_comparison)

#Using nstart = 50 as a final model

set.seed(40481810)
kmeans_final <- kmeans(x= df_scaled_clean, centers= 4, nstart=50)

#fix by AI 
cluster_data_clean$cluster <- as.factor(kmeans_final$cluster)

cat("Cluster sizes:\n")
print(table(cluster_data_clean$cluster))


#------------------------Silhouette Score-------------------------------------------
sil <- silhouette(kmeans_final$cluster, dist_matrix)
cat("Average silhouette score:", mean(sil[, 3]), "\n")

#Silhouette plot
plot(sil, col = c(my_blue, my_teal, my_green, my_plum),
     border = NA,
     main = " Silhouette Plot, K-means K=4") 

#-----------------------Visualisations----------------------------------------------

#Plot 1 : Clusters on revenue vs Recency 
ggplot(cluster_data_clean, aes(x= recency_days, y = revenue, colour = cluster, ))+
  geom_point(alpha= 0.6, size= 2)+
  scale_colour_manual(values= my_palette)+
  labs(title = "Figure 11: Customer Clusters  - Recency vs Revenue ", x = "Recency (days since last purchase)",
       y = "Total Revenue (£)",
       colour = "Cluster") +
  theme_classic()

## Check silhouette scores for K=2 to 6
sil_scores <- data.frame(K = 2:6, AvgSilhouette = NA)

for(k in 2:6){
  set.seed(40481810)
  km <- kmeans(df_scaled_clean, centers = k, nstart = 50)
  sil <- silhouette(km$cluster, dist_matrix)
  sil_scores[sil_scores$K == k, "AvgSilhouette"] <- round(mean(sil[,3]), 3)
}

print(sil_scores)

## Plot 2: Clusters on Frequency vs Revenue  
ggplot(cluster_data_clean, aes(x = number_of_orders, y = revenue,
                               colour = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_colour_manual(values = my_palette) +
  labs(title = "Customer Clusters — Frequency vs Revenue",
       x = "Number of Orders",
       y = "Total Revenue (£)",
       colour = "Cluster") +
  theme_classic()

## Plot 3: Cluster sizes — bar chart
ggplot(cluster_data_clean, aes(x = cluster, fill = cluster)) +
  geom_bar() +
  scale_fill_manual(values = my_palette) +
  labs(title = "Number of Customers per Cluster",
       x = "Cluster", y = "Count") +
  theme_classic() +
  theme(legend.position = "none")

##===================== Cluster profiling summary====================================
cluster_profile <- cluster_data_clean %>%
  group_by(cluster) %>%
  summarise(
    Customers      = n(),
    Avg_Recency    = round(mean(recency_days), 1),
    Avg_Orders     = round(mean(number_of_orders), 1),
    Avg_Revenue    = round(mean(revenue), 2),
    Avg_Age        = round(mean(Age), 1),
    Avg_Income     = round(mean(Income), 1),
    Avg_HHSize     = round(mean(HouseholdSize), 1),
    Married_Rate   = round(mean(Married) * 100, 1),
    Avg_Education  = round(mean(Education), 2)
  )

print(cluster_profile)



#====================Customer Profiling==============================================

#Nmaing the clusters 
cluster_data_clean[, cluster_name := case_when(cluster ==1 ~ "Wealthy Disengaged",
                                               cluster == 2 ~ "Star Customers",
                                               cluster == 3 ~ "Mainstream Regulars",
                                               cluster == 4 ~ "Lapsing Customers")]

table(cluster_data_clean$cluster_name)

## Plot 1: Average RFM metrics by cluster — grouped bar chart
rfm_long <- cluster_data_clean %>%
  group_by(cluster_name) %>%
  summarise(
    Recency   = round(mean(recency_days), 1),
    Frequency = round(mean(number_of_orders), 1),
    Revenue   = round(mean(revenue), 2)
  ) %>%
  tidyr::pivot_longer(cols = c(Recency, Frequency, Revenue),
                      names_to = "Metric",
                      values_to = "Value")

ggplot(rfm_long, aes(x = cluster_name, y = Value, fill = cluster_name)) +
  geom_col() +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Figure 12 : RFM Metrics by Customer Segment",
       x = "", y = "Value", fill = "Segment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "none")

## Plot 2: Income vs Revenue by cluster — scatter with labels
ggplot(cluster_data_clean, aes(x = Income, y = revenue,
                               colour = cluster_name)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_colour_manual(values = my_palette) +
  labs(title = "Income vs Total Revenue by Segment",
       x = "Household Income (£k)",
       y = "Total Revenue (£)",
       colour = "Segment") +
  theme_classic()

## Plot 3: Age distribution by cluster — density plot
ggplot(cluster_data_clean, aes(x = Age, fill = cluster_name)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = my_palette) +
  labs(title = "Figure 13 : Age Distribution by Customer Segment",
       x = "Age", y = "Density",
       fill = "Segment") +
  theme_classic()

#customer segmentation summary 
kable(cluster_profile,
      caption = "Table 5. Customer Segment Profile Summary",
      col.names = c("Cluster", "Customers", "Avg Recency (days)",
                    "Avg Orders", "Avg Revenue (£)", "Avg Age",
                    "Avg Income (£k)", "Avg HH Size",
                    "Married (%)", "Avg Education")) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_blue, color = "white", bold = TRUE) %>%
  row_spec(2, background = "#EBF5FB") ## Highlight Star Customers row


#===================================================================================
#.              Linear Discriminant Analysis
#===================================================================================
library(MASS)

#One row per customer , demographic variables 
lda_data <- cluster_data_clean[, .(cluster,
                                   Age, Income, HouseholdSize, Married, Education
)]

table(lda_data$cluster)

##Fit LDA 
fit <- lda(cluster ~ Age + Income + HouseholdSize +  Married + Education,data = lda_data)
print(fit)

#Checking which discriminant functions are significnt 
ldaPred <- predict(fit, lda_data)
ld <- ldaPred$x

#ANNOVA test for each discriminant function 
anova(lm(ld[,1] ~ lda_data$cluster))
anova(lm(ld[,2] ~ lda_data$cluster))
anova(lm(ld[,3] ~ lda_data$cluster))

## LDA Discriminant Function Plot
ld_df <- data.frame(
  LD1     = ld[, 1],
  LD2     = ld[, 2],
  cluster = lda_data$cluster
)

ld_df$cluster_name <- factor(ld_df$cluster,
                             levels = c(1,2,3,4),
                             labels = c("Wealthy Disengaged", "Star Customers", 
                                        "Mainstream Regulars", "Lapsing Customers"))

ggplot(ld_df, aes(x = LD1, y = LD2, colour = cluster_name)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(level = 0.95, linewidth = 0.8) +
  scale_colour_manual(values = my_palette) +
  labs(title = "Figure 15: LDA - Discriminant Function Plot",
       subtitle = "Ellipses show 95% confidence regions — confirms cluster separation",
       x = "LD1", y = "LD2",
       colour = "Segment") +
  theme_classic()
#---------------------Confusion Matrix----------------------------------------------
pred_cluster <- predict(fit)$class
conf_matrix <- table(Actual    = lda_data$cluster,
                     Predicted = pred_cluster)
print(conf_matrix)
kable(conf_matrix,
      caption = "Table 9: LDA Confusion Matrix") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_palette[1], color = "white", bold = TRUE)

# Overall accuracy
accuracy <- sum(diag(conf_matrix)) / nrow(lda_data)
cat("Overall Accuracy:", round(accuracy * 100, 1), "%\n")

#Per-cluster accuracy 
cat("\nPer-cluster accuracy:\n")
per_cluster <- round(diag(conf_matrix) /
                       rowSums(conf_matrix) * 100, 1)
print(per_cluster)

#-----------------Load prospect data ----------------------------------------------

prospects <- fread("Prospect customers info.csv")

head(prospects)
str(prospects)
summary(prospects$ZipCode)

# Fix zip codes in prospects
prospects[, ZipCode := sprintf("%05d", ZipCode)]

## Verify
min(nchar(prospects$ZipCode))  ## Should return 5
head(prospects$ZipCode)         ## Should show leading zeros

#Classigy prospects
pred_prospects <- predict(fit, prospects)$class

#Adding predictions to prospect data 
prospects[, predicted_cluster:= pred_prospects]

## Add segment names
prospects[, segment_name := case_when(
  predicted_cluster == 1 ~ "Wealthy Disengaged",
  predicted_cluster == 2 ~ "Star Customers",
  predicted_cluster == 3 ~ "Mainstream Regulars",
  predicted_cluster == 4 ~ "Lapsing Customers"
)]

## Count prospects per segment
cat("Prospect distribution by segment:\n")
print(table(prospects$segment_name))


#----------------Prospect Summary ---------------------------------------------------
prospect_summary <- prospects[, .N, by = segment_name][order(-N)]
prospect_summary[, Percentage := round(N/sum(N)*100, 1)]

kable(prospect_summary,
      caption = "Table 7: Prospect Classification by Segment",
      col.names = c("Segment", "Prospects", "Percentage (%)")) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_blue, color = "white", bold = TRUE)


ggplot(prospects, aes(x = segment_name, fill = segment_name)) +
  geom_bar() +
  scale_fill_manual(values = my_palette) +
  labs(title = "Prospect Distribution by Predicted Segment",
       x = "Segment", y = "Number of Prospects") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

#-------------Target Segment Selection-----------------------------------------------

## Extract Star_Customers prospects for targeting
Star_Customers_prospects <- prospects[segment_name == "Star Customers"]
cat("Number of Star_Customers prospects to target:", 
    nrow(Star_Customers_prospects), "\n")

## Summary of Star_Customers prospects demographics
Star_Customers_summary <- Star_Customers_prospects[, .(
  Avg_Age       = round(mean(Age), 1),
  Avg_Income    = round(mean(Income), 1),
  Avg_HHSize    = round(mean(HouseholdSize), 1),
  Married_Rate  = round(mean(Married) * 100, 1),
  Avg_Education = round(mean(Education), 2)
)]

print(Star_Customers_summary)

kable(Star_Customers_summary,
      caption = "Table 8. Demographic Profile of Star Customers Prospects",
      col.names = c("Avg Age", "Avg Income (£k)", 
                    "Avg HH Size", "Married (%)", 
                    "Avg Education")) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_blue, color = "white", bold = TRUE)

#======================================================================================
#======================================================================================
# Part 2 
#======================================================================================
#======================================================================================

#======================================================================================
# Conjoint Analysis - Parts-worth & WTP
#======================================================================================

#Load data 
profiles <- fread("Product profiles.csv")
survey <- fread("Conjoint survey results-1.csv")

#checks 
nrow(survey)     
uniqueN(survey$respondent_id)  

#Merge profiles with survey ratings 
conjoint <- merge(survey, profiles, by = "productNo")

#Check merged data 
head(conjoint)
nrow(conjoint)

#---------------------Set a reference level-----------------------------------------

set.seed(40481810)
# Convert to dataframe to avoid data.table factor issues
conjoint <- as.data.frame(conjoint)

conjoint <- conjoint[, c("productNo", "respondent_id", "price.x", 
                         "format.x", "strength.x", "origin.x", 
                         "sustainability.x", "rating")]

## Rename to clean names
names(conjoint) <- c("productNo", "respondent_id", "price", 
                     "format", "strength", "origin", 
                     "sustainability", "rating")

## Verify
head(conjoint)
names(conjoint)
unique(conjoint$format)
unique(conjoint$strength)
unique(conjoint$origin)
unique(conjoint$sustainability)

conjoint$format         <- relevel(factor(conjoint$format),  ref = "Capsule")
conjoint$strength       <- relevel(factor(conjoint$strength),  ref = "Mild")
conjoint$origin         <- relevel(factor(conjoint$origin),ref = "House blend")
conjoint$sustainability <- relevel(factor(conjoint$sustainability),  ref = "No")
conjoint$price          <- as.numeric(conjoint$price)

## Verify
levels(conjoint$format)
levels(conjoint$strength)
levels(conjoint$origin)
levels(conjoint$sustainability)

#===================Individual OLS===================================================
resp <- unique(conjoint$respondent_id)
b <- NULL 

for (i in resp){
  ind.ols <- lm(rating ~ format + strength + origin + sustainability + price,
                data = conjoint[conjoint$respondent_id == i, ])
  b<- rbind(b, ind.ols$coefficients)
}

#Average part-worths 
avg.b <- colMeans(b)
cat("Average part-worths (individual OLS):\n")
print(round(avg.b, 3))

## Individual Part-worth Heterogeneity
b_df <- as.data.frame(b)
b_df_long <- tidyr::pivot_longer(b_df,cols = everything(), names_to  = "attribute",
                                 values_to = "partworth")

ggplot(b_df_long, aes(x = partworth, fill = attribute)) +
  geom_histogram(bins = 30, colour = "white", alpha = 0.8) +
  facet_wrap(~ attribute, scales = "free") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Fgure 19: Distribution of Individual Part-worths",
       subtitle = "Preference heterogeneity across 200 respondents",
       x = "Part-worth", y = "Count") +
  theme_classic() +
  theme(legend.position = "none")

#==================Aggregate OLS====================================================
ols <- lm(rating ~ format + strength + origin + sustainability + price,data = conjoint)

part.worths <- ols$coefficients
cat("\nAggregated part-worths:\n")
print(round(part.worths, 3))

#------------------Visulaisations + WTP-----------------------------------------------

pw <- data.table(
  attribute = c("intercept",rep("format", 3), rep("strength", 2),rep("origin", 2),
                rep("sustainability", 1),rep("price", 1)),
  level = c("(Intercept)","Ground", "Instant", "Whole bean","Dark", "Medium",
            "100% Arabica blend", "Single-origin","Yes","price (per £1)"),
  partworth = as.numeric(part.worths)
)
#Add base levels 

bases <- data.table(
  attribute = c("format", "strength", "origin", "sustainability"),
  level     = c("Capsule", "Mild", "House blend", "No"),
  partworth = rep(0, 4)
)

pw_plot <- rbind(pw[-1], bases)

## Plot part-worths
ggplot(pw_plot, aes(x = partworth, y = reorder(level, partworth), fill = attribute)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = round(partworth, 3)),
            hjust = ifelse(pw_plot$partworth >= 0, -0.1, 1.1), size = 3) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Figure 16: Average Part-worths by Attribute Level",
    x = "Part-worth",
    y = "Attribute Level",
    fill = "Attribute"
  ) +
  theme_classic()

#-------------------WTP Calculation --------------------------------------------------

price_coef <- abs(part.worths["price"])  # 0.372
## WTP for each attribute level
wtp_table <- pw_plot[attribute != "intercept" & attribute != "price"]
wtp_table[, WTP := partworth / price_coef]

print(wtp_table[order(attribute, -WTP)][, WTP := round(WTP, 3)])

##Visulaise WTP 
ggplot(wtp_table, aes(x = WTP, y = reorder(level, WTP), fill = attribute)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = paste0("£", round(WTP, 2))),
            hjust = ifelse(wtp_table$WTP >= 0, -0.15, 1.15), size = 3) +
  scale_fill_manual(values = my_palette) +
  scale_x_continuous(limits = c(-6, 2)) +
  labs(
    title = "Figure 17 : Willingness to Pay by Attribute Level",
    x = "WTP (£ vs base level)",
    y = "Attribute Level",
    fill = "Attribute"
  ) +
  theme_classic()

#===================================================================================
#                       Attribute Importance
#===================================================================================

## Calculate importance per attribute
imp <- pw_plot[, .(imp.value = max(partworth) - min(partworth)), by = attribute]

## Fixed price separately (Debugged by AI )
imp <- rbind(imp[attribute != "price"], 
             data.table(attribute = "price", imp.value = abs(part.worths["price"]) * (6 - 3)))

## Calculate importance ratio (percentage)
imp[, imp.ratio := imp.value / sum(imp.value) * 100]
## Sort by importance
imp <- imp[order(-imp.ratio)]

print(imp)

## Plot Attribute Importance 
ggplot(imp, aes(x = imp.ratio, y = reorder(attribute, imp.ratio), fill = attribute)) +
  geom_col() +
  geom_text(aes(label = paste0(round(imp.ratio, 1), "%")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = my_palette) +
  scale_x_continuous(limits = c(0, 70)) +
  labs(
    title = "Figure 18 : Attribute Importance in Coffee Purchase Decision",
    x = "Importance (%)",
    y = "Attribute",
    fill = "Attribute"
  ) +
  theme_classic() +
  theme(legend.position = "none")

#===================================================================================
#                      Market Share Prediction 
#===================================================================================
## ----------- Define competitor products currently in the market ------------------
competitors <- data.frame(
  brand          = c("Competitor A", "Competitor B", "Competitor C"),
  format         = c("Capsule",      "Capsule",      "Capsule"),
  strength       = c("Medium",       "Dark",         "Mild"),
  origin         = c("House blend",  "100% Arabica blend", "Single-origin"),
  sustainability = c("No",           "No",           "Yes"),
  price          = c(4,              6,               5)
)

## ----------------- Baseline market share WITHOUT PB Coffee -----------------------
competitors_only <- competitors %>%
  mutate(
    format         = factor(format, levels = levels(conjoint$format)),
    strength       = factor(strength, levels = levels(conjoint$strength)),
    origin         = factor(origin, levels = levels(conjoint$origin)),
    sustainability = factor(sustainability, levels = levels(conjoint$sustainability))
  )

X_only      <- model.matrix(~ format + strength + origin + sustainability + price, data = competitors_only)
common_only <- intersect(colnames(b), colnames(X_only))
U_only      <- b[, common_only] %*% t(X_only[, common_only])
colnames(U_only) <- competitors_only$brand

choice_only   <- apply(U_only, 1, which.max)
ms_without_pb <- prop.table(table(choice_only))
names(ms_without_pb) <- competitors_only$brand[as.numeric(names(ms_without_pb))]
cat("Baseline Market Share WITHOUT PB Coffee:\n")
print(round(ms_without_pb * 100, 1))

expU_only        <- exp(U_only)
ms_logit_without <- colMeans(expU_only / rowSums(expU_only)) * 100
cat("\nBaseline Logit Market Share WITHOUT PB Coffee:\n")
print(round(ms_logit_without, 1))
#------------Define your PB product-------------------------------------------------
pb_product <- data.frame(brand = "PB Coffee",format = "Capsule",strength  = "Dark",
                         origin = "100% Arabica blend",sustainability = "Yes",price  = 5)
all_products <- rbind(competitors, pb_product)

#Match factor levels to conjoint data 
all_products <- all_products %>%
  mutate(
    format         = factor(format,         levels = levels(conjoint$format)),
    strength       = factor(strength,       levels = levels(conjoint$strength)),
    origin         = factor(origin,         levels = levels(conjoint$origin)),
    sustainability = factor(sustainability, levels = levels(conjoint$sustainability))
  )

#-------------------- Build Product Matrix -----------------------------------------

X <- model.matrix(~ format + strength + origin + sustainability + price,
                  data = all_products)
# Align individual pw with product matrix 
common <- intersect(colnames(b), colnames(X))
b_MS   <- b[, common, drop = FALSE]
X_MS   <- X[, common, drop = FALSE]

#Estimate utility 
U <- b_MS %*% t(X_MS)
colnames(U) <- all_products$brand

#First Choice Rule - Maximum utility 
choice         <- apply(U, 1, which.max)
ms_firstchoice <- prop.table(table(choice))
names(ms_firstchoice) <- all_products$brand[as.numeric(names(ms_firstchoice))]

cat("Market Share — First Choice Rule:\n")
print(round(ms_firstchoice * 100, 1))

#-------------------Logit Share Rule-----------------------------------------------
expU          <- exp(U)
prob          <- expU / rowSums(expU)
ms_logit      <- colMeans(prob) * 100

cat("\nMarket Share — Logit Rule:\n")
print(round(ms_logit, 1))

##Visualise Market share estimates 
ms_df <- rbind(
  data.table(brand = names(ms_firstchoice), share = as.numeric(ms_firstchoice) * 100, method = "First Choice"),
  data.table(brand = all_products$brand, share = ms_logit, method = "Logit")
)

ggplot(ms_df, aes(x = brand, y = share, fill = brand)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = my_palette) +
  coord_flip(ylim = c(0, 85)) +
  facet_wrap(~ method) +
  labs(
    title = "Figure 20 : Predicted Market Share With and Without PB Coffee",
    x = "Product",
    y = "Market Share (%)",
    fill = "Product"
  ) +
  theme_classic() +
  theme(legend.position = "none")

#===================================================================================
#               PCA and Perceptual Mapping (Perceptual Variables Only)
#===================================================================================

library(ggrepel)

## Load data
attr_data <- fread("Product attributes information.csv")

#Perceptual variables + 3 
perc_vars <- c("convenience", "authenticity", "premium", 
               "perceived_sustainability", "taste_quality", "price","strength_level",
               "sustaintability_claim")

data_pca <- as.data.frame(attr_data)[, perc_vars]
rownames(data_pca) <- attr_data$product_name

## Check
head(data_pca)

## Run PCA
pca <- prcomp(data_pca, retx = TRUE, scale = TRUE)

## Singular values
cat("Singular values:\n")
print(pca$sdev)

## Loading factors
cat("\nLoading factors (rotation matrix):\n")
print(round(pca$rotation, 3))
## Loading factors as formatted table
loadings_table <- as.data.frame(round(pca$rotation, 3))
loadings_table <- loadings_table[, 1:3]  # show first 3 PCs

kable(loadings_table,caption = "Table11: PCA Loading Factors (PC1-PC3)",
      col.names = c("Attributes", "PC1", "PC2", "PC3")) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, background = my_palette[1], color = "white", bold = TRUE)

## PVE — Proportion of Variance Explained
var_explained <- (pca$sdev^2) / sum(pca$sdev^2)
cat("\nProportion of Variance Explained (PVE):\n")
print(round(var_explained, 3))

cat("\nCumulative PVE:\n")
print(round(cumsum(var_explained), 3))

## Full summary
summary(pca)

## Scree plot
plot(pca, type = "l", main = "Scree Plot — Perceptual Variables")

## Extract scores and loadings
pca_scores <- data.table(
  product = rownames(data_pca),
  PC1     = pca$x[, 1] / max(abs(pca$x[, 1])),
  PC2     = pca$x[, 2] / max(abs(pca$x[, 2]))
)

pca_loadings <- data.table(
  attribute = colnames(data_pca),
  PC1       = pca$rotation[, 1] * pca$sdev[1],
  PC2       = pca$rotation[, 2] * pca$sdev[2]
)

## Clean ggplot perceptual map
ggplot() +
  ## Attribute arrows
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 0.7, colour = my_palette[1]) +
  geom_text_repel(data = pca_loadings,
                  aes(x = PC1 * 1.15, y = PC2 * 1.15, label = attribute),
                  size = 3.5, colour = my_palette[1],
                  fontface = "bold",
                  box.padding = 0.5,
                  max.overlaps = 20) +
  
  ## Product points
  geom_point(data = pca_scores,
             aes(x = PC1, y = PC2),
             size = 2.5, colour = my_palette[3]) +
  geom_text_repel(data = pca_scores,
                  aes(x = PC1, y = PC2, label = product),
                  size = 2.8, colour = "grey30",
                  box.padding = 0.4,
                  max.overlaps = 20) +
  
  ## Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  
  labs(
    x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
    title = "Figure 21: Product Perceptual Map(without PB)",
    subtitle = "Based on 8 customer perceptioual and product dimensions"
  ) +
  theme_classic()

#=====================================================================================
# Project PB Coffee onto Perceptual map 
#=====================================================================================

## Profile: Capsule, Dark, Single-origin, Sustainable, £5
pb_attributes <- data.frame( convenience              = 6.0,  # capsule = very convenient
                             authenticity             = 5.5,  # single-origin = high authenticity
                             premium                  = 5.0,  # mid-premium PB product
                             perceived_sustainability = 6.0,  # sustainable claim = high perception
                             taste_quality            = 5.5,  # dark roast = high taste quality
                             price                    = 5.0,  # £5 from brief
                             strength_level           = 7,    # dark roast = 7
                             sustaintability_claim    = 1     # yes sustainable
)

#scaled using same mean and SD as original pca 
pb_scaled <- scale(pb_attributes, center = pca$center, scale  = pca$scale)

##Project onto PCA space
pb_projected <- as.data.frame(pb_scaled %*% pca$rotation)

pb_scores <- data.table( product = "PB Coffee",
                         PC1     = pb_projected$PC1 / max(abs(pca$x[, 1])),
                         PC2     = pb_projected$PC2 / max(abs(pca$x[, 2]))
)

print(pb_scores)

##Plot with PB coffee added 
ggplot() +
  ## Attribute arrows
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 0.7, colour = my_palette[1]) +
  geom_text_repel(data = pca_loadings,
                  aes(x = PC1 * 1.15, y = PC2 * 1.15, label = attribute),
                  size = 3.5, colour = my_palette[1],
                  fontface = "bold",
                  box.padding = 0.5,
                  max.overlaps = 20) +
  
  ## Existing products
  geom_point(data = pca_scores,
             aes(x = PC1, y = PC2),
             size = 2.5, colour = my_palette[3]) +
  geom_text_repel(data = pca_scores,
                  aes(x = PC1, y = PC2, label = product),
                  size = 2.8, colour = "grey30",
                  box.padding = 0.4,
                  max.overlaps = 20) +
  
  ## PB Coffee — highlighted
  geom_point(data = pb_scores,
             aes(x = PC1, y = PC2),
             size = 6, colour = my_palette[6],
             shape = 18) +
  geom_text_repel(data = pb_scores,
                  aes(x = PC1, y = PC2, label = product),
                  size = 4.5, colour = my_palette[6],
                  fontface = "bold",
                  box.padding = 0.8) +
  
  ## Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  
  labs(
    x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
    title = "Figure 22: Product Perceptual Map with PB Coffee Positioning",
    subtitle = "PB Coffee targets the sustainable + premium white space"
  ) +
  theme_classic()


