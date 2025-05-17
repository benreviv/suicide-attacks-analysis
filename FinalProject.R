
#הכנות
df <- read.csv("C:/Users/bebre/Desktop/UNI/YEAR1/Rproject/suicide_attacks.csv")
df_copy <- df
install.packages("dplyr")
install.packages("tidyverse")
install.packages("plotrix")
install.packages("boot")
install.packages("webr")
install.packages("maps")
install.packages("mapproj")
install.packages("plotly")
library(plotly)
library(maps)
library(mapproj)
library(webr)
library(tidyverse)
library(dplyr)
library(plotrix)
library(boot)



#הכנות ראשונות - סידור המידע
# איחוד ארגונים הכתובים בשמות שונים ומייצגים את אותו ארגון. בוצע בעזרת גיטהאב
df_copy <- df_copy %>%
  mutate(groups = ifelse(grepl("^Islamic State", groups, ignore.case = TRUE), 
                         "Islamic State", groups))
df_copy <- df_copy %>%
  mutate(groups = ifelse(grepl("^Al-Qaeda", groups, ignore.case = TRUE), 
                         "Al-Qaeda", groups))
df_copy <- df_copy %>%
  mutate(groups = ifelse(grepl("^Boko Haram", groups, ignore.case = TRUE), 
                         "Boko Haram", groups))
df_copy <- df_copy %>%
  mutate(groups = ifelse(grepl("^Jundullah", groups, ignore.case = TRUE), 
                         "Jundullah", groups))
df_copy <- df_copy %>%
  mutate(groups = ifelse(grepl("^Taliban", groups, ignore.case = TRUE), 
                         "Taliban", groups))

# הסרת שורות המייצגות את אותו פיגוע על ידי איחוד לפי נ"צ ותאריך של הפיגוע
df_copy <- df_copy %>% 
  distinct(target.latitude, target.longtitude,date.year,date.month,date.day, .keep_all = TRUE)
total_killed <- df_copy %>%
  group_by(groups) %>%
  summarise(total_killed = sum(statistics...killed_high, na.rm = TRUE),count = n())
top_10 <- slice_max(total_killed,order_by = total_killed, n=10) %>%
  mutate(avg_per_atk = total_killed / count )

#בוטסטראפ לשאלה 1, לפי הקוד המופיע במודל:
DEATHS <- c(df_copy$statistics...killed_high)
hist(DEATHS)
mean(DEATHS)
N <- length(DEATHS)
B <- 5000
means <- vector("numeric",length = B)
for (b in seq_len(B)) {
  deaths_boot <- sample(DEATHS, size = N, replace = TRUE)
  means[b] <- mean(deaths_boot)
}
hist(means,xlim = c(0,100))
hist(means)
quantile(means,probs = c(0.05,0.95))




#גרף 2- נשקים
#מקורות: Plot.ly/ggplot2/ , Youtube: "R Programming 101"

#יצירת הגרף ועיצוב
weapon_type_isis <- df_copy %>% 
  filter(groups == "Islamic State",!(target.weapon %in% c("Other PBIED","Other VBIED","Unspecified","Unspecified PBIED","Non-suicide IED", "Mixed"))) #מחיקת כלי נשק לא ידועים
ggplot(data = weapon_type_isis, aes(x = target.weapon, y = statistics...killed_high)) +
  geom_point(aes(color = target.weapon), size = 2) +
  labs(x = 'כלי נשק', y = "כמות הרוגים", title = "כמויות הרוגים לפי נשק פר פיגוע") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle=45, hjust=0.9,size =10),legend.position = 'none',plot.title = element_text(hjust = 0.5),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))

#חישוב ממוצעים וסטיות תקן
weapon_mean_sd <- weapon_type_isis %>% 
  group_by(target.weapon) %>% 
  summarise(total_deaths = sum(statistics...killed_high), mean = mean(statistics...killed_high), sd = sd(statistics...killed_high), n=n()) 

#יצירת גרף שני
#בחרנו לגרף זה להשמיט פיגועי "קיצון" עם מעל ל-100 הרוגים ובנוסך להשמיט כלי נשק עם פחות מ-20 תצפיות
#לכן, יצרנו טבלת אונלי אייסיס חדשה הכוללת רק פיגועים עם מספר הרוגים קטן ממאה
#לאחר מכן יצרנו עוד טבלה על סוגי הנשקים, טבלה עם ממוצעים וסטיות תקן ומספר תצפיות הגדול מעשרים, ולבסוף יצרנו את הגרף
only_isis_2 <- only_isis %>% 
  filter(statistics...killed_high<100)
weapon_type_isis_2 <- only_isis_2 %>% 
  filter(!(target.weapon %in% c("Other PBIED","Other VBIED","Unspecified","Unspecified PBIED","Non-suicide IED", "Mixed")))
weapon_mean_sd_2 <- weapon_type_isis_2 %>% 
  group_by(target.weapon) %>% 
  summarise(total_deaths = sum(statistics...killed_high), mean = mean(statistics...killed_high), sd = sd(statistics...killed_high), n=n())
weapon_mean_sd_filtered_2 <- weapon_mean_sd_2 %>% 
  filter(n>20) %>% 
  mutate(results = mean/sd)
ggplot(weapon_mean_sd_filtered_2, aes(x = sd, y = mean)) +
  geom_point(aes(color = target.weapon), size = 5) +
  labs(x = "Sd", y = "Mean", title) +
  theme_minimal()






#דיאגרמות עוגה
#בגרפים הבאים השתמשנו בחבילה שמחוץ לחומר הקורס
#מקור: youtube - "The Data Digest"

target_types <- df_copy %>% 
  group_by(groups, target.type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  filter(groups %in% "Islamic State")
values_islamic <- c(target_types$n[1:3]) #לא בטוח שהכרחי אבל לא הצלחנו דרך אחרת
categories <- c("                  Civillian", "Political", "\nSecurity") #שמנו רווחים לפני האזרחי והורדנו שורה את ביטחוני כדי שלא יגלוש לתוך הדיאגרמה עצמה
rounded <- round(values_islamic/sum(values_islamic)*100, 1)
new2 <- paste0(categories, ": ", rounded, "%")
colors2 <- c("#4863A0", "#728FCE", "#2B3856")
chart2 <- pie3D(x=rounded, labels = new2, main = "Islamic State Target Types", col = colors2, explode = 0.1)



#דיאגרמת עוגה 2
#package name = Webr
#מקורות שבהם נעזרנו: youtube - "The Data Digest"
#להבדיל מגרף המפה שיגיע בהמשך, לא נדרשים פה הסברים מפורטים
only_isis <- df_copy %>% 
  group_by(groups, target.type,claim) %>% 
  filter(groups %in% "Islamic State") %>% 
  rename(Target = target.type)
PieDonut(data = only_isis, aes(Target, claim), title = "Islamic State", r0=0.1,r1 =0.9)






#גרף מפות
#בגרף זה השתמשנו בחבילות שלא למדנו בכלל בשיעור, לכן נסביר כמעט כל שורה
#מקורות שבהם נעזרנו: Youtube - Dr. Lyndon Walker, Prof. Paul Christiansen, GitHub, Plot.ly/ggplot2/
#Maps:
#חבילה הכוללת מפות שונות של מדינות/איזורים המחלקת את המדינות לפי קואורינטות. מאגר הנתונים שבחרנו כולל קואורדינטות ולכן היה נוח להשתמש בחבילה זו

world_map <- map_data("world") #מפת העולם
#מכיוון שיש אי התאמות בין שמות המדינות ב2 מאגרי המידע שלנו ביצענו התאמה בין השמות
world_map <- world_map %>%
  mutate(region = recode(region,"USA" = "United States","UK" = "United Kingdom"))
#שינוי שם העמודה בטבלת המדינות כדי שיתאים למאגר השני שלנו
atks_per_country <- df_copy %>% 
  group_by(target.country) %>% 
  summarise(atks_per_country = n()) %>% 
  rename(region = target.country)

#מכיוון שבמאגר המידע שלנו יש רק מדינות שבהן בוצע לפחות פיגוע אחד, נוסיף לטבלה את המדינות שבהן לא התבצע פיגוע עם הערך 0, זאת כדי לקבל את מפת העולם השלמה
all_countrys <-data.frame(region = unique(world_map$region))
full_atks <- all_countrys %>% 
  left_join(atks_per_country, by = "region") %>% 
  mutate(atks_per_country = ifelse(is.na(atks_per_country),0,atks_per_country))

#חיבור וארגון שתי הטבלאות
atks_per_country_merged <- merge(world_map,full_atks, sort = FALSE, by = "region") 
atks_per_country_merged <- atks_per_country_merged[order(atks_per_country_merged$order),]

#יצירת המפה עצמה באמצעות החבילות-המפה הכללית לא נכנסה לעבודה הסופית
ggplot(atks_per_country_merged, aes(long,lat)) +
  geom_polygon(aes(group = group, fill = atks_per_country)) +
  coord_map(xlim=c(-180,180)) +
  scale_fill_gradient(low = "#ffd5d5", high = "#8b0000",name = "כמות פיגועים") +
  labs(title = "כמות פיגועים בעולם") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff" , color = "#dc0000" ))




#מפה ספציפית לארגון דאעש, אחת בזום ואחת מלאה
atks_by_isis <- df_copy %>%
  filter(groups == "Islamic State") %>%
  group_by(target.country) %>%
  summarise(atks_by_isis = n()) %>%
  rename(region = target.country)
all_countrys <- data.frame(region = unique(world_map$region))
full_atks <- all_countrys %>%
  left_join(atks_by_isis, by = "region") %>%
  mutate(atks_by_isis = ifelse(is.na(atks_by_isis), 0, atks_by_isis))
atks_by_isis_merged <- merge(world_map, full_atks, sort = FALSE, by = "region")
atks_by_isis_merged <- atks_by_isis_merged[order(atks_by_isis_merged$order),]

#בזום
ggplot(atks_by_isis_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_isis)) +
  geom_path(aes(group = group), color = "black", size = 0.2) +
  coord_cartesian(xlim = c(25, 60), ylim = c(20, 40), expand = FALSE) +
  scale_fill_gradient(low = "#ffd5d5", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Islamic State") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))
#מפה מלאה
ggplot(atks_by_isis_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_isis)) +
  coord_map(xlim=c(-180,180)) +
  scale_fill_gradient(low = "#ffd5d5", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Islamic State") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))


#מפה ספציפית לארגון טאליבן, אחת בזום ואחת מלאה
atks_by_taliban <- df_copy %>%
  filter(groups == "Taliban") %>%
  group_by(target.country) %>%
  summarise(atks_by_taliban = n()) %>%
  rename(region = target.country)
all_countrys <- data.frame(region = unique(world_map$region))
full_atks <- all_countrys %>%
  left_join(atks_by_taliban, by = "region") %>%
  mutate(atks_by_taliban = ifelse(is.na(atks_by_taliban), 0, atks_by_taliban))
atks_by_taliban_merged <- merge(world_map, full_atks, sort = FALSE, by = "region")
atks_by_taliban_merged <- atks_by_taliban_merged[order(atks_by_taliban_merged$order),]

#בזום
ggplot(atks_by_taliban_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_taliban)) +
  geom_path(aes(group = group), color = "black", size = 0.2) +
  coord_cartesian(xlim = c(25, 60), ylim = c(20, 40), expand = FALSE) +
  scale_fill_gradient(low = "#ffc9c9", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Taliban") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))

#מפה מלאה
ggplot(atks_by_taliban_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_taliban)) +
  coord_map(xlim=c(-180,180))+
  scale_fill_gradient(low = "#ffc9c9", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Taliban") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))



#מפה ספציפית לארגון אל-קאעידה, אחת בזום ואחת מלאה
atks_by_alqaeda <- df_copy %>%
  filter(groups == "Al-Qaeda") %>%
  group_by(target.country) %>%
  summarise(atks_by_alqaeda = n()) %>%
  rename(region = target.country)
all_countrys <- data.frame(region = unique(world_map$region))
full_atks <- all_countrys %>%
  left_join(atks_by_alqaeda, by = "region") %>%
  mutate(atks_by_alqaeda = ifelse(is.na(atks_by_alqaeda), 0, atks_by_alqaeda))
atks_by_alqaeda_merged <- merge(world_map, full_atks, sort = FALSE, by = "region")
atks_by_alqaeda_merged <- atks_by_alqaeda_merged[order(atks_by_alqaeda_merged$order),]

#בזום
ggplot(atks_by_alqaeda_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_alqaeda)) +
  geom_path(aes(group = group), color = "black", size = 0.2) +
  coord_cartesian(xlim = c(25, 60), ylim = c(20, 40), expand = FALSE) +
  scale_fill_gradient(low = "#ffc9c9", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Al-Qaeda") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))
#מפה מלאה
ggplot(atks_by_alqaeda_merged, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = atks_by_alqaeda)) +
  coord_map(xlim=c(-180,180)) +
  scale_fill_gradient(low = "#ffc9c9", high = "#8b0000", name = "כמות פיגועים") +
  labs(title = "Attacks by Al-Qaeda") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "#c0e1ff", color = "#dc0000"))




#גרף חודשים
#מקורות: Plot.ly/ggplot2/

#נציג את הגרף ב-2 דרכים: בגרף אחד ובפיצול גרפים
#פיצול גרפים
months <- only_isis %>%
  group_by(date.year,date.month) %>% 
  summarise(count = n())
ggplot(data = months, aes(x = date.month, y = count, color = as.factor(date.year), group = date.year)) +
  geom_line(size = 1.2) +
  facet_wrap(~ date.year) +
  geom_point(size = 2) +
  labs(x = "Month", y = "Number Of Attacks", title = "Monthly Attacks Per Year", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),panel.grid.major = element_line(color = "grey")) +
  scale_x_discrete(limits = month.abb) #מחליף את החודשים ממספר לשם החודש


#גרף אחד 
ggplot(data = months, aes(x = date.month, y = count, color = as.factor(date.year), group = date.year)) +
  geom_line(size = 1.1) +
  geom_point(size = 1.5) +
  labs(x = "Month", y = "Number Of Attacks", title = "Monthly Attacks Per Year", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),panel.grid.major = element_line(color = "grey")) +
  scale_x_discrete(limits = month.abb)





#מפת חום
#מקור: Plot.ly/ggplot2/

ggplot(months, aes(x = date.month, y = date.year)) +
  geom_tile(aes(fill = count)) +  # Use geom_tile() for the heatmap
  labs(title = "Heat Map By Month", x = "Month", y = "Year") +
  scale_fill_gradient(low = "#c0e1ff", high = "#ff0000")+
  theme_minimal() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(limits = month.abb)



#גרף 5
#הכנה לביצוע בוטסטראפ:
#פיצול לפי גברים ונשים:
male_only <- only_isis %>% 
  group_by(attacker.gender, statistics...killed_high) %>% 
  filter(attacker.gender %in% "Male") %>% 
  select(attacker.gender,statistics...killed_high)
female_only <- only_isis %>% 
  group_by(attacker.gender, statistics...killed_high) %>% 
  filter(attacker.gender %in% "Female") %>% 
  select(attacker.gender,statistics...killed_high)

#בוטסטראפ גברים:
MALE <- c(male_only$statistics...killed_high)
hist(MALE)
mean(MALE)
N1 <- length(MALE)
B1 <- 20000
means_male <- vector("numeric", length = B1)
for (i in seq_len(B1)) {
  sample_male <- sample(MALE, size = N1, replace = TRUE)
  means_male[i] <- mean(sample_male)
}



#בוטסטראפ נשים:
FEMALE <- c(female_only$statistics...killed_high)
hist(FEMALE)
mean(FEMALE)
N2 <- length(FEMALE)
B2 <- 20000
means_female <- vector("numeric", length = B2)
for (i in seq_len(B1)) {
  sample_female <- sample(MALE, size = N1, replace = TRUE)
  means_female[i] <- mean(sample_female)
}


#המרה לדאטה פריים ואיחוד כדי שנוכל להציג בגרף:
male_df <- data.frame(value = means_male, gender = "Male")
female_df <- data.frame(value = means_female, gender = "Female")
male_and_female <- rbind(male_df,female_df)
ggplot(male_and_female, aes(x = value, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Of Means",
       x = "Average Death Count",
       y = "Density") +
  theme_minimal()


#בדיקת רווח בר סמך לגברים ונשים
hist(means_male, xlim = c(0,100))
hist(means_male)
quantile(means_male, probs = c(0.05,0.95))

hist(means_female, xlim = c(0,100))
hist(means_female)
quantile(means_female, probs = c(0.05,0.95))



