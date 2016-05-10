################################################################
# DS super quick QC
# Created
# Mon Mar  7 16:32:41 2016 ------------------------------
# last Updated
# Tue May 10 11:49:46 2016 ------------------------------
################################################################

# set working directory
setwd("~/Dropbox/SCFBW/scf_data/")


df <- read.table("nh_scf_issues", sep = "|", header =T)
summary(df)
df.ord <- df[order(df$created_at),]
active.users <- summary(as.factor(df$user_id))
hist(active.users, 50, col = "beige", xlab = "Number of posts", ylab = "Number of users", main = "")

uniq.id <- unique(df$user_id)
table(uniq.id)
write.csv(df, "nh_scf_issues.csv")

comments <- read.table("nh_scf_comments", sep = "|", header =T)

df.b <- merge(df, comments, by.x = "id", by.y = "issue_id")
str(df.b)
table(df.b$comment_type)
df.b$user_id.x <- as.factor(df.b$user_id.x)
summary(df.b$user_id.x)
length(unique(df.b$user_id.x))
length(unique(df.b$user_id.y))

summary(df.b)

write.csv(comments, "nh_scf_comments.csv")
write.csv(df.b, "nh_scf_issues_comments.csv")

scf_loc <- df[,1:3]
write.csv(scf_loc, "scf_loc.csv")
