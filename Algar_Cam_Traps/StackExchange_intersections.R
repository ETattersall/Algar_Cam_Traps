########################################
# StackExchange_intersections.R
# Code for Stack Exchange Question
# How do I count the number of intersections within one SpatialLines object?
# Asked Apr. 2, 2018
########################################

## Creating SpatialLines object
#Individual lines
x1 <- c(1, 7)
y1 <- c(1, 7)


x2 <- c(2, 2)
y2 <- c(1, 9)


x3 <- c(-1, 10)
y3 <- c(4, 4)

x4 <- c(1, 2, 6)
y4 <- c(3, 4, 8)

# Convert coordinates to Lines class
Lines1 <- Lines(Line(cbind(x1,y1)), ID = "1")
Lines2 <- Lines(Line(cbind(x2, y2)), ID = "2")
Lines3 <- Lines(Line(cbind(x3, y3)), ID = "3")
Lines4 <- Lines(Line(cbind(x4, y4)), ID = "4")

# Convert to SpatialLines
Lines <- SpatialLines(list(Lines1, Lines2, Lines3, Lines4), proj4string = CRS(as.character(NA)))
plot(Lines)


# Creating dataframe to hold attributes
df <- as.data.frame( c("diagonal", "vertical", "horizontal", "diagonal"))
colnames(df) <- "Position"

# Merge to create SpatialLinesDataFrame
SpLinesdf <- SpatialLinesDataFrame(Lines, df)

# Logical function to determine if lines have overlapping points
intersects <- as.data.frame(gIntersects(Lines, byid = T))

#Convert logical into numeric
intersects <- intersects + 0

## Find cells that are 1's
Inter1 <- as.data.frame(which(intersects !=0, arr.ind = T)) #arr.ind = T --> each row gives row and column location of the non-zero value

## Remove if row value = column value (duplicate, line matched with itself)
Inter2 <- Inter1[which(!Inter1$row == Inter1$col),]


