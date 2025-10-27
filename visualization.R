library(tidyverse)
EnrollmentVisualizationUS <-function(display_type, scale_value, color2, color1)
{
  US_totals  <- read.csv("US_COMMUNITY_COLLEGES_ENROLLMENT.csv", check.names=FALSE)
  if (display_type=="line")
  {
      

library(ggplot2)
library(tidyr)

# Reshape data to long format for geom_line
US_totals_long <- US_totals %>%
  pivot_longer(
    cols = c(TOTAL_ENROLLED_MEN, TOTAL_ENROLLED_WOMEN),
    names_to = "Gender",
    values_to = "Enrollments"
  ) %>%
  mutate(Gender = recode(Gender, 
                         "TOTAL_ENROLLED_MEN" = "Men",
                         "TOTAL_ENROLLED_WOMEN" = "Women"))

ggplot(US_totals_long, aes(x = YEAR, y = Enrollments/scale_value, color = Gender)) +
  geom_line() +
  scale_color_manual(values = c("Men" = color1, "Women" = color2)) +
  labs(
    x = "Academic year",
    y = "Enrollments",
    title = "Community colleges enrollment in Iowa",
    subtitle = "2015-2016 to 2023-2024"
  )
  } else if (display_type=="bar")
  {
    library(ggplot2)
library(tidyr)

# Reshape data to long format for geom_bar
US_totals_long <- US_totals %>%
  pivot_longer(
    cols = c(TOTAL_ENROLLED_MEN, TOTAL_ENROLLED_WOMEN),
    names_to = "Gender",
    values_to = "Enrollments"
  ) %>%
  mutate(Gender = recode(Gender, 
                         "TOTAL_ENROLLED_MEN" = "Men",
                         "TOTAL_ENROLLED_WOMEN" = "Women"))

ggplot(US_totals_long, aes(x = YEAR, y = Enrollments/scale_value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Men" = color1, "Women" = color2)) +
  labs(
    x = "Academic year",
    y = "Enrollments",
    title = "Community colleges enrollment in Iowa",
    subtitle = "2015-2016 to 2023-2024"
  )
  }

}