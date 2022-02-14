## Create tables for categorical variables
```{r}

wb <- createWorkbook()
#SEX      BREASTFEED MOMEDU   WEALTHIND RESIDENCE LIVCHN
addWorksheet(wb, "Categorical var tables")
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$SEX, dem_health_surv$BREASTFEED)),
          colNames = TRUE,
          xy = c(1,1)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$SEX, dem_health_surv$MOMEDU)),
          colNames = TRUE,
          xy = c(1,5)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$SEX, dem_health_surv$WEALTHIND)),
          colNames = TRUE,
          xy = c(1,9)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$SEX, dem_health_surv$RESIDENCE)),
          colNames = TRUE,
          xy = c(1,13)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$SEX, dem_health_surv$LIVCHN)),
          colNames = TRUE,
          xy = c(1,17)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$BREASTFEED, dem_health_surv$MOMEDU)),
          colNames = TRUE,
          xy = c(1,21)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$BREASTFEED, dem_health_surv$WEALTHIND)),
          colNames = TRUE,
          xy = c(1,27)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$BREASTFEED, dem_health_surv$RESIDENCE)),
          colNames = TRUE,
          xy = c(1,33)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$BREASTFEED, dem_health_surv$LIVCHN)),
          colNames = TRUE,
          xy = c(1,39)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$MOMEDU, dem_health_surv$WEALTHIND)),
          colNames = TRUE,
          xy = c(1,45)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$MOMEDU, dem_health_surv$RESIDENCE)),
          colNames = TRUE,
          xy = c(1,50)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$MOMEDU, dem_health_surv$LIVCHN)),
          colNames = TRUE,
          xy = c(1,55)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$WEALTHIND, dem_health_surv$RESIDENCE)),
          colNames = TRUE,
          xy = c(1,60)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$WEALTHIND, dem_health_surv$LIVCHN)),
          colNames = TRUE,
          xy = c(1,67)
)
writeData(wb,
          "Categorical var tables",
          as.matrix(table(dem_health_surv$RESIDENCE, dem_health_surv$LIVCHN)),
          colNames = TRUE,
          xy = c(1,74)
)

saveWorkbook(wb, "Descriptive statistics.xlsx", overwrite = TRUE)

```