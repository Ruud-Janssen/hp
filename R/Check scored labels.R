library(data.table)


getColumnTypes <- function(x) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    print(cat("\"", class(x[, i]), "\", # ", nm[i], sep = ""))
  }
}

columnsFull <- c(
  "integer", # MSSubClass-20NULL
  "integer", # MSSubClass-30NULL
  "integer", # MSSubClass-40NULL
  "integer", # MSSubClass-45NULL
  "integer", # MSSubClass-50NULL
  "integer", # MSSubClass-60NULL
  "integer", # MSSubClass-70NULL
  "integer", # MSSubClass-75NULL
  "integer", # MSSubClass-80NULL
  "integer", # MSSubClass-85NULL
  "integer", # MSSubClass-90NULL
  "integer", # MSSubClass-120NULL
  "integer", # MSSubClass-150NULL
  "integer", # MSSubClass-160NULL
  "integer", # MSSubClass-180NULL
  "integer", # MSSubClass-190NULL
  "integer", # MSZoning-C (all)NULL
  "integer", # MSZoning-FVNULL
  "integer", # MSZoning-RHNULL
  "integer", # MSZoning-RLNULL
  "integer", # MSZoning-RMNULL
  "numeric", # LotFrontageNULL
  "numeric", # LotAreaNULL
  "integer", # Street-GrvlNULL
  "integer", # Street-PaveNULL
  "integer", # LotConfig-CornerNULL
  "integer", # LotConfig-CulDSacNULL
  "integer", # LotConfig-FR2NULL
  "integer", # LotConfig-FR3NULL
  "integer", # LotConfig-InsideNULL
  "integer", # LandSlope-GtlNULL
  "integer", # LandSlope-ModNULL
  "integer", # LandSlope-SevNULL
  "integer", # Neighborhood-BlmngtnNULL
  "integer", # Neighborhood-BluesteNULL
  "integer", # Neighborhood-BrDaleNULL
  "integer", # Neighborhood-BrkSideNULL
  "integer", # Neighborhood-ClearCrNULL
  "integer", # Neighborhood-CollgCrNULL
  "integer", # Neighborhood-CrawforNULL
  "integer", # Neighborhood-EdwardsNULL
  "integer", # Neighborhood-GilbertNULL
  "integer", # Neighborhood-IDOTRRNULL
  "integer", # Neighborhood-MeadowVNULL
  "integer", # Neighborhood-MitchelNULL
  "integer", # Neighborhood-NAmesNULL
  "integer", # Neighborhood-NoRidgeNULL
  "integer", # Neighborhood-NPkVillNULL
  "integer", # Neighborhood-NridgHtNULL
  "integer", # Neighborhood-NWAmesNULL
  "integer", # Neighborhood-OldTownNULL
  "integer", # Neighborhood-SawyerNULL
  "integer", # Neighborhood-SawyerWNULL
  "integer", # Neighborhood-SomerstNULL
  "integer", # Neighborhood-StoneBrNULL
  "integer", # Neighborhood-SWISUNULL
  "integer", # Neighborhood-TimberNULL
  "integer", # Neighborhood-VeenkerNULL
  "integer", # BldgType-1FamNULL
  "integer", # BldgType-2fmConNULL
  "integer", # BldgType-DuplexNULL
  "integer", # BldgType-TwnhsNULL
  "integer", # BldgType-TwnhsENULL
  "integer", # HouseStyle-1.5FinNULL
  "integer", # HouseStyle-1.5UnfNULL
  "integer", # HouseStyle-1StoryNULL
  "integer", # HouseStyle-2.5FinNULL
  "integer", # HouseStyle-2.5UnfNULL
  "integer", # HouseStyle-2StoryNULL
  "integer", # HouseStyle-SFoyerNULL
  "integer", # HouseStyle-SLvlNULL
  "integer", # OverallQualNULL
  "integer", # OverallCondNULL
  "integer", # YearBuiltNULL
  "integer", # YearRemodAddNULL
  "integer", # RoofStyle-FlatNULL
  "integer", # RoofStyle-GableNULL
  "integer", # RoofStyle-GambrelNULL
  "integer", # RoofStyle-HipNULL
  "integer", # RoofStyle-MansardNULL
  "integer", # RoofStyle-ShedNULL
  "integer", # RoofMatl-ClyTileNULL
  "integer", # RoofMatl-CompShgNULL
  "integer", # RoofMatl-MembranNULL
  "integer", # RoofMatl-MetalNULL
  "integer", # RoofMatl-RollNULL
  "integer", # RoofMatl-Tar&GrvNULL
  "integer", # RoofMatl-WdShakeNULL
  "integer", # RoofMatl-WdShnglNULL
  "integer", # MasVnrType-BrkCmnNULL
  "integer", # MasVnrType-BrkFaceNULL
  "integer", # MasVnrType-StoneNULL
  "numeric", # MasVnrAreaNULL
  "integer", # Foundation-BrkTilNULL
  "integer", # Foundation-CBlockNULL
  "integer", # Foundation-PConcNULL
  "integer", # Foundation-SlabNULL
  "integer", # Foundation-StoneNULL
  "integer", # Foundation-WoodNULL
  "numeric", # BsmtFinSF1NULL
  "numeric", # BsmtFinSF2NULL
  "numeric", # BsmtUnfSFNULL
  "numeric", # TotalBsmtSFNULL
  "integer", # Heating-FloorNULL
  "integer", # Heating-GasANULL
  "integer", # Heating-GasWNULL
  "integer", # Heating-GravNULL
  "integer", # Heating-OthWNULL
  "integer", # Heating-WallNULL
  "integer", # Electrical-1NULL
  "integer", # Electrical-FuseANULL
  "integer", # Electrical-FuseFNULL
  "integer", # Electrical-FusePNULL
  "integer", # Electrical-MixNULL
  "integer", # Electrical-SBrkrNULL
  "numeric", # 1stFlrSFNULL
  "numeric", # 2ndFlrSFNULL
  "numeric", # LowQualFinSFNULL
  "numeric", # GrLivAreaNULL
  "integer", # BsmtFullBath-0NULL
  "integer", # BsmtFullBath-1NULL
  "integer", # BsmtFullBath-2NULL
  "integer", # BsmtFullBath-3NULL
  "integer", # BsmtHalfBath-0NULL
  "integer", # BsmtHalfBath-1NULL
  "integer", # BsmtHalfBath-2NULL
  "numeric", # FullBathNULL
  "numeric", # HalfBathNULL
  "numeric", # BedroomAbvGrNULL
  "numeric", # KitchenAbvGrNULL
  "numeric", # TotRmsAbvGrdNULL
  "numeric", # FireplacesNULL
  "integer", # GarageType-2TypesNULL
  "integer", # GarageType-AttchdNULL
  "integer", # GarageType-BasmentNULL
  "integer", # GarageType-BuiltInNULL
  "integer", # GarageType-CarPortNULL
  "integer", # GarageType-DetchdNULL
  "integer", # GarageYrBltNULL
  "integer", # GarageCars-0NULL
  "integer", # GarageCars-1NULL
  "integer", # GarageCars-2NULL
  "integer", # GarageCars-3NULL
  "integer", # GarageCars-4NULL
  "integer", # GarageCars-5NULL
  "numeric", # GarageAreaNULL
  "numeric", # WoodDeckSFNULL
  "numeric", # OpenPorchSFNULL
  "numeric", # EnclosedPorchNULL
  "numeric", # 3SsnPorchNULL
  "numeric", # ScreenPorchNULL
  "numeric", # PoolAreaNULL
  "integer", # Fence-GdPrvNULL
  "integer", # Fence-GdWoNULL
  "integer", # Fence-MnPrvNULL
  "integer", # Fence-MnWwNULL
  "integer", # MiscFeature-Gar2NULL
  "integer", # MiscFeature-OthrNULL
  "integer", # MiscFeature-ShedNULL
  "integer", # MiscFeature-TenCNULL
  "integer", # SaleType-CODNULL
  "integer", # SaleType-ConNULL
  "integer", # SaleType-ConLDNULL
  "integer", # SaleType-ConLINULL
  "integer", # SaleType-ConLwNULL
  "integer", # SaleType-CWDNULL
  "integer", # SaleType-NewNULL
  "integer", # SaleType-NONENULL
  "integer", # SaleType-OthNULL
  "integer", # SaleType-WDNULL
  "integer", # SaleCondition-AbnormlNULL
  "integer", # SaleCondition-AdjLandNULL
  "integer", # SaleCondition-AllocaNULL
  "integer", # SaleCondition-FamilyNULL
  "integer", # SaleCondition-NormalNULL
  "integer", # SaleCondition-PartialNULL
  "integer", # HasLotFrontageNULL
  "integer", # HasCentralAirNULL
  "integer", # HasAlleyNULL
  "integer", # HasLandContourNULL
  "integer", # ElectricalOkNULL
  "integer", # HasMasVnrNULL
  "integer", # HasGarageYrNULL
  "integer", # HasBasementNULL
  "integer", # ExterCond.nNULL
  "integer", # ExterQual.nNULL
  "integer", # HeatingQC.nNULL
  "integer", # KitchenQual.nNULL
  "integer", # BsmtCond.nNULL
  "integer", # FireplaceQu.nNULL
  "integer", # GarageQual.nNULL
  "integer", # GarageCond.nNULL
  "integer", # Functional.nNULL
  "integer", # BsmtFinType1.nNULL
  "integer", # BsmtFinType2.nNULL
  "integer", # PavedDrive.nNULL
  "integer", # Utilities.nNULL
  "integer", # LotShape.nNULL
  "integer", # BsmtExposure.nNULL
  "integer", # PoolQC.nNULL
  "integer", # GarageFinish.nNULL
  "numeric", # BsmtHeightNULL
  "integer", # Condition_FeedrNULL
  "integer", # Condition_PosNNULL
  "integer", # Condition_ArteryNULL
  "integer", # Condition_RRAeNULL
  "integer", # Condition_RRNnNULL
  "integer", # Condition_RRAnNULL
  "integer", # Condition_PosANULL
  "integer", # Condition_RRNeNULL
  "integer", # Exterior_VinylSdNULL
  "integer", # Exterior_MetalSdNULL
  "integer", # Exterior_Wd.SdngNULL
  "integer", # Exterior_HdBoardNULL
  "integer", # Exterior_BrkFaceNULL
  "integer", # Exterior_WdShingNULL
  "integer", # Exterior_CemntBdNULL
  "integer", # Exterior_PlywoodNULL
  "integer", # Exterior_AsbShngNULL
  "integer", # Exterior_StuccoNULL
  "integer", # Exterior_BrkCommNULL
  "integer", # Exterior_AsphShnNULL
  "integer", # Exterior_StoneNULL
  "integer", # Exterior_ImStuccNULL
  "integer", # Exterior_CBlockNULL
  "integer", # Exterior_Wd.ShngNULL
  "integer", # Exterior_CmentBdNULL
  "integer", # Exterior_Brk.CmnNULL
  "integer", # Exterior_OtherNULL
  "numeric", # SalePriceLogNULL
  "numeric", # MiscValLogNULL
  "integer", # YrMoSoldNULL
  "integer", # newer_dwellingNULL
  "integer", # SaleCondition_PriceDownNULL
  "logical", # HouseDirectModifiedAfterBuiltNULL
  "logical", # HouseDirectSoldAfterBuiltNULL
  "logical", # HouseIsModifiedNULL
  "logical", # GarageDirectBuiltNULL
  "integer", # YrMoSoldCountNULL
  "numeric" # Scored LabelsNULL
)

df_scoredFull <- fread("C:\\Users\\Ruud\\Documents\\Werk\\DataScience\\hp\\output\\House Prices Kaggle_train_scored.csv", data.table = FALSE, header = TRUE, sep = ",")
df_scoredFull <- fread("C:\\Users\\Ruud\\Documents\\Werk\\DataScience\\hp\\output\\House Prices Kaggle_train_scored.csv", data.table = FALSE, header = TRUE, sep = ",", colClasses = columnsFull)
#getColumnTypes(df_scoredFull)

df_scored$resid <- df_scored$`Scored Labels` - df_scored$SalePriceLog
df_scoredFull$resid <- df_scoredFull$`Scored Labels` - df_scoredFull$SalePriceLog

df_scored$SalePrice <- round(exp(df_scored$SalePriceLog))
df_scoredFull$SalePrice <- round(exp(df_scoredFull$SalePriceLog))

df_scored$ScoredSalePrice <- round(exp(df_scored$`Scored Labels`))
df_scoredFull$ScoredSalePrice <- round(exp(df_scoredFull$`Scored Labels`))

df_scored$resid2 <- df_scored$ScoredSalePrice - df_scored$SalePrice
df_scoredFull$resid2 <- df_scoredFull$ScoredSalePrice - df_scoredFull$SalePrice



plotResid <- function(x) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    if (class(x[, i]) != "character") {
  
      gg <- ggplot(x, aes_string(x = nm[i], y = "resid")) + 
        geom_point() +
        geom_smooth(method="loess", se=F)
      plot(gg)
    }
  }
}

plotResid2 <- function(x) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    if (class(x[, i]) != "character") {
      
      gg <- ggplot(x, aes_string(x = nm[i], y = "resid2")) + 
        geom_point() +
        geom_smooth(method="loess", se=F)
      plot(gg)
    }
  }
}


plotResid(df_scoredFull) ## execute function
plotResid2(df_scoredFull) ## execute function
