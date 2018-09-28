#' 计算个人信息
#'
#' @param x 数据
#' @param data 税率表信息
#' @param version  版本信息，默认为201810版
#'
#' @return  返回值
#' @export
#'
#' @examples getTaxInfo(x)$taxAmount;
getTaxInfo <- function(x=3000,data=NULL,version="201810") {
  table <-as(taxRateTable(data = data,version = version),"data.frame");
  tmp <-table[table$start <=x & table$end >x ,];
  level <- tmp$level;
  taxRate <-tmp$taxRate;
  deductAmt <- tmp$deductAmt;
  taxAmt <- x * taxRate /100 -deductAmt;
  res <- list(level=level,taxRate=taxRate,deductAmt=deductAmt,taxAmt=taxAmt);
  return(res);
}

#' 获取税额
#'
#' @param x  数据
#' @param data  税率表数据
#' @param version 版本号
#'
#' @return 返回值
#' @export
#'
#' @examples getTaxAmt(4000);
getTaxAmt <- function(x=3000,data=NULL,version="201810") {
  getTaxInfo(x,data = data,version = version)$taxAmt;
}


#' 获取个税等级信息
#'
#' @param x 应税所得
#' @param data 税率表数据
#' @param version 版本号
#'
#' @return 返回值
#' @export
#'
#' @examples getTaxLevel(3000);
getTaxLevel <- function(x=3000,data=NULL,version="201810") {
  getTaxInfo(x,data = data,version = version)$level;

}

#' 获取税率
#'
#' @param x 数据
#' @param data  税率表
#' @param version  默认版本201810
#'
#' @return 返回值
#' @export
#'
#' @examples getTaxRate(3000);
getTaxRate <- function(x=3000,data=NULL,version="201810") {
  getTaxInfo(x,data = data,version = version)$taxRate;
}

#' 获取速算扣除数
#'
#' @param x 数据
#' @param data  税率表
#' @param version  默认版本201810
#'
#' @return 返回值
#' @export
#'
#' @examples getTaxDeductAmt(3000);
getTaxDeductAmt <- function(x=3000,data=NULL,version="201810"){
  getTaxInfo(x,data = data,version = version)$deductAmt;
}


#' 获取个税结果信息
#'
#' @param preTaxAmt 税前收入
#' @param threshold  起征点
#'
#' @return 返回值
#' @export
#'
#' @examples getTaxOutput(300:400*10);
getTaxOutput <- function(preTaxAmt,threshold=5000) {
  taxableIncome <- preTaxAmt-threshold;

  # tax amount
  taxAmt <- lapply(taxableIncome,function(x){
    getTaxAmt(x)
  })
  taxAmt <- unlist(taxAmt);
  # tax level
  level <- lapply(taxableIncome,function(x){
    getTaxLevel(x)
  })
  level <- unlist(level);

  #tax rate
  taxRate <- lapply(taxableIncome,function(x){
    getTaxRate(x);
  })
  taxRate <- unlist(taxRate);

  # tax deduct amt
  deductAmt <- lapply(taxableIncome,function(x){
    getTaxDeductAmt(x);
  })
  deductAmt <- unlist(deductAmt);


  res <- data.frame(preTaxAmt,threshold,taxableIncome,taxAmt,afterTaxAmt=preTaxAmt-taxAmt,taxRate,level,deductAmt,stringsAsFactors = FALSE)
  names(res) <-c("税前收入","起征点","应税所得","个税","税后收入","适用税率%","级数","速算扣除数")
  return(res);

}

