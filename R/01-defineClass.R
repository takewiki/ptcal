#' 计算税率表
#'
#' @slot FLevel Integer. 等级
#' @slot start numeric.  开始金额
#' @slot end numeric.  结束金额
#' @slot taxRate integer.  税率
#' @slot deductAmt numeric. 扣除数
#'
#' @return 返回值
#' @export
#'
#' @examples taxRateTable();
setClass("taxRateTable", slots = c(level = "integer",
                                   start="numeric",
                                   end="numeric",
                                   taxRate="integer",
                                   deductAmt="numeric"),
                        prototype = prototype(
                                  level=0:7,
                                  start=c(-99999,0,3000,12000,25000,35000,55000,80000),
                                  end=c(0,3000,12000,25000,35000,55000,80000,99999999),
                                  taxRate=c(0L,3L,10L,20L,25L,30L,35L,45L),
                                  deductAmt=c(0,0,210,1410,2660,4410,7160,15160)
                        ))
#' 定义相关的函数
#'
#' @param data  原始数据，数据框
#' @param version 定义版本
#'
#' @return 返回值
#' @export
#'
#' @examples taxRateTable(data);
taxRateTable <- function(data,version="201810") {
  res = new("taxRateTable")
  if (version == "201810") {
    res2 <- res
  } else{
    res2 <- initialize(res,
                       level=data$level,
                       start=data$start,
                       end=data$end,
                       taxRate=data$taxRate,
                       deductAmt=data$deductAmt)
  }
  return(res2)
}
